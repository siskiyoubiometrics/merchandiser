/*
 * nvel_driver.c - CSV-in / CSV-out command-line driver for the NVEL Fortran
 * oracle libraries built by ../build.sh.
 *
 * One source builds two binaries:
 *   -DNVEL_DOUBLE undefined : REAL is float  (link against libnvel_single.so)
 *   -DNVEL_DOUBLE defined   : REAL is double (link against libnvel_double.so)
 *
 * gfortran calling convention (x86-64 Linux, gfortran 13):
 *   - every argument by reference;
 *   - INTEGER  -> int*      (default kind 4, not promoted by either build)
 *   - LOGICAL  -> int*      (kind 4)
 *   - REAL     -> real_t*   (float or double, see above)
 *   - CHARACTER -> char* in position, PLUS one hidden size_t length argument per
 *     CHARACTER argument appended after the last visible argument, in order;
 *   - arrays are column-major: A(i,j) with A(N,M) is a[(i-1) + N*(j-1)];
 *   - module variables are plain globals named __<module>_MOD_<variable>.
 *
 * Usage: nvel_driver_<single|double> [--version] input.csv output.csv
 *
 * Input CSV: header row selects columns by name (any order, missing columns take
 * the defaults listed in col_default()). Column "call" selects the routine:
 *   a = VOLUMELIBRARY, b = CALCDIA and HT2TOPD, c = NVBC, d = VOLEQDEF,
 *   e = NVB_DefaultEq (default NSVB equation; convenience for building inputs).
 * Output CSV: the input columns echoed verbatim, then the result columns.
 *
 * Exit codes: 0 ok, 1 usage/IO error, 2 bad CSV, 3 debug flag set in
 * DEBUG_MOD, 4 trace file appeared in the working directory.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <sys/stat.h>

#ifdef NVEL_DOUBLE
typedef double real_t;
#define RFMT "%.17g"
#define BUILD_NAME "double"
#else
typedef float real_t;
#define RFMT "%.9g"
#define BUILD_NAME "single"
#endif
typedef int f_int;
typedef size_t f_len;

/* ---------------------------------------------------------------- Fortran */
extern void volumelibrary_(f_int *REGN, char *FORST, char *VOLEQ, real_t *MTOPP,
    real_t *MTOPS, real_t *STUMP, real_t *DBHOB, real_t *DRCOB, char *HTTYPE,
    real_t *HTTOT, f_int *HTLOG, real_t *HT1PRD, real_t *HT2PRD, real_t *UPSHT1,
    real_t *UPSHT2, real_t *UPSD1, real_t *UPSD2, f_int *HTREF, real_t *AVGZ1,
    real_t *AVGZ2, f_int *FCLASS, real_t *DBTBH, real_t *BTR, f_int *I3, f_int *I7,
    f_int *I15, f_int *I20, f_int *I21, real_t *VOL, real_t *LOGVOL, real_t *LOGDIA,
    real_t *LOGLEN, real_t *BOLHT, f_int *TLOGS, real_t *NOLOGP, real_t *NOLOGS,
    f_int *CUTFLG, f_int *BFPFLG, f_int *CUPFLG, f_int *CDPFLG, f_int *SPFLG,
    char *CONSPEC, char *PROD, f_int *HTTFLL, char *LIVE, f_int *BA, f_int *SI,
    char *CTYPE, f_int *ERRFLAG, f_int *IDIST,
    f_len lFORST, f_len lVOLEQ, f_len lHTTYPE, f_len lCONSPEC, f_len lPROD,
    f_len lLIVE, f_len lCTYPE);

extern void calcdia_(f_int *REGN, char *FORST, char *VOLEQ, real_t *STUMP,
    real_t *DBHOB, real_t *DRCOB, real_t *HTTOT, real_t *UPSHT1, real_t *UPSHT2,
    real_t *UPSD1, real_t *UPSD2, f_int *HTREF, real_t *AVGZ1, real_t *AVGZ2,
    f_int *FCLASS, real_t *DBTBH, real_t *BTR, real_t *HTUP, real_t *DIB,
    real_t *DOB, f_int *ERRFLAG, f_len lFORST, f_len lVOLEQ);

extern void ht2topd_(f_int *REGN, char *FORST, char *VOLEQ, real_t *DBHOB,
    real_t *HTTOT, real_t *HT1PRD, real_t *HT2PRD, real_t *UPSHT1, real_t *UPSHT2,
    real_t *UPSD1, real_t *UPSD2, real_t *AVGZ1, real_t *AVGZ2, f_int *HTREF,
    real_t *DBTBH, real_t *BTR, f_int *FCLASS, real_t *STEMDIB, real_t *STEMHT,
    f_int *ERRFLAG, f_len lFORST, f_len lVOLEQ);

extern void nvbc_(f_int *REGN, char *FORST, char *DIST, char *VOLEQ, real_t *DBHOB,
    real_t *HTTOT, real_t *MTOPP, real_t *MTOPS, real_t *HT1PRD, real_t *HT2PRD,
    real_t *STUMP, char *PROD, real_t *BRKHT, real_t *BRKHTD, char *LIVE,
    real_t *CR, real_t *CULL, f_int *DECAYCD, real_t *LOGLEN, real_t *LOGDIA,
    real_t *LOGVOL, real_t *BOLHT, f_int *LOGST, real_t *NOLOGP, real_t *NOLOGS,
    real_t *VOL, real_t *DRYBIO, real_t *GRNBIO, f_int *ERRFLG, f_int *FIASPCD,
    char *CTYPE, f_len lFORST, f_len lDIST, f_len lVOLEQ, f_len lPROD, f_len lLIVE,
    f_len lCTYPE);

extern void voleqdef_(char *VAR, f_int *REGN, char *FORST, char *DIST, f_int *SPEC,
    char *PROD, char *VOLEQ, f_int *ERRFLAG,
    f_len lVAR, f_len lFORST, f_len lDIST, f_len lPROD, f_len lVOLEQ);

extern void nvb_defaulteq_(f_int *REGN, char *FORST, char *DIST, f_int *SPCD,
    char *NVBEQ, f_int *ERRFLG, f_len lFORST, f_len lDIST, f_len lNVBEQ);

extern void vernum_(f_int *VERSION);

/* VOLINPUT_MOD (volinput_mod.f at the repo root, the canonical copy) */
extern char  __volinput_mod_MOD_mrulemod;   /* CHARACTER*1, DATA 'N' */
extern char  __volinput_mod_MOD_newcor;     /* CHARACTER*1, DATA 'Y' */
extern f_int __volinput_mod_MOD_formclass;
extern f_int __volinput_mod_MOD_newevod;
extern f_int __volinput_mod_MOD_newopt;
extern real_t __volinput_mod_MOD_newmaxlen, __volinput_mod_MOD_newminlen,
    __volinput_mod_MOD_newminlent, __volinput_mod_MOD_newmerchl,
    __volinput_mod_MOD_newmtopp, __volinput_mod_MOD_newmtops,
    __volinput_mod_MOD_newstump, __volinput_mod_MOD_newtrim,
    __volinput_mod_MOD_newbtr, __volinput_mod_MOD_newdbtbh,
    __volinput_mod_MOD_newminbfd;

/* DEBUG_MOD */
extern f_int __debug_mod_MOD_any_debug;    /* LOGICAL, DATA .FALSE. */
extern f_int __debug_mod_MOD_debug[6];     /* TYPE(DEBUG_INDICATORS): 6 LOGICALs */

/* ------------------------------------------------------------------- CSV */
#define MAXCOLS 256
#define MAXLINE 65536

typedef struct { int n; char *name[MAXCOLS]; } header_t;
typedef struct { int n; char *f[MAXCOLS]; } row_t;

static char *trim(char *s) {
    while (*s && isspace((unsigned char)*s)) s++;
    char *e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) *--e = 0;
    return s;
}

static int split_csv(char *line, char **out, int max) {
    int n = 0; char *p = line;
    while (n < max) {
        char *start = p;
        if (*p == '"') {                       /* minimal quoted-field support */
            start = ++p;
            while (*p && *p != '"') p++;
            if (*p == '"') *p++ = 0;
            while (*p && *p != ',') p++;
        } else {
            while (*p && *p != ',') p++;
        }
        int more = (*p == ',');
        *p = 0;
        out[n++] = trim(start);
        if (!more) break;
        p++;
    }
    return n;
}

static const header_t *g_hdr; static const row_t *g_row;

static const char *col(const char *name, const char *dflt) {
    for (int i = 0; i < g_hdr->n && i < g_row->n; i++)
        if (strcasecmp(g_hdr->name[i], name) == 0 && g_row->f[i][0]) return g_row->f[i];
    return dflt;
}
static f_int  coli(const char *name, f_int dflt) { const char *s = col(name, NULL); return s ? (f_int)strtol(s, NULL, 10) : dflt; }
static real_t colr(const char *name, double dflt) { const char *s = col(name, NULL); return (real_t)(s ? strtod(s, NULL) : dflt); }

/* Fixed-width Fortran CHARACTER buffer: space padded to len, with slack so that
 * callees which declare a longer dummy (VOLINITNVB declares VOLEQ CHARACTER*11
 * while VOLUMELIBRARY passes the caller's 10) never read past our allocation. */
#define SLACK 64
static void fstr(char *buf, const char *s, int len) {
    memset(buf, ' ', len + SLACK);
    int n = (int)strlen(s); if (n > len) n = len;
    memcpy(buf, s, n);
}
static void fstr_out(char *out, const char *buf, int len) {  /* trim trailing blanks */
    int n = len; while (n > 0 && buf[n-1] == ' ') n--;
    memcpy(out, buf, n); out[n] = 0;
}

/* --------------------------------------------------------------- outputs */
typedef struct {
    int have_err, have_vol, have_logs, have_dia, have_ht, have_bio, have_voleqout;
    f_int errflag, tlogs;
    real_t vol[15], nologp, nologs, dib, dob, stemht, drybio[15], grnbio[15];
    real_t loglen[20], bolht[21], logdia[21*3], logvol[7*20];
    char voleq_out[16];
    char mrulemod_after;
} result_t;

static void put_r(FILE *o, int have, real_t v) { if (have) fprintf(o, "," RFMT, (double)v); else fputs(",", o); }
static void put_i(FILE *o, int have, f_int v) { if (have) fprintf(o, ",%d", v); else fputs(",", o); }

static void write_result_header(FILE *o) {
    fputs(",BUILD,ERRFLAG,VOLEQ_OUT,MRULEMOD_AFTER", o);
    for (int i = 1; i <= 15; i++) fprintf(o, ",VOL%d", i);
    fputs(",TLOGS,NOLOGP,NOLOGS,DIB,DOB,STEMHT", o);
    for (int i = 1; i <= 15; i++) fprintf(o, ",DRYBIO%d", i);
    for (int i = 1; i <= 15; i++) fprintf(o, ",GRNBIO%d", i);
    for (int i = 1; i <= 20; i++) fprintf(o, ",LOGLEN%d", i);
    for (int i = 1; i <= 21; i++) fprintf(o, ",BOLHT%d", i);
    for (int j = 1; j <= 3; j++) for (int i = 1; i <= 21; i++) fprintf(o, ",LOGDIA_%d_%d", i, j);
    for (int j = 1; j <= 20; j++) for (int i = 1; i <= 7; i++) fprintf(o, ",LOGVOL_%d_%d", i, j);
    fputs("\n", o);
}

static void write_result(FILE *o, const result_t *r) {
    fprintf(o, ",%s", BUILD_NAME);
    put_i(o, r->have_err, r->errflag);
    fprintf(o, ",%s", r->have_voleqout ? r->voleq_out : "");
    fprintf(o, ",%c", r->mrulemod_after ? r->mrulemod_after : ' ');
    for (int i = 0; i < 15; i++) put_r(o, r->have_vol, r->vol[i]);
    put_i(o, r->have_logs, r->tlogs);
    put_r(o, r->have_logs, r->nologp); put_r(o, r->have_logs, r->nologs);
    put_r(o, r->have_dia, r->dib); put_r(o, r->have_dia, r->dob);
    put_r(o, r->have_ht, r->stemht);
    for (int i = 0; i < 15; i++) put_r(o, r->have_bio, r->drybio[i]);
    for (int i = 0; i < 15; i++) put_r(o, r->have_bio, r->grnbio[i]);
    for (int i = 0; i < 20; i++) put_r(o, r->have_logs, r->loglen[i]);
    for (int i = 0; i < 21; i++) put_r(o, r->have_logs, r->bolht[i]);
    for (int i = 0; i < 63; i++) put_r(o, r->have_logs, r->logdia[i]);
    for (int i = 0; i < 140; i++) put_r(o, r->have_logs, r->logvol[i]);
    fputs("\n", o);
}

/* ------------------------------------------------------- module state */
static int debug_flags_clear(const char *when) {
    int bad = 0;
    if (__debug_mod_MOD_any_debug) { fprintf(stderr, "DEBUG_MOD ANY_DEBUG is true %s\n", when); bad = 1; }
    static const char *nm[6] = {"INPUT","DLL","MODEL","ANALYSIS","EXTERNAL","VOLEQ"};
    for (int i = 0; i < 6; i++) if (__debug_mod_MOD_debug[i]) { fprintf(stderr, "DEBUG_MOD DEBUG%%%s is true %s\n", nm[i], when); bad = 1; }
    return !bad;
}

static const char *trace_files[] = {"Debug.txt", "debug.txt", "DEBUG.TXT", "fort.1", "fort.2", "fort.3", NULL};
static int trace_file_present(void) {
    struct stat st; int found = 0;
    for (int i = 0; trace_files[i]; i++) if (stat(trace_files[i], &st) == 0) { fprintf(stderr, "trace file present: %s\n", trace_files[i]); found = 1; }
    return found;
}

/* Set VOLINPUT_MOD explicitly before every call. Default MRULEMOD='N' and all
 * NEW* zero, i.e. the library's own initial state; MRULEMOD column 'Y' turns on
 * the override and copies the NEW* columns in. MRULES() consumes the override
 * and resets MRULEMOD to 'N' itself. */
static void set_volinput(void) {
    const char *m = col("MRULEMOD", "N");
    __volinput_mod_MOD_mrulemod = (m[0] == 'Y' || m[0] == 'y') ? 'Y' : 'N';
    __volinput_mod_MOD_newcor   = col("NEWCOR", "Y")[0];
    __volinput_mod_MOD_formclass = coli("FORMCLASS", 0);
    __volinput_mod_MOD_newevod  = coli("NEWEVOD", 0);
    __volinput_mod_MOD_newopt   = coli("NEWOPT", 0);
    __volinput_mod_MOD_newmaxlen  = colr("NEWMAXLEN", 0);
    __volinput_mod_MOD_newminlen  = colr("NEWMINLEN", 0);
    __volinput_mod_MOD_newminlent = colr("NEWMINLENT", 0);
    __volinput_mod_MOD_newmerchl  = colr("NEWMERCHL", 0);
    __volinput_mod_MOD_newmtopp   = colr("NEWMTOPP", 0);
    __volinput_mod_MOD_newmtops   = colr("NEWMTOPS", 0);
    __volinput_mod_MOD_newstump   = colr("NEWSTUMP", 0);
    __volinput_mod_MOD_newtrim    = colr("NEWTRIM", 0);
    __volinput_mod_MOD_newbtr     = colr("NEWBTR", 0);
    __volinput_mod_MOD_newdbtbh   = colr("NEWDBTBH", 0);
    __volinput_mod_MOD_newminbfd  = colr("NEWMINBFD", 0);
}

/* ----------------------------------------------------------------- calls */
static void do_call(char callc, result_t *r) {
    char forst[2+SLACK], dist[2+SLACK], voleq[11+SLACK], httype[1+SLACK], prod[2+SLACK],
         conspec[4+SLACK], live[1+SLACK], ctype[1+SLACK], var[2+SLACK];
    fstr(forst,   col("FORST", ""), 2);
    fstr(dist,    col("DIST", ""), 2);
    fstr(voleq,   col("VOLEQ", ""), 11);
    fstr(httype,  col("HTTYPE", "F"), 1);
    fstr(prod,    col("PROD", "01"), 2);
    fstr(conspec, col("CONSPEC", ""), 4);
    fstr(live,    col("LIVE", "L"), 1);
    fstr(ctype,   col("CTYPE", "C"), 1);
    fstr(var,     col("VAR", ""), 2);

    f_int regn = coli("REGN", 0), htlog = coli("HTLOG", 0), htref = coli("HTREF", 0);
    f_int fclass = coli("FCLASS", 0), httfll = coli("HTTFLL", 0), ba = coli("BA", 0), si = coli("SI", 0);
    f_int idist = coli("IDIST", coli("DIST", 0)), spec = coli("SPEC", 0);
    f_int cutflg = coli("CUTFLG", 1), bfpflg = coli("BFPFLG", 1), cupflg = coli("CUPFLG", 1);
    f_int cdpflg = coli("CDPFLG", 1), spflg = coli("SPFLG", 1);
    f_int decaycd = coli("DECAYCD", 0), fiaspcd = coli("FIASPCD", spec);
    real_t mtopp = colr("MTOPP", 0), mtops = colr("MTOPS", 0), stump = colr("STUMP", 0);
    real_t dbhob = colr("DBHOB", 0), drcob = colr("DRCOB", 0), httot = colr("HTTOT", 0);
    real_t ht1prd = colr("HT1PRD", 0), ht2prd = colr("HT2PRD", 0);
    real_t upsht1 = colr("UPSHT1", 0), upsht2 = colr("UPSHT2", 0), upsd1 = colr("UPSD1", 0), upsd2 = colr("UPSD2", 0);
    real_t avgz1 = colr("AVGZ1", 0), avgz2 = colr("AVGZ2", 0), dbtbh = colr("DBTBH", 0), btr = colr("BTR", 0);
    real_t htup = colr("HTUP", 0), stemdib = colr("STEMDIB", 0);
    real_t brkht = colr("BRKHT", 0), brkhtd = colr("BRKHTD", 0), cr = colr("CR", 0), cull = colr("CULL", 0);
    f_int i3 = 3, i7 = 7, i15 = 15, i20 = 20, i21 = 21;

    memset(r, 0, sizeof *r);
    set_volinput();

    switch (callc) {
    case 'a':
        volumelibrary_(&regn, forst, voleq, &mtopp, &mtops, &stump, &dbhob, &drcob, httype,
            &httot, &htlog, &ht1prd, &ht2prd, &upsht1, &upsht2, &upsd1, &upsd2, &htref,
            &avgz1, &avgz2, &fclass, &dbtbh, &btr, &i3, &i7, &i15, &i20, &i21,
            r->vol, r->logvol, r->logdia, r->loglen, r->bolht, &r->tlogs, &r->nologp, &r->nologs,
            &cutflg, &bfpflg, &cupflg, &cdpflg, &spflg, conspec, prod, &httfll, live, &ba, &si,
            ctype, &r->errflag, &idist, 2, 10, 1, 4, 2, 1, 1);
        r->have_err = r->have_vol = r->have_logs = r->have_voleqout = 1;
        fstr_out(r->voleq_out, voleq, 11);
        break;
    case 'b': {
        calcdia_(&regn, forst, voleq, &stump, &dbhob, &drcob, &httot, &upsht1, &upsht2,
            &upsd1, &upsd2, &htref, &avgz1, &avgz2, &fclass, &dbtbh, &btr, &htup,
            &r->dib, &r->dob, &r->errflag, 2, 10);
        f_int err2 = 0;
        ht2topd_(&regn, forst, voleq, &dbhob, &httot, &ht1prd, &ht2prd, &upsht1, &upsht2,
            &upsd1, &upsd2, &avgz1, &avgz2, &htref, &dbtbh, &btr, &fclass, &stemdib,
            &r->stemht, &err2, 2, 10);
        /* both routines return an ERRFLAG; report CALCDIA's in ERRFLAG and fold
         * HT2TOPD's into VOLEQ_OUT as "HT2TOPD_ERR=n" only when nonzero */
        r->have_err = r->have_dia = r->have_ht = 1;
        if (err2) { snprintf(r->voleq_out, sizeof r->voleq_out, "H2T_ERR=%d", err2); r->have_voleqout = 1; }
        break; }
    case 'c':
        nvbc_(&regn, forst, dist, voleq, &dbhob, &httot, &mtopp, &mtops, &ht1prd, &ht2prd,
            &stump, prod, &brkht, &brkhtd, live, &cr, &cull, &decaycd,
            r->loglen, r->logdia, r->logvol, r->bolht, &r->tlogs, &r->nologp, &r->nologs,
            r->vol, r->drybio, r->grnbio, &r->errflag, &fiaspcd, ctype, 2, 2, 11, 2, 1, 1);
        r->have_err = r->have_vol = r->have_logs = r->have_bio = r->have_voleqout = 1;
        fstr_out(r->voleq_out, voleq, 11);
        break;
    case 'd':
        voleqdef_(var, &regn, forst, dist, &spec, prod, voleq, &r->errflag, 2, 2, 2, 2, 10);
        r->have_err = r->have_voleqout = 1;
        fstr_out(r->voleq_out, voleq, 10);
        break;
    case 'e':
        nvb_defaulteq_(&regn, forst, dist, &spec, voleq, &r->errflag, 2, 2, 10);
        r->have_err = r->have_voleqout = 1;
        fstr_out(r->voleq_out, voleq, 10);
        break;
    default:
        fprintf(stderr, "unknown call '%c'\n", callc);
        break;
    }
    r->mrulemod_after = __volinput_mod_MOD_mrulemod;
}

/* ------------------------------------------------------------------ main */
int main(int argc, char **argv) {
    if (argc >= 2 && strcmp(argv[1], "--version") == 0) {
        f_int v = 0; vernum_(&v); printf("%d\n", v); return 0;
    }
    if (argc != 3) { fprintf(stderr, "usage: %s [--version] input.csv output.csv\n", argv[0]); return 1; }

    if (!debug_flags_clear("before any call")) return 3;
    if (trace_file_present()) { fprintf(stderr, "trace file already present before run; remove it first\n"); return 4; }

    FILE *in = fopen(argv[1], "r"); if (!in) { perror(argv[1]); return 1; }
    FILE *out = fopen(argv[2], "w"); if (!out) { perror(argv[2]); return 1; }

    static char line[MAXLINE], hline[MAXLINE];
    header_t hdr; row_t row; memset(&hdr, 0, sizeof hdr);
    if (!fgets(hline, sizeof hline, in)) { fprintf(stderr, "empty input\n"); return 2; }
    hdr.n = split_csv(hline, hdr.name, MAXCOLS);
    int callcol = -1;
    for (int i = 0; i < hdr.n; i++) if (strcasecmp(hdr.name[i], "call") == 0) callcol = i;
    if (callcol < 0) { fprintf(stderr, "input needs a 'call' column\n"); return 2; }

    for (int i = 0; i < hdr.n; i++) fprintf(out, "%s%s", i ? "," : "", hdr.name[i]);
    write_result_header(out);

    g_hdr = &hdr; g_row = &row;
    int nrows = 0;
    while (fgets(line, sizeof line, in)) {
        if (trim(line)[0] == 0) continue;
        char *echo = strdup(line);
        row.n = split_csv(line, row.f, MAXCOLS);
        if (row.n < 1) { free(echo); continue; }
        result_t r;
        do_call(row.f[callcol][0], &r);
        /* echo the input fields exactly, padded with empties to header width */
        for (int i = 0; i < hdr.n; i++) fprintf(out, "%s%s", i ? "," : "", i < row.n ? row.f[i] : "");
        write_result(out, &r);
        free(echo);
        nrows++;
    }
    fclose(in); fclose(out);

    int rc = 0;
    if (!debug_flags_clear("after run")) rc = 3;
    if (trace_file_present()) rc = 4;
    fprintf(stderr, "%s: %d rows -> %s%s\n", BUILD_NAME, nrows, argv[2], rc ? " (DEBUG/TRACE CHECK FAILED)" : "");
    return rc;
}
