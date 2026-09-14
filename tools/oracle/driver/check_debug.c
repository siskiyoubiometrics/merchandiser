/*
 * check_debug.c - open each oracle library with dlopen and assert, by reading
 * DEBUG_MOD's variables through their gfortran-mangled names, that every debug
 * flag is false in the freshly loaded library (i.e. before any call). Also fails
 * if a trace file (Debug.txt, fort.1..3) is present in the working directory.
 *
 * Usage: check_debug libA.so [libB.so ...]      exit 0 = all clear
 * The driver binaries repeat the same assertion before and after every run.
 */
#include <stdio.h>
#include <string.h>
#include <dlfcn.h>
#include <sys/stat.h>

int main(int argc, char **argv) {
    int rc = 0;
    if (argc < 2) { fprintf(stderr, "usage: %s lib.so ...\n", argv[0]); return 1; }
    for (int a = 1; a < argc; a++) {
        void *h = dlopen(argv[a], RTLD_NOW | RTLD_LOCAL);
        if (!h) { fprintf(stderr, "%s: %s\n", argv[a], dlerror()); rc = 1; continue; }
        int *any = (int *)dlsym(h, "__debug_mod_MOD_any_debug");
        int *dbg = (int *)dlsym(h, "__debug_mod_MOD_debug");
        int *ludbg = (int *)dlsym(h, "__debug_mod_MOD_ludbg");
        char *mrulemod = (char *)dlsym(h, "__volinput_mod_MOD_mrulemod");
        if (!any || !dbg || !ludbg || !mrulemod) { fprintf(stderr, "%s: DEBUG_MOD/VOLINPUT_MOD symbols not found\n", argv[a]); rc = 1; dlclose(h); continue; }
        static const char *nm[6] = {"INPUT","DLL","MODEL","ANALYSIS","EXTERNAL","VOLEQ"};
        printf("%s\n  ANY_DEBUG=%d LUDBG=%d MRULEMOD='%c'", argv[a], *any, *ludbg, *mrulemod);
        for (int i = 0; i < 6; i++) printf(" DEBUG%%%s=%d", nm[i], dbg[i]);
        printf("\n");
        if (*any) { fprintf(stderr, "  FAIL: ANY_DEBUG true\n"); rc = 3; }
        for (int i = 0; i < 6; i++) if (dbg[i]) { fprintf(stderr, "  FAIL: DEBUG%%%s true\n", nm[i]); rc = 3; }
        if (*mrulemod != 'N') { fprintf(stderr, "  FAIL: MRULEMOD initial value is '%c', expected 'N'\n", *mrulemod); rc = 3; }
        dlclose(h);
    }
    static const char *files[] = {"Debug.txt","debug.txt","DEBUG.TXT","fort.1","fort.2","fort.3",NULL};
    struct stat st;
    for (int i = 0; files[i]; i++) if (stat(files[i], &st) == 0) { fprintf(stderr, "FAIL: trace file %s present\n", files[i]); rc = 4; }
    if (rc == 0) puts("debug state clear, no trace files");
    return rc;
}
