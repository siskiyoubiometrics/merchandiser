# Gate 1 port-error oracle probes

The BLM, Behre, Clark, and Region 4 values used by the focused regressions were
verified on 2026-09-06 with the read-only drivers in the main `treevolume`
repository. Both drivers reported NVEL version `20260729`. The reference clone
was pinned to commit `38548071d5aa652bb90c7f111f86b427f798a1c9`.

The double build returned the following successful values:

| Case | Output | Value |
|---|---|---:|
| `B00BEHW011`, DBH 3, HT 15, form class 80 | `VOL1` | 0.31286263448680285 |
| `B04BEHW202`, DBH 12, HT 80, query 40 | `DIB` | 7.4324882318936325 |
| `616BEHW202`, DBH 12, HT 80, query 40 | `DIB` | 7.6008719615020759 |
| `900CLKE001`, DBH 12, HT 80 | `VOL1` | 25.69999999999999929 |
| `811CLKE100`, DBH 12, HT 80 | `VOL1` | 24.10000000000000142 |
| `811CLKO100`, DBH 12, HT 80 | `VOL1` | 30.10000000000000142 |
| `811CLK0100`, DBH 12, HT 80 | `VOL1` | 30.10000000000000142 |
| `811CLKE100`, DBH 12, HT 80 | `VOL14` | 0.869693330966445188 |
| `811CLKE330`, DBH 2, HT 30 | `VOL14` | 0.034911375375197742 |
| `811CLKE370`, DBH 2, HT 130 | `VOL14` | 0.015957106155581297 |
| `400MATW202`, DBH 12, HT 80 | `VOL1` | 24.95955850787330377 |

SHA-256 values:

```text
8f7f9cd7e4fd9f4ef537340d4811deba4ac7cc050e092d410253456e2bae78ed  gate1_port_oracle_input.csv
6a87c7e64af3e15629e47ceae73c0bcc2470ff8c8200d2525ec749f657d776c9  gate1_port_oracle.double.raw.csv
586f7da9c8530cbfc77f7b5dd6b416df0f2c236ef37fa43d387ee44129fbe648  gate1_port_oracle.single.raw.csv
```
