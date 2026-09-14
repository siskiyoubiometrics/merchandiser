# Reference build and fixture tooling

`build.sh` builds the independent source-library reference. `generate.py` and
`compare.py` read `MERCHANDISER_FIXTURES`, falling back to `TREEVOLUME_FIXTURES`
when the new name is unset. Set it to the external fixture directory before
using either script. Generation writes to that directory and is a separate
maintainer action from running package tests.

No fixture data is stored in this repository. The fixture scripts and
reference drivers retain their original calculation rules.
