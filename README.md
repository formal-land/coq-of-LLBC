## Translation of LLBC into Coq

### Building and executing

1) To define [charon](https://github.com/AeneasVerif/charon) as a [git submodule](https://git-scm.com/book/en/v2/Git-Tools-Submodules): `git submodule update --init`

2) To build: `dune build`

3) To run with LLBC file `file.LLBC`: `dune exec bin/main.exe file.LLBC`
