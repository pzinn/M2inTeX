# M2inTeX
Work in progress.
A simple M2 script to parse M2 code inside a LaTeX file, run it and insert its output
For now this is a standalone file, so just load it in M2 with `load "parsetex.m2"`
then run it with e.g.
```
"output.tex" << parseTeX get "input.tex" << close
```
