# M2inTeX
Work in progress.
A simple M2 script to parse M2 code inside a LaTeX file, run it and insert its output.

For now this is a standalone file, so just load it in M2 with `load "parsetex.m2"`
then run it with e.g.
```
"output.tex" << parseTeX get "input.tex" << close
```

The M2 code must be enclosed in `\begin{lstlisting}[language=Macaulay2] \end{lstlisting}`, see the ex.tex file.
(This uses the listings LaTeX package)
