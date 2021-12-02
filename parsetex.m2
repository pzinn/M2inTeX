debug Core

codeEnv = "verbatim"; -- the environment used in the TeX file for M2 code

codeRegex = "\\\\begin{"|codeEnv|"}\n*([\\s\\S]*?)\n*\\\\end{"|codeEnv|"}";

verbatim := false; -- not thread-safe

parseTeX = f -> (
    codes := select(codeRegex,"$1",f);
    rest := separate(codeRegex,f); -- seems silly to do the regex twice
    --print(codes,rest);
    saveMode := topLevelMode; -- not thread-safe
    verbatim = false;
    topLevelMode = TeX;
    s := capture codes;
    if s#0 then print "warning: running the code produced an error";
    topLevelMode = saveMode;
    s = separate("%input\n",last s);--print s;
    k := 1;
    concatenate mingle(rest,apply(codes, ss -> (
        l := min(#(lines ss),#s-k);
        first("\\smallskip\n" | concatenate s_{k..<k+l} | "\\smallskip\n",
        k = k+l)
        )))
    )

-* ex of use
"ex-parsed.tex" << parseTeX get "ex.tex" << close
*-

-----------------

ZZ#{TeX,InputPrompt} = lineno -> (
    concatenate(
        "%input\n",
        if not verbatim then (verbatim = true; "\\begin{verbatim}\n"),
        interpreterDepth:"i",
        toString lineno,
        " : "
        ))

ZZ#{TeX,InputContinuationPrompt} = identity

Nothing#{TeX,Print} = identity

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

Thing#{TeX,Print} = x -> (
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid TeX output";
    << "\\noindent\\verb|" | on() | " = |" | y << endl;
    )
Thing#{TeX,BeforePrint} = x -> (
    if verbatim then (
        << "\\end{verbatim}\n";
        verbatim = false;
        );
    x
    )

texAfterPrint :=  x -> (
    if class x === Sequence then x = RowExpression deepSplice { x };
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << "\n\\noindent\\verb|" | on() | " : |" |  y  << endl;
    )

-- all that's below would go if afterprint was expressionified

Thing#{TeX,AfterPrint} = x -> texAfterPrint class x;

Boolean#{TeX,AfterPrint} = identity

Expression#{TeX,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{TeX,AfterPrint} = identity

Ideal#{TeX,AfterPrint} = Ideal#{TeX,AfterNoPrint} = (I) -> texAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{TeX,AfterPrint} = MonomialIdeal#{TeX,AfterNoPrint} = (I) -> texAfterPrint (MonomialIdeal," of ",ring I)

InexactNumber#{TeX,AfterPrint} = x -> texAfterPrint (class x," (of precision ",precision x,")")

Module#{TeX,AfterPrint} = M -> texAfterPrint(
    ring M,"-module",
    if M.?generators then
    if M.?relations then (", subquotient of ",ambient M)
    else (", submodule of ",ambient M)
    else if M.?relations then (", quotient of ",ambient M)
    else if rank ambient M > 0 then
    (", free",
	if not all(degrees M, d -> all(d, zero))
	then (", degrees ",runLengthEncode if degreeLength M === 1 then flatten degrees M else degrees M)
	)
    )

Net#{TeX,AfterPrint} = identity

Nothing#{TeX,AfterPrint} = identity

Matrix#{TeX,AfterPrint} = Matrix#{TeX,AfterNoPrint} =
RingMap#{TeX,AfterPrint} = RingMap#{TeX,AfterNoPrint} = f -> texAfterPrint (class f, " ", new MapExpression from {target f,source f})

-- Sequence#{TeX,AfterPrint} = Sequence#{TeX,AfterNoPrint} = identity

CoherentSheaf#{TeX,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     n := rank ambient F;
     texAfterPrint("coherent sheaf on ",X,
     if M.?generators then
     if M.?relations then (", subquotient of ", ambient F)
     else (", subsheaf of ", ambient F)
     else if M.?relations then (", quotient of ", ambient F)
     else if n > 0 then (
	  ", free"
	  -- if not all(degrees M, d -> all(d, zero))
	  -- then << ", degrees " << if degreeLength M === 1 then flatten degrees M else degrees M;
	  )
     )
 )

ZZ#{TeX,AfterPrint} = identity

