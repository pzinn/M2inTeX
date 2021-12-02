debug Core

codeEnv = "verbatim"; -- the environment used in the TeX file for M2 code

codeRegex = "\\\\begin{"|codeEnv|"}\n*([\\s\\S]*?)\n*\\\\end{"|codeEnv|"}";

parseTeX = f -> (
    codes := select(codeRegex,"$1",f);
    rest := separate(codeRegex,f); -- seems silly to do the regex twice
    saveMode := topLevelMode; -- not thread-safe
    topLevelMode = TeX;
    s := capture codes;
    topLevelMode = saveMode;
    s = drop(separate("%input\n",last s),-1);
    k := 1;
    concatenate mingle(rest,apply(codes, ss -> (
        l := #(lines ss); -- is there better?
        first("\\smallskip\n" | concatenate s_{k..<k+l} | "\\smallskip\n",
        k = k+l)
        )))
    )

-* ex of use
"ex-parsed.tex" << parseTeX get "ex.tex" << close
*-

-----------------


ZZ#{TeX,InputPrompt} = lineno -> concatenate(
    "%input\n\\begin{verbatim}\n", -- or whatever
    interpreterDepth:"i",
    toString lineno,
    " : "
    )

ZZ#{TeX,InputContinuationPrompt} = identity

Thing#{TeX,BeforePrint} = identity

Nothing#{TeX,Print} = identity

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

Thing#{TeX,Print} = x -> (
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid TeX output";
    << "\\end{verbatim}\n\\verb|" | on() | " = |" | y << endl;
    )

texAfterPrint :=  x -> (
    if class x === Sequence then x = RowExpression deepSplice { x };
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << "\\\\\n\\verb|" | on() | " : |" |  y  << endl;
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

