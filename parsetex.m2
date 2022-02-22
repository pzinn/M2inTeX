debug Core

codeEnv = "verbatim"; -- the environment used in the TeX file for M2 code


verbatim := false; -- not thread-safe

codeComment := "-* start code *- "; -- added at the start of every code chunk to split correctly
codeComment1 := regexQuote codeComment;

inputComment := "% start M2\n"; -- comment added in TeX to mark start of a M2 code chunk
inputComment1 := regexQuote inputComment;

codeRegex := "(?<!"|regexQuote inputComment|")\\\\begin{"|regexQuote codeEnv|"}\n*([\\s\\S]*?)\n*\\\\end{"|codeEnv|"}";

parseTeX = f -> (
    codes := select(codeRegex,codeComment|"$1",f);
    rest := separate(codeRegex,f); -- seems silly to do the regex twice
    --print(codes,rest);
    saveMode := topLevelMode; -- not thread-safe
    verbatim = false;
    topLevelMode = TeX;
    s := capture append(codes,codeComment);
    if s#0 then print ("warning: running the code produced an error"|s#1);
    topLevelMode = saveMode;
    --  s = separate("(?="|inputComment1|"[^%]*?"|codeComment|")",last s);
    s = apply(drop(separate("(?="|inputComment1|"[^%]*?"|codeComment1|")",last s),-1),
        x->"\\smallskip\n" | replace(codeComment1,"",x) | "\\smallskip\n");
    concatenate mingle(rest,s)
    )

-* ex of use
"M2inTeX/ex-parsed.tex" << parseTeX get "M2inTeX/ex.tex" << close
*-

-----------------
lastprompt:=""; -- borrowed from startup.m2.in
ZZ#{TeX,InputPrompt} = lineno -> (
    concatenate(
        inputComment,
        if not verbatim then (verbatim = true; "\\begin{verbatim}\n"),
        lastprompt = concatenate(interpreterDepth:"i",
            toString lineno,
            " : ")
        ))

ZZ#{TeX,InputContinuationPrompt} = lineno -> #lastprompt;

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

