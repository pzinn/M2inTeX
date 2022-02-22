debug Core

codeBegin = "\\begin{lstlisting}[language=Macaulay2]"; -- the environment used in the TeX file for M2 code
codeEnd = "\\end{lstlisting}";

escapeChar = "`";

verbatim := false; -- not thread-safe

codeComment := "-* start code *- "; -- added at the start of every code chunk to split correctly

inputComment := "% start M2\n"; -- comment added in TeX to mark start of a M2 code chunk

parseTeX = f -> (
    codeRegex := "(?<!"|regexQuote inputComment|")"|regexQuote codeBegin|"\n*([\\s\\S]*?)\n*"|regexQuote codeEnd; -- the negative lookbehind means, don't rerun
    codeComment1 := regexQuote codeComment;
    inputComment1 := regexQuote inputComment;

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

ZZ#{TeX,InputPrompt} = lineno -> concatenate(
    inputComment,
    if not verbatim then (verbatim = true; codeBegin|"\n"), -- should always be true
    escapeChar,
    "\\underline{",
    lastprompt = concatenate(interpreterDepth:"i",toString lineno),
    "}",
    escapeChar,
    " : "
    )

ZZ#{TeX,InputContinuationPrompt} = lineno -> if verbatim then #lastprompt+3 else (
    verbatim = true;
    concatenate(
	codeBegin,
	"\n",
	#lastprompt:" "
	))

Nothing#{TeX,Print} = identity

on := () -> concatenate(escapeChar,"\\underline{",interpreterDepth:"o", toString lineNumber,"}",escapeChar)

Thing#{TeX,Print} = x -> (
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid TeX output";
    if not verbatim then (verbatim = true; << codeBegin|"\n"; );
    << on() | " = " | escapeChar | y | escapeChar << endl
    )

closeMaybe = x -> (
    if verbatim then (
        << codeEnd|"\n";
        verbatim = false;
        );    
)    

texAfterPrint :=  x -> (
    if class x === Sequence then x = RowExpression deepSplice { x };
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    if not verbatim then (verbatim = true; << codeBegin|"\n"; );
    << on() | " : " | escapeChar |  y | escapeChar  << endl;
    closeMaybe();
    )

Thing#{TeX,AfterNoPrint} = closeMaybe

-- all that's below would go if afterprint was expressionified

Thing#{TeX,AfterPrint} = x -> texAfterPrint class x;

Boolean#{TeX,AfterPrint} = closeMaybe

Expression#{TeX,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{TeX,AfterPrint} = closeMaybe

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

Net#{TeX,AfterPrint} = closeMaybe

Nothing#{TeX,AfterPrint} = closeMaybe

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

ZZ#{TeX,AfterPrint} = closeMaybe

