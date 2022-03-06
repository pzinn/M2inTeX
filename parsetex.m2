debug Core

codeBegin = "\\begin{lstlisting}[language=Macaulay2]\n"; -- the environment used in the TeX file for M2 code
codeEnd = "\\end{lstlisting}";

escapeChar = "`"; -- code used to tell listings package that go out of verbatim mode. may need to use more obscure character

codeComment := "-* start code *- "; -- added at the start of every code chunk to split correctly

runComment := "% M2 output\n"; -- comment added in TeX to mark start of a M2 code chunk that's already been run

outputCmd = "\\macoutput"; -- for direct access to output

--fmt = x -> replace("\n","@\n",x);

local outputLst;

parseTeX = s -> (
    norerun := "(?<!"|regexQuote runComment|")"; -- to avoid rerunning
    codeRegex := norerun | regexQuote codeBegin|"([\\s\\S]*?)"|regexQuote codeEnd;
    codes := select(codeRegex,s);
    rest := separate(codeRegex,s); -- seems silly to do the regex twice
    --print(fmt\codes,fmt\rest);
    outputLst = new MutableList;
    saveMode := topLevelMode; -- not thread-safe
    topLevelMode = TeX;
    s = capture apply(codes,x -> codeComment | substring(x,#codeBegin,#x-#codeBegin-#codeEnd));
    topLevelMode = saveMode;
    if s#0 then print ("warning: running the code produced an error"|s#1);
    --print (fmt s#1);
    s = last s;
    if last s == "\n" then s=substring(s,0,#s-1);
    s = separate(regexQuote codeComment,s);
    prev := s#0;
    s = for i from 1 to #s-1 list (
	runComment | codeBegin | prev | (
	    l := regex("(\n).*(\n).*\\'",s#i);
	    (a,b) := if l===null then (0,0) else (l#1#0,l#2#0);
	    prev = substring(s#i,a+1,b-a-1);
	    --print ("prev=",fmt prev);
    	    substring(s#i,0,a)
	    ) | codeEnd
	);
    --print (fmt\s);
    if #rest != #s + 1 then print "warning: code/noncode mismatch";
    s = concatenate mingle(rest,s);
    -- final step: replace outputCmd with actual output
    outputRegex := "(?<!%)" | regexQuote outputCmd | "\\{\\d+\\}";
    codes = select(outputRegex,s);
    rest = separate(outputRegex,s); -- seems silly to do the regex twice
    concatenate mingle(rest, apply(codes, x -> (
		i := value substring(x,#outputCmd+1,#x-#outputCmd-2);
		(if outputLst#?i then outputLst#i else "") | "%" | x | "\n" )))
    )

-* ex of use
"M2inTeX/ex-parsed.tex" << parseTeX get "M2inTeX/ex.tex" << close
*-

-----------------
lastprompt:=""; -- borrowed from startup.m2.in

ZZ#{TeX,InputPrompt} = lineno -> concatenate(
    escapeChar,
    "\\underline{",
    lastprompt = concatenate(interpreterDepth:"i",toString lineno),
    "}",
    escapeChar,
    " : "
    )

ZZ#{TeX,InputContinuationPrompt} = lineno -> #lastprompt+3

Nothing#{TeX,Print} = identity

on := () -> concatenate(escapeChar,"\\underline{",interpreterDepth:"o", toString lineNumber,"}",escapeChar)

Thing#{TeX,Print} = x -> (
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid TeX output";
    outputLst#lineNumber = y; -- store output
    << on() | " = " | escapeChar | y | escapeChar << endl
    )

texAfterPrint :=  x -> (
    if class x === Sequence then x = RowExpression deepSplice { x };
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << on() | " : " | escapeChar |  y | escapeChar  << endl;
    )

Thing#{TeX,AfterNoPrint} = identity

InexactNumber#{TeX,Print} = x ->  withFullPrecision ( () -> Thing#{TeX,Print} x )

printFunc#TeX = x -> (
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid TeX output";
    << escapeChar | y | escapeChar << endl;
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

