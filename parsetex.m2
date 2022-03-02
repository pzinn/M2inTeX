debug Core

codeBegin = "\\begin{lstlisting}[language=Macaulay2]"; -- the environment used in the TeX file for M2 code
codeEnd = "\\end{lstlisting}";

escapeChar = "`"; -- code used to tell listings package that go out of verbatim mode. may need to use more obscure character

codeComment := "-* start code *- "; -- added at the start of every code chunk to split correctly

--fmt = x -> replace("\n","@\n",x);

parseTeX = f -> (
    codeRegex := "(?<="|regexQuote codeBegin|"\n)([\\s\\S]*?)(?="|regexQuote codeEnd|")";
    codes := select(codeRegex,f);
    rest := separate(codeRegex,f); -- seems silly to do the regex twice
    --print(fmt\codes,fmt\rest);
    saveMode := topLevelMode; -- not thread-safe
    topLevelMode = TeX;
    s := capture apply(codes,x->codeComment | x);
    if s#0 then print ("warning: running the code produced an error"|s#1);
    topLevelMode = saveMode;
    --print (fmt s#1);
    s = last s;
    if last s == "\n" then s=substring(s,0,#s-1);
    s = separate(regexQuote codeComment,s);
    prev := s#0;
    s = for i from 1 to #s-1 list (
	prev | (
	    l := regex("(\n).*(\n).*\\'",s#i);
	    (a,b) := if l===null then (0,0) else (l#1#0,l#2#0);
	    prev = substring(s#i,a+1,b-a-1);
	    --print ("prev=",fmt prev);
    	    substring(s#i,0,a)
	    ));
    --print (fmt\s);
    if #rest != #s + 1 then print "warning: code/noncode mismatch";
    concatenate mingle(rest,s)
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
    << on() | " = " | escapeChar | y | escapeChar << endl
    )

closeMaybe = x -> (
)    

texAfterPrint :=  x -> (
    if class x === Sequence then x = RowExpression deepSplice { x };
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid html output";
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

