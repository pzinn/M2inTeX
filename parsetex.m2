debug Core

codeBegin = "\\begin{lstlisting}"; -- the environment used in the TeX file for M2 code
codeEnd = "\\end{lstlisting}";

inputCmd = "\\lstinputlisting"; -- same thing but inputs the code from an external file

inputLanguage = "Macaulay2"; -- option [language=Macaulay2] to the commands above
outputLanguage = "Macaulay2output"; -- really just an alias to avoid rerunning run code

escapeChar = "`"; -- code used to tell listings package to go out of verbatim mode. may need to use more obscure character

codeComment := "-* start code *- "; -- added at the start of every code chunk to split correctly

outputCmd = "\\macoutput"; -- for direct access to output

--fmt = x -> replace("\n","@\n",x);

local outputs;

parseFile = fn -> (
    r := regex("/[^/]*$",fn);
    parseTeX(get fn, Path => if r === null then "" else substring(fn,0,r#0#0+1))
    )

-* ex of use
"M2inTeX/ex-parsed.tex" << parseFile "M2inTeX/ex.tex" << close
*-

parseTeX = { Path => "" } >> o -> s -> (
    langRegex := "(\\[[^\\]]*language=)"|inputLanguage|"(\\]|,[\\s\\S]*?\\])";
    codeRegex := "(?:" | regexQuote codeBegin | langRegex | "\n([\\s\\S]*?)"|regexQuote codeEnd
    | "|" | regexQuote inputCmd | langRegex | "\\{(.*?)\\}" | ")"; -- phew
    codes := select(codeRegex,s);
    rest := separate(codeRegex,s); -- seems silly to do the regex twice
    --print(fmt\codes,fmt\rest);
    outputs = new MutableHashTable;
    codes = apply(codes, x -> (
	    r := regex(codeRegex,x); -- ... and once more ...
	    if r#1#1 != 0 then (
		substring(r#1,x) | outputLanguage | substring(r#2,x) | "\n", -- option
		substring(r#3,x) -- code
		) else (
		substring(r#4,x) | outputLanguage | substring(r#5,x) | "\n",
		try get (o.Path|substring(r#6,x)) else ""
	       )
	   ));
    saveMode := topLevelMode; -- not thread-safe
    topLevelMode = TeX;
    s = capture apply(codes, x -> codeComment | x#1);
    topLevelMode = saveMode;
    if s#0 then print ("warning: running the code produced an error"|s#1);
    --print (fmt s#1);
    --print peek outputs;
    s = last s;
    if last s == "\n" then s=substring(s,0,#s-1);
    s = separate(regexQuote codeComment,s);
    prev := s#0;
    s = for i from 1 to #s-1 list (
	codeBegin | codes#(i-1)#0 | prev | (
	    l := regex("(\n).*(\n).*\\'",s#i);
	    (a,b) := if l===null then (0,0) else (l#1#0,l#2#0);
	    prev = substring(s#i,a+1,b-a-1);
	    --print ("prev=",fmt prev);
    	    substring(s#i,0,a)
	    ) | "\n" | codeEnd
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
		(if outputs#?i then outputs#i else "") | "%" | x | "\n" )))
    )

-* ex of use (assuming ex.tex is in the current directory)
"ex-parsed.tex" << parseTeX get "ex.tex" << close
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
    outputs#lineNumber = y; -- store output
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

