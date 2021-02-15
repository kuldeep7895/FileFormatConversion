
fun readlist (infile) = 
	let 

	  val ins = TextIO.openIn infile 

	  fun loop ins = 

	   case TextIO.inputLine ins of 

	      SOME line => line :: loop ins 

	    | NONE      => [] 

	in 

	  loop ins before TextIO.closeIn ins 

	end ;





fun convertLine(line,x,y) = 
	let 
		val expl = explode(line); 
		val res = ""
	in
		let
			fun iterString([],x) = ""
				| iterString(z::zs,x) = 
					if(z = y) then concat ["\\"^str(z),iterString(zs,x)]
					else
						if (z = #"\\") then
							if(not(zs=[]) andalso hd(zs)=x) then concat [str(x),iterString(tl(zs),x)] else concat [str(z), iterString(zs,x)]
					else
						if(z = x ) then concat [str(y) , iterString(zs,x)]
					else
						concat [str(z), iterString(zs,x)]
		in
			iterString(expl,x)
		end
	end;


fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
	let
		val allLines = readlist(infilename);
		val os = TextIO.openAppend  outfilename;
		val res = "";
	in
		let
			fun iterLines([]) = ""
				| iterLines(x::xs) = convertLine(x,delim1,delim2)^iterLines(xs);
		in
			TextIO.output (os, iterLines(allLines)) 
		end
	end;
	


(*val ans = convertDelimiters("sample.txt",#",","out.txt",#"\t");*)

(*val ans = convertDelimiters("sample.txt",#",","out.txt",#"|");*)

(*val ans = convertDelimiters("out.txt",#"|","sample.txt",#",");*)

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");



fun tsv2csv(incilename, outfilename) = convertDelimiters(incilename,#"\t",outfilename,#",");


fun convertLineBreak("",x,y) = ""
	| convertLineBreak(line,x,y) = 
		let
			val expl = explode(line);
			val len = List.length(explode(line)); 
		in
			if(List.nth(expl,len-1) = x) then implode(List.update(expl,len-1,y))
			else implode(expl)^str(y)
		end;

fun convertNewlines(infilename, newline1, outfilename, newline2) = 
	let
		val allLines = readlist(infilename);
		val os = TextIO.openAppend  outfilename;
	in
		let
			fun iterLines([]) = ""
				| iterLines(x::xs) = convertLineBreak(x,newline1,newline2)^iterLines(xs);
		in
			TextIO.output (os, iterLines(allLines)) 
		end
	end;

fun unix2dos(infilename, outfilename) = fun convertNewlines(infilename, #"\n", outfilename, #"\r\n");




(*fun dos2unix(incilename, outfilename) = *)


