
fun readlist (infile : string) = 
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
					if(z = y) then concat ["\\y" , iterString(zs,x)]
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
		val ins = TextIO.openAppend  outfilename;
		val res = "";
	in
		let
			fun iterLines([]) = ""
				| iterLines(x::xs) = convertLine(x,delim1,delim2)^iterLines(xs);
		in
			TextIO.output (ins, iterLines(allLines)) 
		end
	end;
	


val ans = convertDelimiters("sample.txt",#",","out.txt",#"\t");

(*fun csv2tsv(infilename, outfilename) = *)



(*fun tsv2csv(incilename, outfilename) = *)


(*fun convertNewlines(infilename, newline1, outfilename, newline2) = *)



(*fun unix2dos(infilename, outfilename) = *)




(*fun dos2unix(incilename, outfilename) = *)



val infile = "sample.txt" ;

val expl = explode("kuldeep");

val pureGraph =  readlist(infile);
