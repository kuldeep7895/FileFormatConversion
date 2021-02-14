(*fun convertDelimiters(infilename, delim1, outfilename, delim2) = *)
(*	let*)
(*		val allLines = readlist(infilename);*)
(*	in*)
(*		let*)
(*			fun iterLines([]) = ""*)
(*				| iterLines(x::xs) = convertLine(x,,delim1,delim2)+iterLines(xs);*)
(*		in*)
(*			iterLines(allLines)*)
(*		end*)
(*	end;*)
(*	*)
(*(*val ans = convertDelimiters("sample.txt",",","out.txt","/t");*)*)

(*fun convertLine(line,x,y) = *)
(*	let *)
(*		var expl = explode(line); *)
(*		var res = "";*)
(*	in*)
(*		let*)
(*			fun iterString([],x) = res;*)
(*				| iterString(y::ys,x) = *)
(*					if(y = x)*)
(*		in*)
(*			iterString(expl,x)*)
(*		end*)
(*	end;*)


(*fun csv2tsv(infilename, outfilename) = *)



(*fun tsv2csv(incilename, outfilename) = *)


(*fun convertNewlines(infilename, newline1, outfilename, newline2) = *)



(*fun unix2dos(infilename, outfilename) = *)




(*fun dos2unix(incilename, outfilename) = *)



val infile = "sample.txt" ;

fun readlist (infile : string) = let 

  val ins = TextIO.openIn infile 

  fun loop ins = 

   case TextIO.inputLine ins of 

      SOME line => line :: loop ins 

    | NONE      => [] 

in 

  loop ins before TextIO.closeIn ins 

end ;

val expl = explode("kuldeep");

val pureGraph =  readlist(infile);
