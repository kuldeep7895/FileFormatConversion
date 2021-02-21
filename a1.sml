exception Invalid

fun next_char input =
	Option.valOf (TextIO.input1 input)


fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
	let
		val ins = TextIO.openIn infilename
		val os = TextIO.openAppend  outfilename;
	in
		let
			fun iterFile(ins,numFieldChar,enclosed) = 
			if(TextIO.endOfStream ins) then ""
			else
				let
					val nextChar = next_char ins; 
				in
					if(numFieldChar = 0) then
						if(nextChar = #"\"") then
							concat ["\"",iterFile(ins,1,true)]
						else
							if(nextChar = delim1) then 
								concat["\"\""^str(delim2),iterFile(ins,0,false)]
							else
								concat ["\""^str(nextChar),iterFile(ins,1,false)]
					else
						if(enclosed) then
							if(nextChar = #"\"") then
								let
									val nextNextChar = next_char ins;
								in
									if(nextNextChar = delim1) then
										concat["\""^str(delim2),iterFile(ins,0,false)]
									else if(nextNextChar = #"\"") then
										concat["\""^"\"",iterFile(ins,numFieldChar+1,true)]
									else if(nextNextChar = #"\n") then
										concat["\""^("\n"),iterFile(ins,0,false)]
									else if(nextNextChar = #"\r") then
										let
											val lfChar = next_char ins;
										in
											if(lfChar = #"\n") then
												concat["\""^("\r\n"),iterFile(ins,0,false)]
											else
												raise Invalid
										end
									else 
										raise Invalid
								end
							else
								concat [str(nextChar),iterFile(ins,numFieldChar+1,true)]
						else
							if(nextChar = delim1) then
								concat ["\""^str(delim2),iterFile(ins,0,false)]
							else
								if(nextChar = #"\n") then
									concat["\""^("\n"),iterFile(ins,0,false)]
								else if(nextChar = #"\r") then
									let
										val lfChar = next_char ins;
									in
										if(lfChar = #"\n") then
											concat["\""^("\r\n"),iterFile(ins,0,false)]
										else
											raise Invalid
									end
								else
									concat [str(nextChar),iterFile(ins,numFieldChar+1,false)]
				end
		in
(*			iterFile(ins)*)
			TextIO.output (os,iterFile(ins,0,false)) 
		end
	end;
	

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");

fun tsv2csv(incilename, outfilename) = convertDelimiters(incilename,#"\t",outfilename,#",");

convertDelimiters("TestCases/himym.csv",#",", "out.txt", #"&");


(*fun convertNewlines(infilename, newline1, outfilename, newline2) = *)
(*	let*)
(*	in*)
(*	end;*)

