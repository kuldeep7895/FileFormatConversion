exception Invalid;
exception emptyInputFile;
exception UnevenFields of string;

fun next_char input =
	Option.valOf (TextIO.input1 input)


fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
	let
		val ins = TextIO.openIn infilename
		val os = TextIO.openAppend  outfilename;
	in
		let
			fun iterFile(ins,numFieldChar,enclosed,lineNum,expecNumField,curNumField) = 
			if(TextIO.endOfStream ins) then ""
			else
				let
					val nextChar = next_char ins; 
				in
					if(numFieldChar = 0) then
						if(nextChar = #"\"") then
							concat ["\"",iterFile(ins,1,true,lineNum,expecNumField,curNumField)]
						else
							if(nextChar = delim1) then 
								concat["\"\""^str(delim2),iterFile(ins,0,false,lineNum,expecNumField,curNumField+1)]
							else
								concat ["\""^str(nextChar),iterFile(ins,1,false,lineNum,expecNumField,curNumField)]
					else
						if(enclosed) then
							if(nextChar = #"\"") then
								let
									val nextNextChar = next_char ins;
								in
									if(nextNextChar = delim1) then
										concat["\""^str(delim2),iterFile(ins,0,false,lineNum,expecNumField,curNumField+1)]
									else if(nextNextChar = #"\"") then
										concat["\""^"\"",iterFile(ins,numFieldChar+1,true,lineNum,expecNumField,curNumField)]
									else if(nextNextChar = #"\n") then
										if(lineNum = 1) then
											concat["\""^("\n"),iterFile(ins,0,false,lineNum+1,curNumField+1,0)]
										else
											if(not (curNumField+1=expecNumField)) then
												let
													val msg = concat["Expected: ",Int.toString(expecNumField)," fields, Present: ",Int.toString(curNumField+1)," fields on Line ",Int.toString(lineNum)]
												in
													raise  UnevenFields msg
												end
												
(*												raise 	UnevenFields "kuldeep"*)
											else
												concat["\""^("\n"),iterFile(ins,0,false,lineNum+1,expecNumField,0)]
									else if(nextNextChar = #"\r") then
										let
											val lfChar = next_char ins;
										in
											if(lfChar = #"\n") then
												if(lineNum = 1) then
													concat["\""^("\r\n"),iterFile(ins,0,false,lineNum+1,curNumField+1,0)]
												else
													if(not (curNumField+1=expecNumField)) then
														let
															val msg = concat["Expected: ",Int.toString(expecNumField)," fields, Present: ",Int.toString(curNumField+1)," fields on Line ",Int.toString(lineNum)]
														in
															raise  UnevenFields msg
														end
													else
														concat["\""^("\r\n"),iterFile(ins,0,false,lineNum+1,expecNumField,0)]
											else
												raise Invalid
										end
									else 
										raise Invalid
								end
							else
								concat [str(nextChar),iterFile(ins,numFieldChar+1,true,lineNum,expecNumField,curNumField)]
						else
							if(nextChar = delim1) then
								concat ["\""^str(delim2),iterFile(ins,0,false,lineNum,expecNumField,curNumField+1)]
							else
								if(nextChar = #"\n") then
									if(lineNum = 1) then
											concat["\""^("\n"),iterFile(ins,0,false,lineNum+1,curNumField+1,0)]
									else
										if(not (curNumField+1=expecNumField)) then
											let
												val msg = concat["Expected: ",Int.toString(expecNumField)," fields, Present: ",Int.toString(curNumField+1)," fields on Line ",Int.toString(lineNum)]
											in
												raise  UnevenFields msg
											end
										else
											concat["\""^("\n"),iterFile(ins,0,false,lineNum+1,expecNumField,0)]
									
								else if(nextChar = #"\r") then
									let
										val lfChar = next_char ins;
									in
										if(lfChar = #"\n") then
											if(lineNum = 1) then
												concat["\""^("\r\n"),iterFile(ins,0,false,lineNum+1,curNumField+1,0)]
											else
												if(not (curNumField+1=expecNumField)) then
													let
														val msg = concat["Expected: ",Int.toString(expecNumField)," fields, Present: ",Int.toString(curNumField+1)," fields on Line ",Int.toString(lineNum)]
													in
														raise  UnevenFields msg
													end
												else
													concat["\""^("\r\n"),iterFile(ins,0,false,lineNum+1,expecNumField,0)]
										else
											raise Invalid
									end
								else
									concat [str(nextChar),iterFile(ins,numFieldChar+1,false,lineNum,expecNumField,curNumField)]
				end
		in
(*			iterFile(ins)*)
			if(TextIO.endOfStream ins) then
				raise emptyInputFile
			else
				TextIO.output (os,iterFile(ins,0,false,1,0,0))
				handle UnevenFields msg => (print msg) 
		end
	end;
	

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");

fun tsv2csv(incilename, outfilename) = convertDelimiters(incilename,#"\t",outfilename,#",");

convertDelimiters("TestCases/himym_uneven.csv",#",", "out.txt", #"&");


(*fun convertNewlines(infilename, newline1, outfilename, newline2) = *)
(*	let*)
(*	in*)
(*	end;*)

