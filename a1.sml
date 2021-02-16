fun next_char input =
	Option.valOf (TextIO.input1 input)


fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
	let
		val ins = TextIO.openIn infilename
	in
		let
			fun iterFile(ins) = 
			if(TextIO.endOfStream ins) then ""
			else
				let
					val nextChar = next_char ins; 
				in
					if(nextChar = #"\\") then 
						if(TextIO.endOfStream ins) then "\\"
						else
							let
								val nextNextChar = next_char ins;
							in
								if(nextNextChar = delim1) then concat [str(delim1),iterFile(ins)]
								else if(nextNextChar = delim2) then concat ["\\\\"^str(delim2),iterFile(ins)]
								else concat ["\\"^str(nextNextChar),iterFile(ins)]
							end
					else
						if(nextChar = delim1) then concat [str(delim2),iterFile(ins)]
						else if(nextChar = delim2) then concat ["\\"^str(delim2),iterFile(ins)]
						else concat [str(nextChar),iterFile(ins)]
				end
			
		in
			iterFile(ins)
		end
	end;
	

fun csv2tsv(infilename, outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t");

fun tsv2csv(incilename, outfilename) = convertDelimiters(incilename,#"\t",outfilename,#",");

