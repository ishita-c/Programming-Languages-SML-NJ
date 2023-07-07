exception BadFormattingException of string ;

fun mdt2html(filename: string) =
	let 
		val ins = TextIO.openIn(filename^".mdt")
		val outs = TextIO.openOut(filename^".html")
		val init = TextIO.output(outs, "<html><body>\n")
		val backslash = #"\\"
		
		fun helper(ch : char option, hn: int, ho: int, an: int, ao: int, hr: int, ut: string,q: int, al: string, dl: string, nl: int, ul: int , ol: string, p: int, t: int, tf: string, bq: int, cb: int) = 
			(* Variables --
				ch : character under consideration in the current recursive call
				hn : count of '#'s
				ho : whether the current recursive call is in a heading
				an : count of '*'s
				ao : whether the current recursive call is between '*'s
				hr : count of '-'s
				ut : text to be underlined 
				q : double quotes on
				al : automatic link text, "_" by default
				dl : direct link text
				nl : new lines
				ul : whether inside unordered list
				ol : ordered list text before "."
				p : whether inside paragraph
				t : whether inside table
				tf : table field (alphanumeric)
				bq : whether inside blockquotes
				cb : whether inside codeblock
			*)
			
			case ch of 
				NONE =>
					(
						if ol <> "_" then (
							TextIO.output(outs, "</p></li>\n");
							TextIO.output(outs, "</ol>\n")
						) 
						else if cb = 1 then (
							TextIO.output(outs,"</code></pre>\n") 
						)
						else (
							TextIO.output(outs, "\n")
						);
						TextIO.closeIn ins;
						TextIO.output(outs, "</body></html>");
						TextIO.closeOut outs
					)
			|   SOME(c) =>
					if cb = 1 then (
						if c <> #"\t" andalso c <> #" " andalso nl = 1 then (
							TextIO.output(outs,"</code></pre>") ;
							helper(SOME c, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, 0)
						) 
						else if c = #"\n" then (
							TextIO.output1(outs,c) ;
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 1, ul, ol, p, t, tf, bq, 1)
						) 
						else if c = #"<" then (
							TextIO.output(outs, "&lt;") ;
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, 1)
						) 
						else if c = #">" then (
							TextIO.output(outs, "&gt;") ;
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, 1)
						) 
						else if c = #"&" then (
							TextIO.output(outs, "&amp;") ;
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, 1)
						)
						else if c = #"\t" andalso nl = 1 then (
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, 1)
						)
						else (
							TextIO.output1(outs,c) ;
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, 1)
						)
					)
					
					else if c = #"\"" andalso al = "_" then (
						TextIO.output1(outs,c) ;
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, 1 - q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if q = 1 then (
						if c = backslash then (
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) else (
							TextIO.output1(outs,c) ;
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						)
					)
					else if nl > 1 andalso ul = 0 andalso ol = "_"  then (
						if p = 0  then (
							TextIO.output(outs, "<p>");
							helper(SOME c, hn, ho, an, ao, hr, ut, q, al, dl, 1, ul, ol, 1, t, tf, bq, cb)
						) 
						else if Char.isAlpha(c) then (
							TextIO.output(outs, "</p><p>");
							helper(SOME c, hn, ho, an, ao, hr, ut, q, al, dl, 1, ul, ol, 1, t, tf, bq, cb)
						) else if p = 1 then (
							TextIO.output(outs, "</p>");
							helper(SOME c, hn, ho, an, ao, hr, ut, q, al, dl, 1, ul, ol, 0, t, tf, bq, cb)
						) else (
							helper(SOME c, hn, ho, an, ao, hr, ut, q, al, dl, 1, ul, ol, 0, t, tf, bq, cb)
						)
					)
					else if c = #"#" then
						(helper(TextIO.input1 ins, hn + 1, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if hn > 0 andalso ho = 0 then (
						if hn < 7 then (
							TextIO.output(outs, "<h"^(Int.toString(hn))^">") ;
							helper(SOME c, hn, 1, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) else (
							raise BadFormattingException("Error: Only 6 levels of headings available.")
						)
					)
					else if c <> #"(" andalso dl <> "_" andalso al = "w"  then (
						TextIO.output(outs, "["^dl^"]");
						helper(SOME c, hn, ho, an, ao, hr, ut, q, "_", "_", 0, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"*" andalso ao = 0 then (
						helper(TextIO.input1 ins, hn, ho, an + 1, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"*" andalso ao > 0 then (
						helper(TextIO.input1 ins, hn, ho, an - 1, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if an > 0 andalso ao = 0 then (
						if c = #" " then (
							helper(TextIO.input1 ins, hn, ho, an, 0, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) 
						else if an = 1 then (
							TextIO.output(outs, "<em>") ;
							helper(SOME c, hn, ho, an, 1, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) else if an = 2 then (
							TextIO.output(outs, "<strong>") ;
							helper(SOME c, hn, ho, an, 2, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) else if an = 3 then (
							TextIO.output(outs, "<strong><em>") ;
							helper(SOME c, hn, ho, an, 3, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) else (
							raise BadFormattingException("Error: Too many asterisks!")
						)
					)
					else if an = 0 andalso ao > 0 then (
						if ao = 1 then (
							TextIO.output(outs, "</em>") ;
							helper(SOME c, hn, ho, 0, 0, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) else if ao = 2 then  (
							TextIO.output(outs, "</strong>") ;
							helper(SOME c, hn, ho, 0, 0, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) else (
							TextIO.output(outs, "</em></strong>") ;
							helper(SOME c, hn, ho, 0, 0, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						)
					)
					else  if an < ao andalso ao > 0 then (
						raise BadFormattingException("Error: " ^ Int.toString(an)^" missing asterisk!")
					)
					else if c = #"-" andalso dl = "_" andalso al = "_" then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr + 1, ut, q, al, dl, nl, ul, ol, p, t, tf, bq, cb)
					)
					else if hr > 0 then (
						if hr = 1 andalso nl > 0 then (
							if ul = 1 then (
								TextIO.output(outs, "</p></li>");
								TextIO.output(outs, "<p><li>")
							) else (
								
								TextIO.output(outs, "<ul>\n");
								TextIO.output(outs, "<p><li>")
							);
							helper(SOME c, hn, ho, an, ao, hr-1, ut, q, al, dl, 0, 1, ol, p, t, tf, bq, cb)
						)
						else if hr < 3 then (
							TextIO.output1(outs, #"-");
							helper(SOME c, hn, ho, an, ao, hr-1, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						) else (
							TextIO.output(outs, "<hr>");
							helper(SOME c, hn, ho, an, ao, 0, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
						)
					)
					else if (c,ut) = (#"_","_")  then (
						TextIO.output(outs, "<u>");
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut^" ", q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"_" then (
						if String.sub(ut,0) = #"_" then (
							TextIO.output(outs, String.extract(ut,1,NONE))
						) else (
							TextIO.output(outs, ut)
						);
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, " ", q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if ut <> "_" andalso c = #"\n" then (
						TextIO.output(outs, "</u>");
						TextIO.output(outs, String.extract(ut,1,NONE));
						helper(SOME c, hn, ho, an, ao, hr, "_", q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if ut <> "_" andalso c <> #" " then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut^String.str(c), q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if ut <> "_" andalso c = #" " then (
						TextIO.output(outs, "</u>");
						TextIO.output(outs, String.extract(ut,1,NONE));
						helper(SOME c, hn, ho, an, ao, hr, "_", q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"<" andalso al = "" then (
						TextIO.output(outs, "<CENTER><TABLE border=\"1\">");
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", dl, 0, ul, ol, p, 1, tf, bq, cb)
					)
					else if c = #"<" then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "", dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #">" andalso t = 1 then (
						TextIO.output(outs, "</TABLE></CENTER>");
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, 3, tf, bq, cb)
					)
					else if c <> #">" andalso t = 3 then (
						raise BadFormattingException("Error: Please close your tables properly!")
					)
					else if c = #">" then (
						if al = "_" andalso t = 0 andalso nl = 1 then (
							TextIO.output(outs,"<blockquote>");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", dl, 0, ul, ol, p, t, tf, bq + 1, cb)
						)
						else if al = "_" andalso t = 0 andalso bq > 0 then (
							TextIO.output(outs,"<blockquote>");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", dl, 0, ul, ol, p, t, tf, bq + 1, cb)
						)
						else if String.size(al)>6 andalso String.substring(al,0,7) = "http://" then (
							TextIO.output(outs, "<a href=\"");
							TextIO.output(outs, al);
							TextIO.output(outs, "\">");
							TextIO.output(outs, al);
							TextIO.output(outs, "</a>");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", dl, 0, ul, ol, p, t, tf, bq, cb)
						)
						else if String.size(al)>7 andalso String.substring(al,0,8) = "https://" then (
							TextIO.output(outs, "<a href=\"");
							TextIO.output(outs, al);
							TextIO.output(outs, "\">");
							TextIO.output(outs, al);
							TextIO.output(outs, "</a>");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", dl, 0, ul, ol, p, t, tf, bq, cb)
						) else if t = 3 then (
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, 0, tf, bq, cb)
						)
						else if al <> "_" then (
							TextIO.output(outs, "<");
							TextIO.output(outs, al);
							TextIO.output(outs, ">");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", dl, 0, ul, ol, p, t, tf, bq, cb)
						) else (
							TextIO.output(outs, ">");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", dl, 0, ul, ol, p, t, tf, bq, cb)
						)
					)
					else if t > 0 then (
						if c = #"|" andalso t = 1 then (
							TextIO.output(outs, "<TR><TD>"^tf^"</TD>");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, 2, "", bq, cb)
						) else if c = #"|" andalso t = 2 then (
							TextIO.output(outs, "<TD>"^tf^"</TD>");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, 2, "", bq, cb)
						) else if c = #"\n" andalso tf = "" then (
							TextIO.output1(outs, c);
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, 1, "", bq, cb)
						)
						else if c = #"\n" then (
							TextIO.output(outs, "<TD>"^tf^"</TD></TR>\n");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, 1, "", bq, cb)
						) else (
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf^String.str(c), bq, cb)
						)
					)
					else if c = #"[" then (
						if dl <> "_" then (
							TextIO.output(outs, "["^dl^"]")
						) else ();
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", "", 0, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"]" then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "w", dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"(" andalso al = "w" then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "", dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #")" andalso al <> "_" then (
						TextIO.output(outs, "<a href=\"");
						TextIO.output(outs, al);
						TextIO.output(outs, "\">");
						TextIO.output(outs, dl);
						TextIO.output(outs, "</a>");
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, "_", "_", 0, ul, ol, p, t, tf, bq, cb)
					)
					else if al <> "_" then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al^String.str(c), dl, 0, ul, ol, p, t, tf, bq, cb)
					)
					else if dl <> "_" then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl^String.str(c), 0, ul, ol, p, t, tf, bq, cb)
					)
					else if hn > 0 andalso c = #"\n" then (
						TextIO.output(outs, "</h"^(Int.toString(hn))^">") ;
						TextIO.output1(outs,c) ;
						helper(TextIO.input1 ins, 0, 0, an, ao, hr, ut, q, al, dl, 1, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"\n" andalso nl = 1 andalso ul = 1 then (
						TextIO.output(outs, "</p></li>\n");
						TextIO.output(outs, "</ul>\n");
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 2, 0, ol, p, t, tf, bq, cb)
					) 
					else if c = #"\n" andalso nl = 1 then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 2, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"\n" andalso nl = 2 andalso ol <> "_" then (
						TextIO.output(outs, "</p></li>\n");
						TextIO.output(outs, "</ol>\n");
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 2, 0,"_", p, t, tf, bq, cb)
					) 

					else if Char.isDigit(c) andalso nl > 0 then (
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, nl, ul, ol^String.str(c), p, t, tf, bq, cb)
					)
					else if String.sub(ol,0) = #"o" andalso String.size(ol) > 1 andalso nl > 0 andalso c = #"." then (
						if ul = 1 then (
							TextIO.output(outs, "</ul>\n")
						) else ();
						TextIO.output(outs, "</p></li>\n");
						TextIO.output(outs, "<p><li>");
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, "o", p, t, tf, bq, cb)
					)
					else if String.size(ol) > 1 andalso nl > 0 then (
						if c = backslash then (
							TextIO.output(outs, String.extract(ol,1,NONE));
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, "_", p, t, tf, bq, cb)
						) 
						else if c = #"." then (
							TextIO.output(outs, "<ol>\n");
							TextIO.output(outs, "<p><li>");
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, "o", p, t, tf, bq, cb)
						)else (
							TextIO.output(outs, String.extract(ol,1,NONE));
							TextIO.output1(outs, c);
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, "_", p, t, tf, bq, cb)
						)
					)

					else if c = #"\n" then (
						if bq > 0 then (
							TextIO.output(outs, "</blockquote>");
							helper(SOME c, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq - 1, cb)
						) else (
							TextIO.output1(outs,c) ;
							helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 1, ul, ol, p, t, tf, bq, cb)
						)
					)
					else if c = #" " then (
						TextIO.output1(outs,c) ;
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, nl, ul, ol, p, t, tf, bq, cb)
					)
					else if c = #"\t" andalso nl > 0 andalso cb = 0 then (
						TextIO.output(outs,"<pre><code>\n") ;
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, 1)
					)
					else (
						TextIO.output1(outs,c) ;
						helper(TextIO.input1 ins, hn, ho, an, ao, hr, ut, q, al, dl, 0, ul, ol, p, t, tf, bq, cb)
					)
	in 
		helper(TextIO.input1 ins, 0, 0, 0, 0, 0, "_", 0, "_", "_", 1, 0, "_", 0, 0, "", 0, 0)
	end
		handle BadFormattingException(errorMsg) => print(errorMsg^"\n");
