type pos = int
type lexresult = Tokens.token


val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1


val strVal = ref "";
val strCount = ref 0;

val count = ref 0;
val copy = ref 0; 

val temp_line = ref 1;
val temp_pos  = ref [1];


val eof = fn () => 
			let
				 val pos = hd(!linePos) 
				 val error_pos = hd(!temp_pos)
				 
			in 
				if !count > 0 then (lineNum := !temp_line; linePos := tl(!temp_pos);  ErrorMsg.error error_pos ("Unclosed_Comment"))
				else if !strCount > 0 then (lineNum := !temp_line;linePos := tl(!temp_pos); ErrorMsg.error error_pos ("Unclosed_String"))
				else ();
			  (count := 0; copy := 0; strVal := ""; strCount := 0;temp_line := 1; temp_pos := [1]; ErrorMsg.reset();  Tokens.EOF(pos,pos)) 
			end


%%
%s COMMENT STRING IGNORE;
SPACE = [" "\t];
%%
<INITIAL> var       => (Tokens.VAR(yypos,yypos+3));
<INITIAL> function  => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>	break	    => (Tokens.BREAK(yypos, yypos+5));
<INITIAL> while	    => (Tokens.WHILE(yypos,yypos+5));
<INITIAL> for	      => (Tokens.FOR(yypos,yypos+3));
<INITIAL> to    		=> (Tokens.TO(yypos,yypos+2));
<INITIAL> let   		=> (Tokens.LET(yypos, yypos+3));
<INITIAL> in				=> (Tokens.IN(yypos, yypos+2));
<INITIAL> end		 		=> (Tokens.END(yypos,yypos+3));
<INITIAL> type			=> (Tokens.TYPE(yypos, yypos+4));
<INITIAL> array   	=> (Tokens.ARRAY(yypos, yypos+5));
<INITIAL> if				=> (Tokens.IF(yypos, yypos+2));
<INITIAL> then	 		=> (Tokens.THEN(yypos, yypos+4));
<INITIAL> else			=> (Tokens.ELSE(yypos, yypos+4));
<INITIAL> do				=> (Tokens.DO(yypos, yypos+2));
<INITIAL> of 				=> (Tokens.OF(yypos, yypos+2));
<INITIAL> nil				=> (Tokens.NIL(yypos, yypos+3));

<INITIAL> ","	      => (Tokens.COMMA(yypos,yypos+1));
<INITIAL> ":"				=> (Tokens.COLON(yypos,yypos+1));
<INITIAL> ";"  		  => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> "("				=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ")"				=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "["				=> (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> "]"				=> (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "{"				=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "}"				=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL> "."				=> (Tokens.DOT(yypos,yypos+1));
<INITIAL> "+" 			=> (Tokens.PLUS(yypos,yypos+1));
<INITIAL> "-"				=> (Tokens.MINUS(yypos,yypos+1));
<INITIAL> "/"				=> (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "*"				=> (Tokens.TIMES(yypos,yypos+1));
<INITIAL> "="				=> (Tokens.EQ(yypos,yypos+1));
<INITIAL> "<>"			=> (Tokens.NEQ(yypos,yypos+2));
<INITIAL> ">"				=> (Tokens.GT(yypos,yypos+1));
<INITIAL> ">="			=> (Tokens.GE(yypos,yypos+2));
<INITIAL> "<"				=> (Tokens.LT(yypos,yypos+1));
<INITIAL> "<="			=> (Tokens.LE(yypos,yypos+2));
<INITIAL> "&"				=> (Tokens.AND(yypos,yypos+1));
<INITIAL> "|"				=> (Tokens.OR(yypos,yypos+1));
<INITIAL> ":="			=> (Tokens.ASSIGN(yypos,yypos+2));

<INITIAL> \n 		    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> "/*"			=> (YYBEGIN COMMENT; count := !count+1; copy := !copy + 1; temp_line := !lineNum; temp_pos := yypos :: !linePos;  continue());
<INITIAL> \"				=> (strCount := 1; strVal := ""; temp_line := !lineNum; temp_pos := yypos :: !linePos;  YYBEGIN STRING;  continue());

<INITIAL> [0-9]+    => (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos+size(yytext)));
<INITIAL> [a-zA-Z][0-9a-zA-Z_]*  => (Tokens.ID(yytext, yypos, yypos+size(yytext)));

<INITIAL> {SPACE}	  => (continue());
<INITIAL> .       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<COMMENT> "/*"		=> (count := !copy+1; copy := !count; continue());
<COMMENT> "*/"		=> (count := !copy-1; copy := !count; if !count <= 0 then (YYBEGIN INITIAL; count := 0; copy := 0;  continue()) else continue());   
<COMMENT> \n			=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; strVal := !strVal ^ yytext; continue());
<COMMENT> .				=> (continue());

<STRING>  \"			=> (strCount := 0; YYBEGIN INITIAL; Tokens.STRING(!strVal, yypos, yypos+size(!strVal)));
<STRING>  \\\"	 	=> (strVal := !strVal ^ "\""; continue());
<STRING>  \\t 		=> (strVal := !strVal ^ "\t"; continue());
<STRING>  \\n			=> (strVal := !strVal ^ "\n"; continue());
<STRING>  \\\^[@A-Z\[\\\]\^_] 		  => (strVal := !strVal ^ Char.toString(chr((ord(String.sub(yytext,2))-64))) ; continue()); 
<STRING>  \\[0-9]{3} => (strVal := !strVal ^ Char.toString(chr(valOf(Int.fromString(substring(yytext,1,3))))); continue());
<STRING>  \\\\    => (strVal := !strVal ^ "\\"; continue());
<STRING>  \\ 			=> (YYBEGIN IGNORE; continue());
<STRING>  \\.      => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
<STRING>  \n 	    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; strVal := !strVal ^ yytext; continue());
<STRING>  . 	=> (strVal := !strVal ^ yytext; continue());


<IGNORE>  \\			=> (YYBEGIN STRING; continue());
<IGNORE>  \n 		  => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<IGNORE>  {SPACE} => (continue());
<IGNORE>  .       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());





