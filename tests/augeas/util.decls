typedef QUOTELESS_STRING = (UPPERCASE | LOWERCASE | DIGIT | " " | "'" | "_" | ":" | "/" | "-" | "." | "*" | "=")*;;

typedef STRING_COMPONENT = UPPERCASE | LOWERCASE | DIGIT | "\"" | " " | "'" | "_" | ":" | "/" | "-" | "." | "*" | "=";;
typedef STRING = STRING_COMPONENT*;;
typedef NONEMPTY_STRING = STRING_COMPONENT+;;
typedef DELIMITED_STRING = (UPPERCASE | LOWERCASE | DIGIT | "\\\"" | " " | "'" | "_" | ":" | "/" | "-" | "." | "*" | "=")*;;

typedef COMMENT = "# " STRING;;
(*test COMMENT matches "# comment";;*)
typedef COMMENT_DICT = "{\"#comment\"=\"" DELIMITED_STRING "\"}";;
(*test COMMENT_DICT matches "{\"#comment\"=\"comment\"}";;*)

typedef EMPTYDICT = "{}";;

typedef ENV_VAR = (UPPERCASE | "_")+;;
typedef WORD = (LOWERCASE | UPPERCASE | "_" | "." | "-" | ":" | "/" | "+" | DIGIT)*;;
(*test WORD matches "my_username";;*)

typedef SIMPLE_WORD = (LOWERCASE | UPPERCASE | DIGIT)+;;

typedef FOLDER = ( (WORD "/")* WORD (. | "/") )
		| ("${" ENV_VAR "}");;
(*test FOLDER matches "my_username";;*)
(*test FOLDER matches "${MY_HOME}";;*)

typedef MULTILINE_COMMENT = "/*" STRING ("\n" STRING)* "*/";;
(*test MULTILINE_COMMENT matches "/*test\nmultiline*/";;*)
(*test MULTILINE_COMMENT matches "/*testmultiline*/";;*)

typedef MULTILINE_COMMENT_DICT = "{\"#mcomment\"" (("{\"string\"=\""STRING"\"}")+) "}";;
(*test MULTILINE_COMMENT_DICT matches "{\"#mcomment\"{\"string\"=\"test\"}{\"string\"=\"multiline\"}}";;*)
(*test MULTILINE_COMMENT_DICT matches "{\"#mcomment\"{\"string\"=\"testmultiline\"}}";;*)
