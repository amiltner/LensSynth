quotient typedef LOWERCASE = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ;;
quotient typedef UPPERCASE = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ;;
quotient typedef DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;;
quotient typedef NUMBER = DIGIT+;;


quotient typedef QUOTELESS_STRING = (UPPERCASE | LOWERCASE | DIGIT | " " | "'" | "_" | ":" | "/" | "-" | "." |  "=" | "+")*;;

quotient typedef STRING_COMPONENT = UPPERCASE | LOWERCASE | DIGIT | "\"" | " " | "'" | "_" | ":" | "/" | "-" | "." | "=" | "+";;
quotient typedef STRING = STRING_COMPONENT*;;
quotient typedef COMMENT = "# " STRING;;
quotient typedef COMMENT_DICT = "{\"#comment\"=\"" DELIMITED_STRING "\"}";;

quotient typedef DELIMITED_STRING = (UPPERCASE | LOWERCASE | DIGIT | "\\\"" | " " | "'" | "_" | ":" | "/" | "-" | "." | "=" | "+")*;;

quotient string_to_delimited = [STRING <=> DELIMITED_STRING {}]

quotient typedef INDENT = [ (" " | "\t")* -> " " ];;
quotient typedef INDENT_REQ = [ (" " | "\t")+ -> " "];;
quotient typedef ALPHANUM = (UPPERCASE | LOWERCASE | DIGIT)+;;
quotient typedef RANGE = (ALPHANUM "-" ALPHANUM | ALPHANUM );;
quotient typedef PREFIX = "-";;

quotient typedef EMPTYDICT = "{}";;

quotient typedef SCHEDULE_VALUE = "reboot" | "yearly" | "annually" | "monthly"
                       | "weekly" | "daily" | "midnight" | "hourly";;
quotient typedef SCHEDULE = "@" SCHEDULE_VALUE;;

quotient typedef USER = (UPPERCASE | LOWERCASE | DIGIT)+;;

quotient typedef TIME = NUMBER INDENT_REQ NUMBER INDENT_REQ NUMBER INDENT_REQ RANGE INDENT_REQ RANGE;;

quotient typedef SHELLCOMMAND_CHAR = LOWERCASE | UPPERCASE | DIGIT | "_" | "/" | "|"  | "." ;;
quotient typedef SC_CHAR_OR_SPACE = LOWERCASE | UPPERCASE | DIGIT | "_" | "/" | "|" | "." | " " ;;
quotient typedef SHELLCOMMAND = (SHELLCOMMAND_CHAR (SC_CHAR_OR_SPACE)* SHELLCOMMAND_CHAR) | SHELLCOMMAND_CHAR;;

quotient typedef SHELLVAR_CHAR = LOWERCASE | UPPERCASE | DIGIT | "_";; 
quotient typedef SHELLVAR_NAME = SHELLVAR_CHAR+;;
quotient typedef SHELLVALUE_CHAR = LOWERCASE | UPPERCASE | DIGIT | "_" | "/" | "|" | ".";;
quotient typedef SHELLVALUE_NAME = SHELLVALUE_CHAR+;;

quotient typedef SHELLVAR = SHELLVAR_NAME "=" SHELLVALUE_NAME "\n";;
quotient typedef COMMENTLINE = COMMENT "\n";;
quotient typedef ENTRY = INDENT (PREFIX | . ) (TIME | SCHEDULE) INDENT_REQ USER INDENT_REQ SHELLCOMMAND "\n";;
quotient typedef CRON = ( "\n" | SHELLVAR | COMMENTLINE | ENTRY)*;;

quotient typedef PREFIX_DICT = "{\"prefix\"=" ("true" | "false") "}";;
quotient typedef TIME_DICT = "{\"minute\"=" NUMBER ",\"ws1\"=" INDENT_REQ ",\"hour\"=" NUMBER 
  ",\"dayofmonth\"=" NUMBER ",\"month\"=" RANGE ",\"dayofweek\"=" RANGE "}";;
quotient typedef SCHEDULE_DICT = "{\"schedule\"=\"" SCHEDULE_VALUE "\"}";;
quotient typedef ENTRY_DICT = PREFIX_DICT "," (TIME_DICT | SCHEDULE_DICT)
  "\",\"user\"=\"" USER "\",\"command\"=\"" SHELLCOMMAND "\"}";;
quotient typedef SHELL_DICT = "{\"varname\"=\"" SHELLVAR_NAME "\",\"value\"=\"" SHELLVALUE_NAME "\"}";;
quotient typedef CRON_DICT = ((EMPTYDICT | SHELL_DICT | COMMENT_DICT | ENTRY_DICT) "\n")*;;

quotient time_lens = [TIME_DICT <=> TIME {}]
quotient entry_lens = [ENTRY_DICT <=> ENTRY {}]
quotient cron_lens = [CRON_DICT <=> CRON {}]
