typedef LOWERCASE = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ;;
typedef UPPERCASE = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ;;
typedef DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;;
typedef NUMBER = DIGIT+;;


typedef QUOTELESS_STRING = (UPPERCASE | LOWERCASE | DIGIT | " " | "'" | "_" | ":" | "/" | "-" | "." |  "=" | "+")*;;

typedef STRING_COMPONENT = UPPERCASE | LOWERCASE | DIGIT | "\"" | " " | "'" | "_" | ":" | "/" | "-" | "." | "=" | "+";;
typedef STRING = STRING_COMPONENT*;;
typedef COMMENT = "# " STRING;;
typedef COMMENT_DICT = "{\"#comment\"=\"" DELIMITED_STRING "\"}";;

typedef DELIMITED_STRING = (UPPERCASE | LOWERCASE | DIGIT | "\\\"" | " " | "'" | "_" | ":" | "/" | "-" | "." | "=" | "+")*;;

string_to_delimited = [STRING <=> DELIMITED_STRING {}]

typedef INDENT = [ (" " | "\t")* -> " " ];;
typedef INDENT_REQ = [ (" " | "\t")+ -> " "];;
typedef ALPHANUM = (UPPERCASE | LOWERCASE | DIGIT)+;;
typedef RANGE = (ALPHANUM "-" ALPHANUM | ALPHANUM );;
typedef PREFIX = "-";;

typedef EMPTYDICT = "{}";;

typedef SCHEDULE_VALUE = "reboot" | "yearly" | "annually" | "monthly"
                       | "weekly" | "daily" | "midnight" | "hourly";;
typedef SCHEDULE = "@" SCHEDULE_VALUE;;

typedef USER = (UPPERCASE | LOWERCASE | DIGIT)+;;

typedef TIME = NUMBER INDENT_REQ NUMBER INDENT_REQ NUMBER INDENT_REQ RANGE INDENT_REQ RANGE;;

typedef SHELLCOMMAND_CHAR = LOWERCASE | UPPERCASE | DIGIT | "_" | "/" | "|"  | "." ;;
typedef SC_CHAR_OR_SPACE = LOWERCASE | UPPERCASE | DIGIT | "_" | "/" | "|" | "." | " " ;;
typedef SHELLCOMMAND = (SHELLCOMMAND_CHAR (SC_CHAR_OR_SPACE)* SHELLCOMMAND_CHAR) | SHELLCOMMAND_CHAR;;

typedef SHELLVAR_CHAR = LOWERCASE | UPPERCASE | DIGIT | "_";; 
typedef SHELLVAR_NAME = SHELLVAR_CHAR+;;
typedef SHELLVALUE_CHAR = LOWERCASE | UPPERCASE | DIGIT | "_" | "/" | "|" | ".";;
typedef SHELLVALUE_NAME = SHELLVALUE_CHAR+;;

typedef SHELLVAR = SHELLVAR_NAME "=" SHELLVALUE_NAME "\n";;
typedef COMMENTLINE = COMMENT "\n";;
typedef ENTRY = INDENT (PREFIX | . ) (TIME | SCHEDULE) INDENT_REQ USER INDENT_REQ SHELLCOMMAND "\n";;
typedef CRON = ( "\n" | SHELLVAR | COMMENTLINE | ENTRY)*;;

typedef PREFIX_DICT = "{\"prefix\"=" ("true" | "false") "}";;
typedef TIME_DICT = "{\"minute\"=" NUMBER ",\"hour\"=" NUMBER 
  ",\"dayofmonth\"=" NUMBER ",\"month\"=" RANGE ",\"dayofweek\"=" RANGE "}";;
typedef SCHEDULE_DICT = "{\"schedule\"=\"" SCHEDULE_VALUE "\"}";;
typedef ENTRY_DICT = PREFIX_DICT "," (TIME_DICT | SCHEDULE_DICT)
  "\",\"user\"=\"" USER "\",\"command\"=\"" SHELLCOMMAND "\"}";;
typedef SHELL_DICT = "{\"varname\"=\"" SHELLVAR_NAME "\",\"value\"=\"" SHELLVALUE_NAME "\"}";;
typedef CRON_DICT = ((EMPTYDICT | SHELL_DICT | COMMENT_DICT | ENTRY_DICT) "\n")*;;

time_lens = [TIME_DICT <=> TIME {}]
entry_lens = [ENTRY_DICT <=> ENTRY {}]
cron_lens = [CRON_DICT <=> CRON {}]

test time_lens { "{\"minute\"=45,\"hour\"=6,\"dayofmonth\"=3,\"month\"=8,\"dayofweek\"=0}" <-> "45    6   3  8 0" };;

