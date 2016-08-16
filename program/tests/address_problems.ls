#use "base.decls"

typedef NAME = UPPERCASE (LOWERCASE)* ;;

typedef NUMBER = DIGIT (DIGIT)* ;;

typedef STREETTYPE = "Road" | "Avenue" | "Street" | "Boulevard""" ;;

typedef CARDINALDIRECTION = "North" | "South" | "East" | "West" ;;

typedef STREETNAME = NAME (" " NAME)* ((" " STREETTYPE) | .)
			(. | (" " CARDINALDIRECTION)) ;;

typedef ZIP = DIGIT DIGIT DIGIT DIGIT DIGIT ;;

typedef STATE = UPPERCASE UPPERCASE ;;

typedef CSV_ADDRESS = (NAME "," NAME ","
	STATE "," ZIP "," NAME "," NUMBER "," STREETNAME "\n") ;;

typedef LETTER_ADDRESS = (NAME " " NAME "\n" NUMBER " " STREETNAME "\n"
			(UPPERCASE (LOWERCASE)*) ", " STATE " " " " ZIP) ;;

typedef CSV_ADDRESS_DATABASE = CSV_ADDRESS*;;
typedef LETTER_ADDRESS_LIST = . | LETTER_ADDRESS |
	(LETTER_ADDRESS "\n\n" LETTER_ADDRESS ("\n\n" LETTER_ADDRESS)*);;

csv_to_letter = [CSV_ADDRESS_DATABASE <=> LETTER_ADDRESS_LIST
{"Miltner,Anders,NJ,08544,Princeton,88,College Road West
Miltner,Susan,CA,94904,Greenbrae,610,Via Casitas
" <-> 
"Anders Miltner
88 College Road West
Princeton, NJ  08544

Susan Miltner
610 Via Casitas
Greenbrae, CA  94904"}]

test csv_to_letter {"Miltner,Anders,NJ,08544,Princeton,88,College Road West
Miltner,Susan,CA,94904,Greenbrae,610,Via Casitas
" <-> 
"Anders Miltner
88 College Road West
Princeton, NJ  08544

Susan Miltner
610 Via Casitas
Greenbrae, CA  94904"};;
