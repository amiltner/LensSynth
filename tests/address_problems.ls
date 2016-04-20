#use "base.decls"

typedef NAME sharing = \UPPERCASE (\LOWERCASE)* ;;

typedef NUMBER sharing = \DIGIT (\DIGIT)* ;;

typedef STREETTYPE sharing = Road + Avenue + Street + Boulevard ;;

typedef CARDINALDIRECTION sharing = North + South + East + West ;;

typedef STREETNAME sharing = \NAME (' ' \NAME)* ((' ' \STREETTYPE) + .) (. + (' ' \CARDINALDIRECTION)) ;;

typedef ZIP sharing = \DIGIT \DIGIT \DIGIT \DIGIT \DIGIT ;;

typedef STATE sharing = \UPPERCASE \UPPERCASE ;;

typedef CSV_ADDRESS sharing = (\NAME , \NAME , \STATE , \ZIP , \NAME , \NUMBER , \STREETNAME \n) ;;

typedef LETTER_ADDRESS sharing = (\NAME ' ' \NAME \n \NUMBER ' ' \STREETNAME \n (\UPPERCASE (\LOWERCASE)*) , ' ' \STATE ' ' ' ' \ZIP) ;;

typedef CSV_ADDRESS_DATABASE sharing = \CSV_ADDRESS*;;
typedef LETTER_ADDRESS_LIST sharing = . + \LETTER_ADDRESS + (\LETTER_ADDRESS \n \n \LETTER_ADDRESS (\n \n \LETTER_ADDRESS)*);;

csv_to_letter = [\CSV_ADDRESS_DATABASE <=> \LETTER_ADDRESS_LIST
{"Miltner,Anders,NJ,08544,Princeton,88,College Road West
Miltner,Susan,CA,94904,Greenbrae,610,Via Casitas
" <-> 
"Anders Miltner
88 College Road West
Princeton, NJ  08544

Susan Miltner
610 Via Casitas
Greenbrae, CA  94904"}]
