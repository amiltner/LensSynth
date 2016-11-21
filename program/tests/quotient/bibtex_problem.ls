typedef LOWERCASE = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ;;
typedef UPPERCASE = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ;;
typedef DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;;
typedef WSP = (" " | "\n" | "\t" )+;;

typedef NAME = UPPERCASE (LOWERCASE)*;;

typedef LASTCOMMASTART = NAME "," (WSP NAME)*;;

typedef STARTTOEND = (NAME WSP)* NAME;;

typedef BIBTEXAUTHORLIST = LASTCOMMASTART (" and " LASTCOMMASTART)*;;
typedef BIBTEXAUTHORINFO = "author={" BIBTEXAUTHORLIST "}";;

typedef AUTAG = "au - ";;
typedef TITAG = "ti - ";;
typedef JOTAG = "jo - ";;

typedef TAGGEDAUTHORDEFNS = AUTAG STARTTOEND ("\n " AUTAG STARTTOEND)*;;

typedef TITLE = NAME (WSP NAME)*;;
typedef BIBTEXTITLE = "title={" TITLE "}";;
typedef TAGGEDTITLE = TITAG TITLE;;

typedef JOURNAL = NAME (WSP NAME)*;;
typedef BIBTEXJOURNAL = "journal={" JOURNAL "}";;
typedef TAGGEDJOURNAL = JOTAG JOURNAL;;

typedef FULLBIBTEX = "{" (perm [ BIBTEXJOURNAL "," ; BIBTEXAUTHORINFO "," ; BIBTEXTITLE "," sep "" ]) "}";;
typedef FULLTAGS = perm [ TAGGEDAUTHORDEFNS ; TAGGEDTITLE ; TAGGEDJOURNAL sep "\n" ] ;;

bibtex_to_readable_au = [BIBTEXAUTHORINFO <=> TAGGEDAUTHORDEFNS
{"author={Foster, Nathan and Pierce, Benjamin and Bohannon, Aaron}" <->
"au - Nathan Foster
 au - Benjamin Pierce
 au - Aaron Bohannon"}]

bibtext_to_readable_title = [BIBTEXTITLE <=> TAGGEDTITLE
{"title={Boomerang Resourceful Lenses For String Data}" <->
 "ti - Boomerang Resourceful Lenses For String Data"}]

journal_to_readable_journal = [BIBTEXJOURNAL <=> TAGGEDJOURNAL
{"journal={Principals Of Programming Languages}" <->
 "jo - Principals Of Programming Languages"}]

bibtext_to_tagged_tester = [FULLBIBTEX <=> FULLTAGS {
"{author={Foster, Nathan and Pierce, Benjamin and Bohannon, Aaron},title={Boomerang Resourceful Lenses For String Data},journal={Principals Of Programming Languages},}"
<->
"au - Nathan Foster
 au - Benjamin Pierce
 au - Aaron Bohannon
ti - Boomerang Resourceful Lenses For String Data
jo - Principals Of Programming Languages"
}]

(*quotient test bibtext_to_tagged_tester
{"{author={Foster, Nathan and Pierce, Benjamin and Bohannon, Aaron},}"
<->
"au - Nathan Foster
 au - Benjamin Pierce
 au - Aaron Bohannon",

"{title={Boomerang Resourceful Lenses For String Data},}"
<->
"ti - Boomerang Resourceful Lenses For String Data",

"{journal={Principals Of Programming Languages},}"
<->
"jo - Principals Of Programming Languages"
};;*)
