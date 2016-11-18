quotient typedef LOWERCASE = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ;;
quotient typedef UPPERCASE = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ;;
quotient typedef DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;;
quotient typedef WSP = (" " | "\n" | "\t" )+;;

quotient typedef NAME = UPPERCASE (LOWERCASE)*;;

quotient typedef LASTCOMMASTART = NAME "," (WSP NAME)*;;

quotient typedef STARTTOEND = (NAME WSP)* NAME;;

quotient typedef BIBTEXAUTHORLIST = LASTCOMMASTART (" and " LASTCOMMASTART)*;;
quotient typedef BIBTEXAUTHORINFO = "author={" BIBTEXAUTHORLIST "}";;

quotient typedef AUTAG = "au - ";;
quotient typedef TITAG = "ti - ";;
quotient typedef JOTAG = "jo - ";;

quotient typedef TAGGEDAUTHORDEFNS = AUTAG STARTTOEND ("\n " AUTAG STARTTOEND)*;;

quotient typedef TITLE = NAME (WSP NAME)*;;
quotient typedef BIBTEXTITLE = "title={" TITLE "}";;
quotient typedef TAGGEDTITLE = TITAG TITLE;;

quotient typedef JOURNAL = NAME (WSP NAME)*;;
quotient typedef BIBTEXJOURNAL = "journal={" JOURNAL "}";;
quotient typedef TAGGEDJOURNAL = JOTAG JOURNAL;;

quotient typedef FULLBIBTEX = "{" (perm [ BIBTEXJOURNAL "," ; BIBTEXAUTHORINFO "," ; BIBTEXTITLE "," ] sep ("")) "}";;
quotient typedef FULLTAGS = perm [ TAGGEDAUTHORDEFNS ; TAGGEDTITLE ; TAGGEDJOURNAL ] sep ("\n") ;;

quotient bibtex_to_readable_au = [BIBTEXAUTHORINFO <=> TAGGEDAUTHORDEFNS
{"author={Foster, Nathan and Pierce, Benjamin and Bohannon, Aaron}" <->
"au - Nathan Foster
 au - Benjamin Pierce
 au - Aaron Bohannon"}]

quotient bibtext_to_readable_title = [BIBTEXTITLE <=> TAGGEDTITLE
{"title={Boomerang Resourceful Lenses For String Data}" <->
 "ti - Boomerang Resourceful Lenses For String Data"}]

quotient journal_to_readable_journal = [BIBTEXJOURNAL <=> TAGGEDJOURNAL
{"journal={Principals Of Programming Languages}" <->
 "jo - Principals Of Programming Languages"}]

quotient bibtext_to_tagged_tester = [FULLBIBTEX <=> FULLTAGS {
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
