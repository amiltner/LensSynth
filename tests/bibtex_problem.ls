#use "base.decls"
typedef NAME = \UPPERCASE (\LOWERCASE)*;;

typedef LASTCOMMASTART sharing = \NAME, (\WSP \NAME)*;;

typedef STARTTOEND sharing = (\NAME \WSP)* \NAME;;

typedef BIBTEXAUTHORLIST sharing = . + (\LASTCOMMASTART (\WSP and \WSP \LASTCOMMASTART)*);;
typedef BIBTEXAUTHORINFO sharing = author = {\BIBTEXAUTHORLIST};;

typedef AUTAG sharing = au ' ' - ' ';;
typedef TITAG sharing = ti ' ' - ' ';;
typedef JOTAG sharing = jo ' ' - ' ';;

typedef TAGGEDAUTHORDEFNS sharing = . + (\AUTAG \STARTTOEND (\WSP \n \WSP \AUTAG \STARTTOEND)*);;

typedef TITLE sharing = \NAME (\WSP \NAME)*;;
typedef BIBTEXTITLE sharing = title = {\TITLE};;
typedef TAGGEDTITLE sharing = \TITAG \TITLE;;

typedef JOURNAL sharing = \NAME (\WSP \NAME)*;;
typedef BIBTEXJOURNAL sharing = journal = {\JOURNAL};;
typedef TAGGEDJOURNAL sharing = \JOTAG \JOURNAL;;

typedef FULLBIBTEX sharing = { ((\BIBTEXJOURNAL + \BIBTEXAUTHORINFO + \BIBTEXTITLE),)* };;
typedef FULLTAGS sharing = . + ((\TAGGEDAUTHORDEFNS + \TAGGEDTITLE + \TAGGEDJOURNAL)((\n (\TAGGEDAUTHORDEFNS + \TAGGEDTITLE + \TAGGEDJOURNAL))*)) ;;

bibtex_to_readable_au = [\BIBTEXAUTHORINFO <=> \TAGGEDAUTHORDEFNS
{"author={Foster, Nathan and Pierce, Benjamin and Bohannon, Aaron}" <->
"au - Nathan Foster 
 au - Benjamin Pierce 
 au - Aaron Bohannon"}]

bibtext_to_readable_title = [\BIBTEXTITLE <=> \TAGGEDTITLE
{"title={Boomerang Resourceful Lenses For String Data}" <->
 "ti - Boomerang Resourceful Lenses For String Data"}]

journal_to_readable_journal = [\BIBTEXJOURNAL <=> \TAGGEDJOURNAL
{"journal={Principals Of Programming Languages}" <->
 "jo - Principals Of Programming Languages"}]

bibtext_to_tagged_tester = [\FULLBIBTEX <=> \FULLTAGS {
"{author={Foster, Nathan and Pierce, Benjamin and Bohannon, Aaron},title={Boomerang Resourceful Lenses For String Data},journal={Principals Of Programming Languages},}" <->
"au - Nathan Foster 
 au - Benjamin Pierce 
 au - Aaron Bohannon
ti - Boomerang Resourceful Lenses For String Data
jo - Principals Of Programming Languages"}]
