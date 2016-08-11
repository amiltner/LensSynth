#use "base.decls"
typedef NAME = UPPERCASE (LOWERCASE)*;;

typedef LASTCOMMASTART = NAME "," (WSP NAME)*;;

typedef STARTTOEND = (NAME WSP)* NAME;;

typedef BIBTEXAUTHORLIST = . | (LASTCOMMASTART (WSP "and" WSP LASTCOMMASTART)*);;
typedef BIBTEXAUTHORINFO = "author={" BIBTEXAUTHORLIST "}";;

typedef AUTAG = "au - ";;
typedef TITAG = "ti - ";;
typedef JOTAG = "jo - ";;

typedef TAGGEDAUTHORDEFNS = . | (AUTAG STARTTOEND (WSP "\n" WSP AUTAG STARTTOEND)*);;

typedef TITLE = NAME (WSP NAME)*;;
typedef BIBTEXTITLE = "title={" TITLE "}";;
typedef TAGGEDTITLE = TITAG TITLE;;

typedef JOURNAL = NAME (WSP NAME)*;;
typedef BIBTEXJOURNAL = "journal={" JOURNAL "}";;
typedef TAGGEDJOURNAL = JOTAG JOURNAL;;

typedef FULLBIBTEX = "{" ((BIBTEXJOURNAL | BIBTEXAUTHORINFO | BIBTEXTITLE)",")* "}";;
typedef FULLTAGS = . | ((TAGGEDAUTHORDEFNS | TAGGEDTITLE | TAGGEDJOURNAL)(("\n" (TAGGEDAUTHORDEFNS | TAGGEDTITLE | TAGGEDJOURNAL))*)) ;;

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

bibtext_to_tagged_tester= [FULLBIBTEX <=> FULLTAGS {
}]

(* OH GOD THE OUTPUT IS BAD *)
(* MAKE IT SO LENSES CAN TAKE IN FILES, NOT JUST RANDOM EXAMPLES *)
(* benchmark suite of lenses & tests that it's CORRECT, not bogus *)
(* test examples that AREN'T given in inputs *)
(* how do we generate a READABLE lens *)
(* pp make better *)
(* need more tests!!! *)
(* randomly sample, and choose some additional files *)
(* add in min, max, avg # of examples *)
(* scrap your boilerplate paper *)
   (* series of papers *)
   (* simon peyton jones *)
   (* library of combinators to traverse tree-like dss *)

(* look at boomerang lenses, are they comprehensible, if so why? *)
(* common subexpression elimination / common sublens identification *)
(* pretty print out with named sublenses as pieces, provide
   compositional appearance to user *)
(* find similar solutions, show its true we can transform more
   complicated things than in Sumit's work *)
(* powershell thing with system log files *)
(* sumit oopsla paper, provanof(?) guy *)
(* windows powershell(?!) *)

(* for next work... *) (* clean up the -OUTPUTkjkj *) (* MAKEukjkuu
LENSESuuuuujjjjjjjj jjjjkkk:qkj: *) (* Some weaker form of completeness? *) (*
complete w.r.t. subset of kleene axioms *) (* semiring axioms / semirings + xyz
*) (* better to have SOME statement we can have as a basis *) (* design not ad
hoc, principle to this dnf form architecture *) (* shows bound on scope *)

(* Whitespace / quotient lenses *)
   (* can we insert things *)
   (* grammar over whitespace *)
   (* READ THAT PAPER *)
   (* chapter in nate's thesis? *)
   (* chapter in old TA's thesis? *)
   (* lenses many papers in general *)
      (* 1-3 per week depending on business *)
      (* what ideas can i borrow / apply *)
      (* what do they FAIL at that maybe synthesis can
         help or not even synthesis *)
      (* find the man behind curtain in papers *)