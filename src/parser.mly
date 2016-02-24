%{
open Lang

exception Internal_error of string

%}

%token <string> LID   (* lowercase identifiers *)
%token <string> UID   (* uppercase identifiers *)
%token <int> PROJ     (* tuple projection *)

(* integer constants translated to O, S(O), S(S(O)), etc. *)
%token <int> INT

%token LET        (* let *)
%token IN         (* in *)

%token LEFTRIGHTFATARR (* <=> *)
%token LEFTRIGHTARR    (* <-> *)
%token COMMA      (* , *)
%token STAR       (* * *)
%token PIPE       (* | *)
%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token LBRACE     (* { *)
%token RBRACE     (* } *)
%token EQ         (* = *)
%token DOT        (* . *)

%token EOF

%start <Lang.synth_problem> synth_problem

%%

synth_problem:
  | c=context r1=regex LEFTRIGHTFATARR r2=regex es=examples EOF
        { (c, r1,r2,es) }

(***** Context {{{ *****)

context:
  | (* empty *)
    { [] }
  | d=decl c=context
    { d::c }

decl:
  | LET u=UID EQ r=regex IN
    { (u,r) }

(***** }}} *****)


(***** Regexes {{{ *****)

regex:
  | s=LID { RegExBase s }
  | r1=regex r2=regex { RegExConcat (r1,r2) }
  | r=regex STAR { RegExStar r }
  | r1=regex PIPE r2=regex { RegExOr (r1,r2) }
  | LPAREN r=regex RPAREN { r }
  | u=UID { RegExUserDefined u }
  | DOT { RegExBase "" }

(***** }}} *****)

(***** Examples {{{ *****)

examples:
  | LBRACE RBRACE
    { [] }
  | LBRACE exs=examples_inner RBRACE
    { exs }

examples_inner:
  | ex=example COMMA exs=examples_inner
    { ex::exs }
  | ex=example
    { [ex] }

example:
  | s1=LID LEFTRIGHTARR s2=LID
    { (s1,s2) }

(***** }}} *****)
