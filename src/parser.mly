%{
open Lang
open Str

exception Internal_error of string

%}

%token <string> LID   (* lowercase identifiers *)
%token <string> UID   (* uppercase identifiers *)
%token <string> STR   (* string identifiers *)
%token <int> PROJ     (* tuple projection *)

(* integer constants translated to O, S(O), S(S(O)), etc. *)
%token <string> INT

%token LET        (* let *)
%token TYPEDEF    (* typedef *)
%token IN         (* in *)
%token SHARING    (* sharing *)

%token LEFTRIGHTFATARR (* <=> *)
%token LEFTRIGHTARR    (* <-> *)
%token COMMA      (* , *)
%token STAR       (* * *)
%token PIPE       (* | *)
%token PLUS       (* + *)
%token LPAREN     (* ( *)
%token RPAREN     (* ) *)
%token LBRACE     (* { *)
%token RBRACE     (* } *)
%token EQ         (* = *)
%token DOT        (* . *)
%token SLASH      (* / *)
%token SPACE      (* ' ' *)
%token BACKSLASH  (* \ *)
%token COLON      (* : *)
%token LT         (* < *)
%token GT         (* > *)
%token HYPHEN      (* - *)
%token NEWLINE    (* \\n *)
%token LBRACKET   (* [ *)
%token RBRACKET   (* ] *)
%token SEMI       (* ; *)
%token HASH       (* # *)

%token EOF

%start <Lang.synth_problems> synth_problems

%%

synth_problems:
  | c=context ss=specifications EOF
        { (c, ss) }

(***** Context {{{ *****)

context:
  | (* empty *)
    { [] }
  | d=decl c=context
    { d::c }

decl:
  | TYPEDEF u=UID EQ r=regex SEMI SEMI
    { (u,r,false) }
  | TYPEDEF u=UID SHARING EQ r=regex SEMI SEMI
    { (u,r,true) }

(***** }}} *****)


(***** Specifications {{{ *****)

specifications:
  | (* empty *)
    { [] }
  | s=specification ss=specifications
    { s::ss }

specification:
  |  n=LID EQ LBRACKET r1=regex LEFTRIGHTFATARR r2=regex es=examples RBRACKET
    { (n,r1,r2,es) }


(***** }}} *****)

(***** Regexes {{{ *****)

regex:
  | s=base { RegExBase s }
  | r1=regex r2=regex { RegExConcat (r1,r2) }
  | r=regex STAR { RegExStar r }
  | r1=regex PLUS r2=regex { RegExOr (r1,r2) }
  | LPAREN r=regex RPAREN { r }
  | BACKSLASH u=UID { RegExUserDefined u }

base:
  | s=LID { s }
  | s=UID { s }
  | s=str { s }
  | DOT { "" }
  | HASH { "#" }
  | BACKSLASH { "\\" }
  | SLASH { "/" }
  | SPACE { " " }
  | s=INT { s }
  | COMMA { "," }
  | COLON { ":" }
  | LT    { "<" }
  | GT    { ">" }
  | HYPHEN { "-" }
  | NEWLINE { "\n" }
  | EQ { "=" }
  | LBRACE { "{" }
  | RBRACE { "}" }

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
  | s1=str LEFTRIGHTARR s2=str
    { (s1,s2) }

str:
  | s=STR
    { Str.global_replace (Str.regexp "\\\\\\\\") "\\\\"
      (Str.global_replace (Str.regexp "\\\\n") "\n"
      (Str.global_replace (Str.regexp "\\\\\"") "\"" s)) }

(***** }}} *****)
