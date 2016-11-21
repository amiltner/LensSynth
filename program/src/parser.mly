%{
open String_utilities
open Lang
%}

%token <string> LID   (* lowercase identifiers *)
%token <string> UID   (* uppercase identifiers *)
%token <string> STR   (* string identifiers *)

(* integer constants translated to O, S(O), S(S(O)), etc. *)

%token TYPEDEF    (* typedef *)
%token ABSTRACT   (* abstract *)
%token TEST       (* test *)
%token MATCHES    (* matches *)
%token PERM       (* permutation *)
%token SEP        (* separator *)

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
%token BACKSLASH  (* \ *)
%token HYPHEN      (* - *)
%token LBRACKET   (* [ *)
%token RBRACKET   (* ] *)
%token SEMI       (* ; *)
%token ARROW      (* -> *)
%token EOF

%start <Lang.program> program

%%

program:
  | d=decl p=program
    { (d::p) }
  | EOF
    { [] }

decl:
  | d=quotient_defn
    { DeclQuotientRegexCreation d }
  | s=quotient_specification
    { DeclQuotientSynthesizeLens s }
  | TEST r=quotient_regex MATCHES s=str SEMI SEMI
    { DeclQuotientTestString (r,s) }
  | TEST n=LID exs=examples SEMI SEMI
    { DeclQuotientTestLens (n, exs) }

quotient_defn:
  | TYPEDEF u=UID EQ r=quotient_regex SEMI SEMI
    { (u, r, true) }
  | ABSTRACT u=UID EQ r=quotient_regex SEMI SEMI
    { (u, r, false) }

quotient_specification:
  | n=LID EQ LBRACKET q1=quotient_regex LEFTRIGHTFATARR q2=quotient_regex es=examples RBRACKET
    { (n, q1, q2, es) }

(***** Regexes {{{ *****)

regex:
  | r=base_regex_l0 {r}

quotient_regex_list:
  | LBRACKET RBRACKET { QuotientRegExPermute ([], RegExBase "") }
  | LBRACKET l=quotient_regex_list_internal SEP s=regex RBRACKET { QuotientRegExPermute (l,s) }

quotient_regex_list_internal:
  | r=quotient_regex { [r] }
  | r=quotient_regex SEMI l=quotient_regex_list_internal { r :: l }

quotient_regex:
  | r=quotient_regex_l0 {r}

quotient_regex_l0:
  | r1=quotient_regex_l1 PIPE r2=quotient_regex_l0 { QuotientRegExOr (r1, r2) }
  | r=quotient_regex_l1 { r }

quotient_regex_l1:
  | r1=quotient_regex_l2 r2=quotient_regex_l1 { QuotientRegExConcat (r1, r2) }
  | r=quotient_regex_l2 { r }

quotient_regex_l2:
  | r = quotient_regex_l2 STAR { QuotientRegExStar r }
  | r = quotient_regex_l2 PLUS { QuotientRegExConcat(r, QuotientRegExStar r) }
  | r = quotient_regex_l3 { r }

quotient_regex_l3:
  | PERM l=quotient_regex_list { l }
  | s = base { QuotientRegExBase s }
  | LBRACKET r=base_regex ARROW s=base RBRACKET { QuotientRegExMap (r, s) }
  | LPAREN r=quotient_regex_l0 RPAREN { r }
  | u = UID { QuotientRegExVariable u }

base_regex:
  | r=base_regex_l0 { r }

base_regex_l0:
  | r1=base_regex_l1 PIPE r2=base_regex_l0 { RegExOr (r1,r2) }
  | r=base_regex_l1 { r }

base_regex_l1:
  | r1=base_regex_l2 r2=base_regex_l1 { RegExConcat (r1,r2) }
  | r=base_regex_l2 { r }

base_regex_l2:
  | r=base_regex_l2 STAR { RegExStar r }
  | r=base_regex_l2 PLUS { RegExConcat(r, RegExStar r) }
  | r=base_regex_l3 { r }

base_regex_l3:
  | s=base { RegExBase s }
  | LPAREN r=base_regex_l0 RPAREN { r }
  | u=UID { RegExVariable u }


base:
  | s=str { s }
  | DOT { "" }
  | BACKSLASH { "\\" }
  | SLASH { "/" }
  | HYPHEN { "-" }

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
    { undelimit_string s }

(***** }}} *****)
