#use "base.decls"

let NAME = \UPPERCASE (\LOWERCASE)* in
let NUMBER = (\DIGIT)* in
let STREETTYPE = Road + Avenue + Street + Boulevard in
let CARDINALDIRECTION = North + South + East + West in
let STREETNAME = \NAME (' ' \NAME)* ((' ' \STREETTYPE) + .) (. + (' ' \CARDINALDIRECTION)) in
let ZIP = \DIGIT \DIGIT \DIGIT \DIGIT \DIGIT in
let STATE = \UPPERCASE \UPPERCASE in

cvs_to_letter = [(\NAME , \NAME , \STATE , \ZIP , \NAME , \NUMBER , \STREETNAME \n)* <=>
		 (\NAME ' ' \NAME \n \NUMBER ' ' \STREETNAME \n \NAME , ' ' \STATE ' ' ' ' \ZIP \n \n)*
{"Miltner,Anders,NJ,08544,Princeton,88,College Road West
Miltner,Susan,CA,94904,Greenbrae,610,Via Casitas
" <-> 
"Anders Miltner
88 College Road West
Princeton, NJ  08544

Susan Miltner
610 Via Casitas
Greenbrae, CA  94904

"}]
