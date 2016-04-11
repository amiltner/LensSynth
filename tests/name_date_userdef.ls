#use "base.decls"

let NAME = \UPPERCASE (\LOWERCASE)* in
let DATE = (1+2+3+4+5+6+7+8+9+10+11+12) / (0+1+2) (0+1+2+3+4+5+6+7+8+9) / (0+1+2+3+4+5+6+7+8+9) (0+1+2+3+4+5+6+7+8+9) (0+1+2+3+4+5+6+7+8+9) (0+1+2+3+4+5+6+7+8+9) in

swap = [(((\NAME + (\NAME ' ' \NAME))): ' ' (\DATE))\n* <=> ((\DATE), ' ' ((\NAME , ' ' \NAME) + \NAME))\n*
{ "Anders Miltner: 5/10/1991
" <-> "5/10/1991, Miltner, Anders
"  }]
