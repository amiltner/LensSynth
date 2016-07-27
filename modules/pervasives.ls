Module Character {
  typedef Uppercase = [A-Z];;
  typedef Lowercase = [a-z];;
  typedef Char = \Uppercase + \Lowercase;;
  ls switch_case : Uppercase <-> Lowercase;;
}

Module Number {
  typedef Digit = [0-9];;
  typedef Natural = 0 + ([1-9] \Digit*);;
  typedef Integer = 0 + ((. | -)([1-9] \Digit*));;
  typedef Decimal = \Integer \. (\Digit*);;
  ls additive_inverse_integer : Integer <-> Integer { 0 <-> 0 ; 1 <-> -1 };;
  ls additive_inverse_decimal : Decimal <-> Decimal { 1 <-> -1 };;
}
