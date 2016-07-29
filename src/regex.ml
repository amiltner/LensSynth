
type regex =
  | RegExEmpty
  | RegExBase of string
  | RegExConcat of regex * regex
  | RegExOr of regex * regex
  | RegExStar of regex
  | RegExVariable of string
  | RegExMapped of int
