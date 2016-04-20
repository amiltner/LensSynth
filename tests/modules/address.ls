#using name.ls

Module Address using Character,Name,Numbers {
  typedef StreetType = \Street | \Boulevard | . | \Circle | \Avenue;;
  typedef StreetAddress = \Numbers \Name* \StreetType;;
  abstype City = \Name*;;
  abstype State = \Character.Uppercase \Character.Uppercase;;
  typedef Zip = \Digit \Digit \Digit \Digit \Digit;;
  typedef Letter_Address sharing =
    \Name.FullName \n
    \StreetAddress \n
    \City , ' ' \State ' '' ' \Zip;;
  typedef CSV_Address sharing =
    (. | \Name | (\Name \Name)) , \Name ,
      \Zip , \State , \City , \StreetAddress


  ls letter_csv_transform : \Letter_Address <-> \CSV_Address { x <-> y };;


  ls a . b <-> c . d

  a .  r . b <-> c . d
switch around a and b difficult to decide


  typedef CSV_Address_Database : (\CSV_Address \n)*;;
  typedef Letter_Address_List : . | \Letter_Address
    | (\Letter_Address \n \n \Letter_Address (\n \n \Letter_Address)*);;



  ls csv_db_letter_list : CSV_Address_Database <-> Letter_Address_List { use myaddressmapping.txt };;
}
