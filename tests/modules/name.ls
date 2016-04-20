Module Name using Character.* {
  typedef Name = \Uppercase (\Lowercase)*;;
  typedef LastCommaFirstMiddle = \Name , ' ' \Name (. | (' ' \Name));;
  typedef FirstMiddleLast = \Name (. | ' ' \Name) ' ' \Name;;
  ls fml_lcfm : LastCommaFirstMiddle <-> FirstMiddleLast;;
}
