typedef LOWERCASE = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ;;
typedef UPPERCASE = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" ;;
to_upper_or_lower = [LOWERCASE <=> UPPERCASE {}]
typedef DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;;
typedef NUMBER = DIGIT+;;
typedef WSP = (" " | "\n" | "\t")*;;
typedef NONEMPTY_WSP = (" " | "\n" | "\t")+;;
typedef SAMELINE_WSP_CHAR = " " | "\t" | "\\\n";;
typedef SAMELINE_WSP = SAMELINE_WSP_CHAR*;;
typedef NONEMPTY_SAMELINE_WSP = SAMELINE_WSP_CHAR+;;
