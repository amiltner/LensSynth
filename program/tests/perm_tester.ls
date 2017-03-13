typedef A = perm("a"*, "b" | "c", "x"* -> "xxx") with "BOOYAH"* "BAZINGAH";;

typedef B = perm("e" | "f", "y"* -> "yyy", "d"*) with "ALAKAZAM" "ALAKAZEE"*;;

x = [A <=> B {}]
