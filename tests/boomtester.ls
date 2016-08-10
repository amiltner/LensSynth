typedef A = (("aa" | "c") "b")*;;

typedef C = A | "c";;

ctoc = [C <=> C {}]