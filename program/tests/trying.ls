typedef A = (("aa" | "c") "b")*;;

typedef C = A | "c";;

ctoc = [(("aa" | "c") "b")* <=> (("aa" | "c") "b")* {}]

