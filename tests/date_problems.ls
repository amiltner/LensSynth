#use "base.decls"

let YEAR = \DIGIT \DIGIT \DIGIT \DIGIT in
let MONTH = \DIGIT + 11 + 12 in
let DAY = ((1 + 2 + 3) \DIGIT) + 31 in
let WEEKDAYS = sunday + monday + tuesday + wednesday + thursday + friday + saturday in

english_to_british = [\MONTH / \DAY / \YEAR <=> \DAY / \MONTH / \YEAR {}]
