#use "base.decls"
#use "util.decls"

typedef COMMA_SEPARATED_NONEMPTY_WORD_LIST = (WORD ",")*WORD;;
typedef NAME_VALUE_CONF = WORD":"COMMA_SEPARATED_NONEMPTY_WORD_LIST;;
typedef ALIASCONF = ((NAME_VALUE_CONF | . | COMMENT) "\n")*;;

typedef VALUE_DICT = "{\"value\"=\""WORD"\"}";;
typedef NAME_VALUE_DICT = "{\"name\"=\""WORD"\""VALUE_DICT+"}";;
typedef ALIAS_DICT = (NAME_VALUE_DICT | EMPTYDICT | COMMENT_DICT)*;;

afs_cellalias = [ALIAS_DICT <=> ALIASCONF {}]
