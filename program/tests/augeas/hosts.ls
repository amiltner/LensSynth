#use "base.decls"
#use "util.decls"

typedef IPADDR = NUMBER "." NUMBER "." NUMBER "." NUMBER;;
typedef HOSTNAME = WORD;;
typedef HOSTSLINE = IPADDR WSP HOSTNAME (WSP HOSTNAME | WSP COMMENT)*;;
typedef HOSTS = ((HOSTSLINE | . | COMMENT) "\n")*;;

typedef IPADDR_DICT = "{\"ipaddr\"=\"" IPADDR "\"}";;
typedef HOST_DICT = "{\"name\"=\"" HOSTNAME ",\"ws\"=\"" WSP "\"}";;
typedef COMMENT_DICT = "{\"comment\"=\"" COMMENT ",\"ws\"=\"" WSP "\"}";;
typedef HOSTLINE_DICT = "{\"ipaddrs\"=\"" IPADDR_DICT ",\"hosts\"=" (HOST_DICT | COMMENT_DICT)* "}";;
typedef HOSTS_DICT = ((HOSTLINE_DICT | EMPTYDICT | COMMENT_DICT) "\n")*;;

hosts_lens = [HOSTS <=> HOSTS_DICT {}]
