#use "base.decls"


typedef FULL_ACCESS_CONFIG sharing = ((\FULLCONFIGACCESS + \CONFIG_COMMENT) \n)*;;
typedef FULL_ACCESS_PP sharing = ((\PPCONFIGACCESS + \READABLE_COMMENT) \n)*;;

typedef ACCESSTYPE sharing = "+" + "-";;

typedef FULLCONFIGACCESS sharing = \ACCESSTYPE " : " \USERNAMEHOSTGROUPCONFIGLIST ":" \ORIGINLIST;;

typedef PPCONFIGACCESS sharing = ("{ \"access\" = \"" \ACCESSTYPE "\"" \PRINTABLEUNHGCFGLIST \PPORIGINLIST " }");;

typedef STRING sharing = (\UPPERCASE + \LOWERCASE + \DIGIT + "\"" + ' ')*;;

typedef ORIGIN sharing = (\UPPERCASE + \LOWERCASE + \DIGIT + :)(\UPPERCASE + \LOWERCASE + \DIGIT + :)*;;
typedef PPORIGIN sharing = "{ \"origin\" = \"" \ORIGIN "\" }";;
typedef PPORIGINLIST sharing = (\n \PPORIGIN)*;;
typedef ORIGINLIST sharing = (' ' \ORIGIN)*;;
typedef IDENTIFIER sharing = (\UPPERCASE + \LOWERCASE + \DIGIT)(\UPPERCASE + \LOWERCASE + \DIGIT)*;;
typedef USERNAMEHOSTGROUPCONFIG sharing = \IDENTIFIER + (\IDENTIFIER "@" \IDENTIFIER) + ("@" \IDENTIFIER);;
typedef USERNAMEHOSTGROUPCONFIGLIST sharing = (\USERNAMEHOSTGROUPCONFIG ' ')*;;
typedef USERINFO sharing = "{ \"user\" = \"" \IDENTIFIER "\" }";;
typedef USERHOSTINFO sharing = "{ \"user\" = \"" \IDENTIFIER "\"\n	{ \"host\" = \"" \IDENTIFIER "\" } }";;
typedef NETGROUPINFO sharing = "{ \"netgroup\" = \"" \IDENTIFIER "\" }";;
typedef PRINTABLEUNHGCFG sharing = \USERINFO + \USERHOSTINFO + \NETGROUPINFO;;
typedef PRINTABLEUNHGCFGLIST sharing = (\n \PRINTABLEUNHGCFG)*;;

typedef CONFIG_COMMENT sharing = "# " \STRING;;

typedef DELIMITED_STRING sharing = (\UPPERCASE + \LOWERCASE + \DIGIT + "\\\"" + ' ')*;;

typedef READABLE_COMMENT sharing = "\"#comment\" = \"" \DELIMITED_STRING "\"";;

typedef CONFIG_STRING sharing = (\CONFIG_COMMENT \n)*;;

typedef READABLE_STRING sharing = (\READABLE_COMMENT \n)*;;

typedef IDENTIFIER sharing = (\UPPERCASE + \LOWERCASE + \DIGIT) (\UPPERCASE + \LOWERCASE + \DIGIT)*;;

extract_comment = [\CONFIG_COMMENT <=> \READABLE_COMMENT
{"# here is a comment with \"quotes\"" <-> "\"#comment\" = \"here is a comment with \\\"quotes\\\"\""}]

extract_netgroup = [("@" \IDENTIFIER) <=> \NETGROUPINFO
{
"@princeton" <-> "{ \"netgroup\" = \"princeton\" }"
}]

extract_origin_list = [\ORIGINLIST <=> \PPORIGINLIST
{" cron crond :0" <-> "\n{ \"origin\" = \"cron\" }\n{ \"origin\" = \"crond\" }\n{ \"origin\" = \":0\" }"}]

extract_userdata = [\USERNAMEHOSTGROUPCONFIG <=> \PRINTABLEUNHGCFG
{"anders" <-> "{ \"user\" = \"anders\" }",
"anders@princeton" <-> "{ \"user\" = \"anders\"\n	{ \"host\" = \"princeton\" } }",
"@princeton" <-> "{ \"netgroup\" = \"princeton\" }"
}]

extract_access = [\FULLCONFIGACCESS <=> \PPCONFIGACCESS
{"+ : anders anders@princeton @princeton : cron crond :0" <-> "{ \"access\" = \"+\"\n{ \"user\" = \"anders\" }\n{ \"user\" = \"anders\"\n	{ \"host\" = \"princeton\" } }\n{ \"netgroup\" = \"princeton\" }\n{ \"origin\" = \"cron\" }\n{ \"origin\" = \"crond\" }\n{ \"origin\" = \":0\" } }"}]

extract_config = [\FULL_ACCESS_CONFIG <=> \FULL_ACCESS_PP
{"+ : anders anders@princeton @princeton : cron crond :0\n# here is a comment with \"quotes\"\n" <-> "{ \"access\" = \"+\"\n{ \"user\" = \"anders\" }\n{ \"user\" = \"anders\"\n	{ \"host\" = \"princeton\" } }\n{ \"netgroup\" = \"princeton\" }\n{ \"origin\" = \"cron\" }\n{ \"origin\" = \"crond\" }\n{ \"origin\" = \":0\" } }\n\"#comment\" = \"here is a comment with \\\"quotes\\\"\"\n"}]
