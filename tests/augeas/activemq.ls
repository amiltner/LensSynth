#use "base.decls"

typedef ENV_VAR = (UPPERCASE | "_")*;;
typedef LOCALFOLDER = (LOWERCASE | UPPERCASE | "_")*;;
test LOCALFOLDER matches "my_username";;

typedef FOLDER = ( (LOCALFOLDER "/")* LOCALFOLDER (. | "/") )
		| ("${" ENV_VAR "}");;
test FOLDER matches "my_username";;
test FOLDER matches "${MY_HOME}";;

typedef ACTIVEMQ_DICT = "{ }\n{ \"ACTIVEMQ_HOME\" = \"" FOLDER
			"\" }\n{ \"ACTIVEMQ_BASE\" = \"" FOLDER
			"\" }";;
test ACTIVEMQ_DICT matches "{ }\n{ \"ACTIVEMQ_HOME\" = \"/usr/share/activemq\" }\n{ \"ACTIVEMQ_BASE\" = \"${ACTIVEMQ_HOME}\" }";;

typedef ACTIVEMQ_CONFIG = "ACTIVEMQ_HOME=" FOLDER "\nACTIVEMQ_BASE=" FOLDER "\n";;
test ACTIVEMQ_CONFIG matches "ACTIVEMQ_HOME=/usr/share/activemq\nACTIVEMQ_BASE=${ACTIVEMQ_HOME}\n";;

map_activemq = [ACTIVEMQ_DICT <=> ACTIVEMQ_CONFIG
{ "{ }\n{ \"ACTIVEMQ_HOME\" = \"/usr/share/activemq\" }\n{ \"ACTIVEMQ_BASE\" = \"${ACTIVEMQ_HOME}\" }"
<-> "ACTIVEMQ_HOME=/usr/share/activemq\nACTIVEMQ_BASE=${ACTIVEMQ_HOME}\n"}]
