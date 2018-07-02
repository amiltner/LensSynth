#use "base.decls"

typedef TEXT_FIELD_CHAR = (UPPERCASE | LOWERCASE | DIGIT | " " | "\n" | "\t");;
typedef TEXT_FIELD = TEXT_FIELD_CHAR*;;
typedef NONEMPTY_TEXT_FIELD = TEXT_FIELD_CHAR+;;

typedef NUMERIC_FIELD = NUMBER;;

typedef TITLE_LEGACY = "<Field Id=1>" TEXT_FIELD "</Field>";;
typedef TITLE_MODERN = "\"title\"=\"" NONEMPTY_TEXT_FIELD "\"";;
typedef TITLE_MODERN_FIELD = "" | (TITLE_MODERN ",");;
test TITLE_MODERN matches "\"title\"=\"title\"";;
title_converter = [TITLE_LEGACY <=> TITLE_MODERN_FIELD {}]

typedef TYPE_LEGACY = "<Field Id=2>" TEXT_FIELD "</Field>";;
typedef TYPE_MODERN = "\"type\"=\"" NONEMPTY_TEXT_FIELD "\"";;
typedef TYPE_MODERN_FIELD = "" | (TYPE_MODERN ",");;
type_converter = [TYPE_LEGACY <=> TYPE_MODERN_FIELD {"<Field Id=2>bug</Field>" <-> "\"type\"=\"bug\","}]

typedef ID_LEGACY = "<Field Id=3>" NUMERIC_FIELD "</Field>";;
typedef ID_MODERN = "\"id\"=\"" NUMERIC_FIELD "\"";;

typedef DESCRIPTION_LEGACY = "<Field Id=4>" TEXT_FIELD "</Field>";;
typedef DESCRIPTION_MODERN = "\"description\"=\"" NONEMPTY_TEXT_FIELD "\"";;
typedef DESCRIPTION_MODERN_FIELD = ("" | (DESCRIPTION_MODERN ","));;
description_converter = [DESCRIPTION_LEGACY <=> DESCRIPTION_MODERN_FIELD {}]

typedef WORKITEM_LEGACY =
"<WorkItem>"
  TITLE_LEGACY TYPE_LEGACY ID_LEGACY DESCRIPTION_LEGACY
"</WorkItem>";;
test WORKITEM_LEGACY matches "<WorkItem><Field Id=1></Field><Field Id=2></Field><Field Id=3>1</Field><Field Id=4></Field></WorkItem>";;

typedef WORKITEM_MODERN =
  "{"
    DESCRIPTION_MODERN_FIELD
    (ID_MODERN ",")
    (TITLE_MODERN_FIELD)
    (TYPE_MODERN_FIELD)
  "}";;

legacy_converter = [WORKITEM_LEGACY <=> WORKITEM_MODERN
{
"<WorkItem><Field Id=1></Field><Field Id=2></Field><Field Id=3>1</Field><Field Id=4></Field></WorkItem>"
<->
"{\"id\"=\"1\",}",
"<WorkItem><Field Id=1>title</Field><Field Id=2></Field><Field Id=3>1</Field><Field Id=4></Field></WorkItem>"
<->
"{\"id\"=\"1\",\"title\"=\"title\",}",
"<WorkItem><Field Id=1>title</Field><Field Id=2>bug</Field><Field Id=3>1</Field><Field Id=4></Field></WorkItem>"
<->
"{\"id\"=\"1\",\"title\"=\"title\",\"type\"=\"bug\",}",
"<WorkItem><Field Id=1>title</Field><Field Id=2>bug</Field><Field Id=3>1</Field><Field Id=4>this is a bad bug</Field></WorkItem>"
<->
"{\"description\"=\"this is a bad bug\",\"id\"=\"1\",\"title\"=\"title\",\"type\"=\"bug\",}",
"<WorkItem><Field Id=1></Field><Field Id=2>bug</Field><Field Id=3>1</Field><Field Id=4>this is a bad bug</Field></WorkItem>"
<->
"{\"description\"=\"this is a bad bug\",\"id\"=\"1\",\"type\"=\"bug\",}",
"<WorkItem><Field Id=1>title</Field><Field Id=2></Field><Field Id=3>1</Field><Field Id=4>this is a bad bug</Field></WorkItem>"
<->
"{\"description\"=\"this is a bad bug\",\"id\"=\"1\",\"title\"=\"title\",}",
"<WorkItem><Field Id=1></Field><Field Id=2></Field><Field Id=3>1</Field><Field Id=4>this is a bad bug</Field></WorkItem>"
<->
"{\"description\"=\"this is a bad bug\",\"id\"=\"1\",}",
"<WorkItem><Field Id=1></Field><Field Id=2>bug</Field><Field Id=3>1</Field><Field Id=4></Field></WorkItem>"
<->
"{\"id\"=\"1\",\"type\"=\"bug\",}"
}]

