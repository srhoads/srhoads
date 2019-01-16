
# source("global.R")
library(tidyverse)

# used to be global.R:
minuscontainsregex <- paste0(
  c("julysttojun", "excludesallmega", "yearsingrademean", "basesalary", "totalpay", "alarymean",
    "levelname", "lvlname", "externalavailability","externalweight", "phcgenderp", "gendertotal",
    "shfname", "hqname", "employernam", "loyernam", "reqname", "namerejection", "namelevelmgmt", "levelmgmt",
    "underutilized", "averagegendertenure", "annualsalary", "gendersalary", "numberofmalegender", "sourace", 
    'actionname', 'usbased', 'pleasehere', 'unkown', "programnam", "nameofsubgroup",
    "jbuser", "mngr", "college", "company", "source", "county", "genderaverage",
    "client", "nocolumn", "transferred", "decisionmaker", "nycb", "groupsname",
    "racemale", "futurenam", "weborad", "raceorlatino", "flowby", "gendercompensation",
    "visor", "agency", "surveynam", "testistered", "tomet", "movedtodest", "namereviewreviewednotselected",
    "tocost", "toaap", "rptto", "completingform", "areanam", "vendornam", 
    "replacement", "paybas", "media", "namehired", "bgname", "reqfilled", 
    "replaced", "hrgname", "yrrover", "product", "metname", "denotesrequired", 
    "operior", "class", "costctr", "grename", "aofunc", "costcode", 
    "unoname", "shouldbenamed", "lynamed", "claimant", "batterynam", "bandname", 
    "namereqstandard", "categoryname", "patriationpopulation", "factiontype", "reasonforaction", 
    "obgroup", "foldername", "itemstoselect", "svocp", "cscname", 
    "facname", "coursename", "funcname", "custname", "nameofgroup", "projectname", 
    "certifiername", "yrsof", "hrdname", "evpname", "itemstoselectname",
    "aapcategory", "buname", "projectfocus", "putnafraceandgender", "denotesrequiredraceandgender", 
    "zounosnick", "alanname", "namemarkedingreeneare", "iracegregory", "hccommname", "foodservicesale", 
    "ofes", "veteran", "ofeg", "office", "position", "coebat", 
    "interviewnotes", "faap", "method", "namefrom", "aldname", "team", 
    "sracens", "fisher", "deparment", "filename", "lename", "refer", 
    "racecounts", "eesub", "countof", "raceareyou", "diversityinfo", 
    "school", "loaction", "secretary", "planname", "upervis", "vlookup", 
    "tradisp", "supv", "supn", "spv", "vietnam", "branch", 
    "recruiter", "decline", "from", "tracecom", "date", "guess", 
    "site", "manager", "organization", "certification", "formac", "agent", 
    "job", "union", "visual", "city", "levelast", "workflow", 
    "superior", "business", "org", "sector", "aborator", "inc", 
    "institution", "eceived", "pcname", "andreqid", "egion", "submitter", 
    "deanname", "stage", "report", "property", "fieldna", 
    "username", "title", "racesfem", "facility", "sheet", 
    "rsnname", "provinc", "depart", "super", "reqfor", "feeder", 
    "loc", "jgname", "country", "orgname", "job", "unknown", 
    "prefix", "uffix", "genderfemale", "gendermale", "activit", "object", 
    "division", "dept", "location", "mgr", "groupname", "confirm", 
    "collab", "employer", "unit", "superv", "function", "count", 
    "aaplan", "center", "partner", "ofccp", "interviewer", 
    "plan", "coach", "requisit", "peoplesoft", "reviewer",
    "namelinked", "racedta", "statu", "yearname", 
    "seekey", "divstaff", "customer", "tier", "cluster", "longname", 
    "nonexempt", "nameorid", "parameter", "racesmale", "aldta", "namename", 
    "racefem", "posting", "hrbpname", "event", "owner", "prnn", 
    "eigenderpress", "reportsto", "recruitingworkflow", "hmname", "sup", 
    "gradena", "displayname", "status", "step", "yeargender", "state"), 
  collapse = "|")

# ---------------------------------------------------------------------------------

gender_list_short <- list(
  "female" = c("femlale", "females", "frau", "weiblich", "she", "fem", "fema","femaleidentif","feman","femino","wmn",
               "femalenotto", "femalke", "femaleiprefernottodisclose", "femal", "felame", "femalechoosenotto","wman",
               "female", "shef", "her", "femaile", "feamle", "femalle", "sfemalev", "femalef", "ffemale", 
               "ffemale", "femaleemaleale", "femnale", "woman", "girl", "femaile", "femail", "femaleemaleail", "idonotwishtoprovidethisinfofemale", 
               "idonotwishtoprovidethisinfomale", "femalefemale", "queerfem", "transfemale", "transwoman", "mtf"),
  "male" = c("males", "mannlich", "mann", "männlich", "male", "him", "hismale", "mail", "christopher", "chris", "michael", "fsbm", "mfale", "mlae", "mike", 
             "dude", "guy", "malenotto", "maleiprefernottodisclose", "malechoosenotto", "femaletomaletrans","comalepanjobboard",
             "maley", "his", "tospecifemaley", "amle", "maile", "smalev", "malem", "malevisual", "maleabogal","oluwafemi",
             "mmale", "maleale","make", "man", "boy", "malemale", "maleundisclosed", "maleidentif","maleau","isazamale","tegamale",
             "malequeer fluid", "malequeerfluid", "gayman", "transmale", "maleiamtransftm", "transedftm", "ftm"),
  "NA" = c("NATOEVERYONE", "malefemalerace", "malefemale", "femalemale")
)


gender_list <- mapply(c, 
                      (gender_list_short <- lapply(gender_list_short, sort)), 
                      (extra_gender_vals_list <- lapply(
                        list("female" = c("0", "00", "000", "2", "02", "002", "two", "gal", "g", "fe", 'f'), 
                             "male" = c("mm", "1", "01", "001", "one", "mae","he", "mal", "m", "m", "mj"), 
                             "NA" = c("idk", "l", "n","a", "a", "a", "a", "a", "binary", "ito", "NA", "google", "ed",
                                      "preferrednotto", "careerbuilder", "simplyhired", "linkedin", "torespond", "monster", "indeedcom", "indeed", "fulltime", "screened", "nr","ng",
                                      "not", "interviewdate", "notcollected", "notthisquestion", "ss","cd", "cm", "ni", "n", "i", "b", "malefemale", "femalemale",
                                      "candidatetakethesteps", "uknown", "report", "notto", "notsupplied", "notselected", "notgiven", "notentered", "noinformation", "noident", "no", "followstepstocompleteeeoinformation", "donot", "column", "ud", "to", "or", "key", 
                                      "ai", "active", "hire", "aa", "mf", "t", "o", "iidentifyasqueerandprefertheytheirpronouns", "nonbinary", "nonconforming", "queer", "na", "to", "blanj", "ua",
                                      "mixed", "server", "un", "tw", "to", "sv", "ndg", "r","cr", "c", "dept", "el", "it", "mc", "wr", "s", "hours", "ftpt", "ar", "cs", "e", "j",
                                      "p", "i", "d", "race", "br", "desc", "gmus", "queertrans", "trans", "transed", "transedqueerly", "name", "namelf", #"white", "asian",
                                      # "whitenothispanicorlatino", "ormoreracesnothispanicorlatino", "ormoreraces", "nativehawaiianorotherpacificislandernothispanicorlatino", "blackorafricanamericannothispanicorlatino",# "americanindianoralaskanativenothispanicorlatino", "asiannothispanicorlatino","latino","amind", # "caucasian", "black", "asianpacificislander", "asianother", "asianindian", "twoormore", "whiteunitedstatesofamerica","hispanicother", "hispaniclatino", "pacificisland",# "nativehawaiianorpacificisler", "orlatino", "orpacificisl", "ormoreraces","asion", "african", "hispanic", "black", "ormore","twoormore",# "americanindianoralaskanative", "americanindianalaskannative", "twoormoreraces", "blackorafricanamerican","hispanicorlatino", "nativehawaiianorotherpacificislander", # "whiteapersonhavingoriginsinanyoftheoriginalpeoplesofeuropethemiddleeastornorthafrica", "twoormoreracesallpersonswhoidentifywithmorethanoneoftheaboveraces",# "nativehawaiianorotherpacificislanderapersonhavingoriginsinanyoftheoriginalpeoplesofhawaiiguamsamoaorotherpacificislands", "blackorafricanamericanapersonhavingoriginsinanyoftheblackracialgroupsofafrica","pacificisl",# "asianapersonhavingoriginsinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam",# "americanindianoralaskanativeapersonhavingoriginsinanyoftheoriginalpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintainstribalaffiliationorcommunityattachment",
                                      "no", "ns", "q", "o", "blank", "8", "9", "10", "11", "g", "l", "w", "p", "h", "0", " ", "u", "x", "null", "unk"))
                        , sort)), 
                      SIMPLIFY=FALSE)












#vim nano emacs
race_list_short <- list(
  "american indian or alaska native" = c("american indian or alaska native", 
                                         "aamericanindianoralaskanative", "aian", "aina", "aindian", "nativeamericanamericanindianalaskanative",
                                         "aioan", "aioran", "alakan", "alaska", "alaskanative", "alaskannative", "nativeamericanalasknative",
                                         "alaskannativeamerican", "ameindia", "ameralaskaindian", "amerianindianalaskanative", 
                                         "american indian or alaska native", "americani", "americanind", "americanindianalaskannativeallpersonshavinginanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhiteomaintainsculturalidentificationthroughtribalaffiliationorcommunityrecognition",
                                         "americanindalasnat", "americanindiainalaskanative", "americanindian", "americanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                                         "americanindianaknative", "americanindianalaskan", "americanindianalaskanat", "nativeamericanalnative",
                                         "americanindianalaskanativ", "americanindianalaskanative", "americanindianalaskannational", 
                                         "americanindianalaskannative", "americanindianalaskannativeallpersonshavinginanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintainsculturalidentificationthroughtribalaffiliationorcommunityrecognition", 
                                         "americanindianalaskiannative", "americanindiannativealaskan", "americanindalasianat","nativeamericanaknative",
                                         "americanindiannativeamerican", "americanindiannofhispanic", "americanindianalaskannativeallpersonshavinginanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintainsculturalidentificationthroughtribalaffiliationorcommunityrecognition",
                                         "americanindianor", "americanindianora", "americanindianoralaska", "alaskannativeoramericanindian",
                                         "americanindianoralaskan", "americanindianoralaskanative", "americanindianoralaskanativealgonquinfirstnation", 
                                         "americanindianoralaskanativecherokee", "americanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhiteomaintaintribalaffiliationorcommunityattachment", 
                                         "americanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachment", 
                                         "americanindianoralaskanativenorhisporlat", "americanindianoralaskanativenot", 
                                         "americanindianoralaskanativeor", "americanindianoralaskanativeotlatino", "americanindianoralaskanativenorhispanic",
                                         "americanindianoralaskanativerolatino", "americanindianoralaskannat", "americanindianoralaskanativeapersonhavingoriginsinanyoftheoriginalpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintainstribalaffiliationorcommunityattachment",
                                         "americanindianoralaskannative", "americanindianoralaskannativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhiteomaintainstribalaffiliationorcommunityattachment", 
                                         "americanindianoralaskannativeor", "americanindianoralaskiannative", "americanindianoralaskanativeinanofthealpeoplesofnorthsouthamericanincludingcentralamericanwhiteomaintainstribalaffiliationorcommunitattachment",
                                         "americanindianoralasknative", "americanindianoralsskanative", "americanindianoralaskanativenorhispanicorlat",
                                         "americanindianornativealaskan", "americanindianpacificislander", "americanindianoralaskannativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintainstribalaffiliationorcommunityattachment",
                                         "americanindiaoralaska", "americanindiaunlaskanative", "americaninidanoralaskanative", 
                                         "americanofirishancestry", "americanoralaskannative", "ameriindnalasknativ", "americanindianoralaskanativelatino",
                                         "amerind", "amerindalasknat", "amerindalasknatnothispnc", "amerindalnative","latinoamericanindianoralaskanative",
                                         "amerindian", "amerindianaknative", "amerindianalaskanative", "americanindianoralaskanativev",
                                         "amerindianalasknnative", "amerindiannativeamerican", "amerindianoralaskanat", 
                                         "amin", "amind", "amindala", "amindalaskannative", "amindian", "americanindianoralaskannativetwo",
                                         "amindianalaskanative", "amindianalaskannative", "amindianamericanindianalaskanative", 
                                         "anai", "asianamericanindianoralaskanative", "blackoramerican", 
                                         "ind", "indian", "indianalaskian", "indiannative", "indigenoatralianno", 
                                         "indigenoatralianyes", "mamericanindianor", "namericanindianalaskannative", 
                                         "native", "nativealaskan", "nativeam", "nativeameicanindian", "natam",
                                         "nativeamer", "nativeameralaskan", "nativeameralaskanative", "nativeamericanulhaque",
                                         "nativeamerican", "nativeamericanala", "nativeamericanalas", "americanindianoralaskanativeapersonhavingoriginsinanyoftheoriginalpeoplesofnorthsouthamericaincludingcentralamericawhomaintainstribalaffiliationorcommunityattachment",
                                         "nativeamericanalaska", "nativeamericanalaskan", "nativeamericanalaskanat", 
                                         "nativeamericanalaskanative", "nativeamericanalasknnative", "nativeamericanalsknnative", 
                                         "nativeamericanb", "nativeamericanindian", "nativeamericanoraknative", 
                                         "nativeamericanoralaskanat", "nativeamericanoralaskanative", "americanindianoralaskanativeinanyofthealpeoplesofnorthsouthamericanincludingcentralamericanwhiteomaintainstribalaffiliationorcommunityattachment",
                                         "nativeamericanoralaskannative", "nativeamericanw", "nativeamerician", 
                                         "nativeindian", "nnativeamerican", "ramericanindianoralaskannative", 
                                         "samericanindianoralaskanative", "samericanindianoralaskanativev", 
                                         "wnativeamerican"),
  "asian" = c("asian", "viet", "asiana","asianorasianeuropean", "indian", "asianisual","minahasianindonesia","chineseindonesia","eurasiansingapore","indonesia","batakindonesia",
              "asion","twoasian","asiantwo","asianv","chineseunitedkingdom","thai","thaithailandthailand","sundaneseindonesia","sthailandthailand","javaneseindonesia",
              "fasian", "asianorpacisl","masian", "india", "rasian","asianorasianbritish", "asianandasianamericanincludespakistanisindians", "bangladeshi","southasian", "sindhiindian", "koreanamerican","chinesejapanese", "asiachinese", "lasian", 
              "asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingcambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandvietnam", "asiannislat", "sasian", "bengladeshi", "aisan", "nepalese", "burnese", "bengali", "sikh", "myanmar", "asianturkic", "asiantibetan", "asiansrilankan", "asianpakistaniamerican", "asiannepali", "asiannepalese", "asiankalmyckmongol", "asianindonesian", "asianindiangujarati", "asianindianfilipino", "asianindianasianpakistani", "asianindianasianindian", "asianbangladeshi", "asianbangalesh", "napalese", "eurasian", "taiwanese", "asianbangladeshiunitedkingdom", "singapore", "indiansouthafrica", "hanchina", "asiannothispanicorlano", "asianindianunitedkingdom", "mysindian", "asianpakistaniunitedkingdom", "eastasian", "gbrasianpakistani", "asianunitedkingdom", "gbrasian", "gbrasianchinese", "mysmalay", "myschinese", "thaithailand", "asianvisual", "asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentnot", 
              "asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam", "asiam", "sgpindian", "thathai", "vietnam", "sunna", "indiansingapore", "asina", "asia", 
              "asn", "sasianv", "kinhvietnam", "asianpakistani", "basian", "asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingcambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam",
              "asianself", "asianjapanese", "aia", "gbrasianindian", "chinesesingapore", "nonindigenotaiwantaiwan","balineseindonesia",
              "asianvietnamese", "asiankorean", "sgpchinese", "asianchinese", "sgchinese", "asians", "westasianarab","asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislands","asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnamwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafrica",
              "asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent", "asianin","ipshita", "indiannon", "indiansubcontinent", "asisan", "asianpi", "indianmalaysia","asianapersonhavingoriginsinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislsthailvietnam",
              "asianinanofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalasiapakistanthephilippineislsthailvietnam","haitan",
              "asianallpersonshavinginanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentthisareaincludesforexamplechinaindiajapanandkorea", "southeastasianincludinurmesecambodianpilipinolaotianmalaysianthaivietnamese","asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislsthailvietnamwhiteinanyofthealpeoplesofeuropemiddleeasrnorthafrica",
              "asianindiansubcontinent", "sgindian", "americanasian", "asianamerican","china","ssingapore","eastasianincludingchinesejapanesekoreanpolynesian","asianchina","apersonhavingoriginsinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislsthailvietnamnot",
              "asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentncludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislands","asianapersonhavingoriginsinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam",
              "asianinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentncludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislands","asianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineisls",
              "aisian", "asianian", "cambodian", "vietnamese", "korean", "japanese", "chinese", "asianindian", "southasiaeastindianincludingindianpakistanisrilankanbangladeshieastindiansfromguyanatrinidadeastafrica","asianinanofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalasiapakistanthephilippineislsthailvietnam",
              "asain", "nativeasian", "southeastasian", "asianasianamerican", "southasiaeastindian", "asiannothispanicorlatono", "asianorasianamericaneuropean"),
  "black or african american" = c("black or african american", "blackorafricanamericanblackorafricanamerican","blackorafricanamericaninanyoftheblackracialgroupsofafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                                  "blackorafricanamerican", "morocco", "sblackorafricanamerican", "sblackorafricanamericanv","blacknothislatino", "blackorafricanamericantwo",
                                  "guyanaian", "jamaican","blackcarribamer", "fblack","mblack", "blackorafricanamericannofhispanic", "blackorafricanamericaninanoftheblackracialgroupsofafrica",
                                  "blackorafricanamericaninanyoftheblackracialgroupsofafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhiteomaintaintribalaffiliationorcommunityattachment", "blackaframer", "rblackorafricanamerican", "jblackorafricanamerican", "cblackorafricanamerican", "bk", "afircanamerican", "blackoraframnisporlatino", "blackorafram", "af", "blacknativeamericanoralaskannative", "blackorafricanamericanotlatino", "africanamercian", "blackorafricaname", "blackaframblackorafricanamerican", "blackorafricanam", "blackorafrica", "blacknislat", "blackorafricanamericanaperso", "blackorafricanmerican", "blackafricanam", "africanam", "sblackafricanamerican", "borafam", "westindian", "blackafricaamerican", "westindianblack", "copticegyptian", "blackafricanmerican", "egyptian", "africanamericanbla", "blackafricaname", "blackamerican", "blackafricanamericaninanyoftheblackracialgroupsofafriwhiteinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam", "backorafricanamerican", "africanamer", "blackafricanamericaninanyoftheblackracialgroupsofafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachment", "yorubanigeria", "blackafricanamerica", "blackcode", "blackaficanamerican", "gbrblackcarribean", "blackafricianamerican", 
                                  "blackafrianamerican", "blackafricanamercian", "sblackafricanamericanv", "africanamericanblacknofhispanic", "africansouthafrica", "africanamerica", "blackafricanamericaninanyoftheblackracialgroupsofafriwhiteinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent", "blackmorethanchoice", 
                                  "blackafericanamerican", "blackafricanamericanself", "blackafricanameri", "blackorafricanamericanorafricanamerican", "blackafroamerican", "blackorafricanamericaninanyoftheblackracialgroupsofafricaasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam",
                                  "blackblackafricanamerican", "blackaffricanam", "blackafam", "cblackafricanamerican", "blackunitedkingdom","blackorafricanamericaninanyoftheblackracialgroupsofafricaasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent",
                                  "twoormoreracesallpersonswho", "two or more racesallpersonswho", "blackafricanamer", "blackorafricanamericaninanyoftheblackracialgroupsofafricanthispanic","twoblack",
                                  "nonhispanicblack","blackorafricanamericannothispanicotlatino","blackafrcianamerican","blackafricanamercan","blackafricananerican","blackmon","blackmore","blacktwo",
                                  "blackafricanunitedkingdom","coloured",
                                  "blackafricanamericanallpersonshavinginanyoftheblackracialgroupsofafrica", "blackorafricanamericanallpersonshavinginanyoftheblackracialgroupsofafrica","blackcarribamerican",
                                  "ablackafricanamerican", "blackafricanamericanaperso", "blackafricanamericanorafricanamerican", "blackorafricanamerican","blackorafricanamericaninanyoftheblackracialgroupsofafricawhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafrica",
                                  "blackaa", "blackafra", "blackafricanamericanblackafricanamerican","blackcarribean","zafafrican","blackorafricanamericaninanyoftheblackracialgroupsofafricanthispanicorlatino",
                                  "blackafricanamericaninanyoftheblackracialgroupsofafrica", "blackafricanameric", "blackafricanameerican","blackorafricanamericanapersonhavingoriginsinanyoftheblackracialgroupsofafrica",
                                  "blackorblackeri", "blackafricanamernothispnc", "balckorafricanamerican", "bafricanamerican","blackorafricanamericaninanyoftheblackracialgroupsofafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                                  "africanamericanorblack", "afr", "blackofafricanamerican", "blackafricnaamerican", "blackafamer", "balck","blackorafricanamericaninanyoftheblackracialgroupsofafricaasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam",
                                  "africanblack", "blackorblacker", "blackorblack", "blackorafam", "blackafricanamericna", "blackafrican", "blackorafricanameric", "blackorafricanameerican",
                                  "blackblackblackorafricanamerican", "somalian", "baa", "ba", "blackforafricanamerican", "bl", "blackorafricanamerican","twoblackorafricanamerican",
                                  "black", "afri", "african", "africanamerican", "africanamericanblack", "blackafricanamerican", "blkafram", "blackafram", "blackblack", "blackandafricanamerican",
                                  "blackorafrican", "blackoraficanamerican", "blackorafroamerican", "blackorafricanamericaninanyoftheblackracialgroupsofafrica", "raceblack"),
  "hispanic or latino" = c("hispanic or latino", 
                           "canales","hispaniclatinoq", "hispanicpuerrico","hispanicorlatinos","nonwhitelatinomerican", "nonwhitelatinomericanincludingindigenopersonsfromcentralandsouthamerican",
                           "lathispanic","dunbrazil","hispanicinic","hispanichlatino","spaniclatino","hispanicc","latinomericanincludingindigenopersonsfromcentralandsouthamerican","latinotwo",
                           "hispaniciclatino", "hispanics", "hispanicorlatinoaor", "hispanicapaniclatino", "bhispanic", "fhispanic", "hispaniclatinos", "mhispanic", "ehispaniclatino", "elatino", "hispaniclatinomerican", "hispanicorlatinoo", "hispanicfri", "thispanicinformation", "hispanicw", "hispaniclatinohispanicorlatino", "hispanicn", "atthispanictime", "shispanicorlatino", "nicaraguen", "hyspanic", "guatemalan", "argentinean","honduran", "hislatin","ecuadorian", "portuguese","peruvian", "costarican","horl", "bolivian","venezulean", "salvadorean","columbian", "hisplatinoall","dominican", "venezuelan","hispaniclatinoorofspanish", "hispanicorlatinounspecified","hispanicorlatinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafriwhiteinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachment", "hispanicorlatinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackafricanamericaninanyoftheblackracialgroupsofafrica","hispaniclat", "haspanic","statehispanicorlatino", "hipanic","latinohispanic", 
                           "iamhispaniclatinoaofcubanmexicanpuerricancentralorsouthamericanorspanishcultureorregardlessof", "hispanicapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessof",
                           "hispaniclatinoi", "bballpersonsofmexicanpuerricancubancentralorsouthamericanorspanishcultureorandthanwhite", "hispaniclatibn", "hl", "hispanicapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessof",
                           "hispanicorlation", "hisplatino", "hsp", "dhispanicorlatino", "lathisp", "shispanicorlatinov", "hispanicmexican", "hispaiclatino", "hispanicorlatinoself", "puerricancommonwealth", "dlatino","puerrico",
                           "hispanicorlatinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachment", "mexican", "hispanichispanic", "hispanicpuerricancommonwealth", 
                           "hispanicorlatinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent", "alatino","latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent",
                           "hispanicorlatinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofblackafricanamericaninanyoftheblackracialgroupsofafrica", "puerricanmainland", "cuban", "hispaniccode", "hispanicpuerricanmainland", "hispaniccuban", "hispanicorlatinopuerrico", 
                           "hispanicorlatinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafrica", "hispanic","latinoapersonofcubanmexicanpuerrica",
                           "bhispanicorlatinowhiteonlyballpersonsofmexicanpuerricancubancentralorsouthamericanorspanishcultureorandofthetwoormoreracesorlatino", "statehispanic","apersonhavingoriginsinanyoftheoriginalpeoplesofnorthsouthamericaincludingcentralamericawhomaintainstribalaffiliationorcommunityattachmentnot",
                           "hispanicorlatin", "hispanicpuerrican", "nhisp","hispaniclatin", "latinoorhispanic", "hispanicorlatinoapersonofcubanmexicanpuerrica", "hispanic or latinoapersonofcubanmexicanpuerrica", "latio",
                           "latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafrica",
                           "latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricaasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                           "latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafrica",
                           "hispanicocom","hispaniclatinohispaniclatino","pardoormulatino",
                           "ahispanicorlatino", "ahispanic or latino", "hispanic or latino or latino", "hispanicorlatinoorlatino","hispanicapersonofcubanpuerricansouthorcentralamerican","hispanicnot","latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                           "hispanicorlatinoorlatino","hispanic or latinoorlatino", "hispanicorlatino","hispanicorlat","hispanicapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafrica",
                           "hispanicorlatinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessof","hispanicapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofblackorafricanamericaninanyoftheblackracialgroupsofafrica",
                           "puerrican", "his", "hislation", "hipaniclation", "hhispanic", "lat", "hisp", "hispanicorlatinopuertorico", "hispanicapersonofcubanpuertoricansouthorcentralamerican","latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafrica",
                           "hispanicorlatinoapersonofcubanmexicanpuertoricansouthorcentralamericanorspanishcultureororiginregardlessof","latinomerican","twohispaniclatino","hispanicstwo","latinopuerrico",
                           "hispanicorlatinoapersonofcubanmexicanpuertoricansouthorcentralamericanspanishcultureororiginregardlessof","hila","hla","hispanicstate","hispanicdstate","latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessof",
                           "hisplat", "hispw", "puertorican", "guamanianchamorro","hislatinohispanicorlatino", "hispanicoflatino","hispanicapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricaasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                           "hispaniclatinospaniclatino", "mexicanamermexicanchicano", "southamerican", "hispanicorlatino","hispanicapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                           "hispanicorlatinio", "spanish", "hispanic", "hispaniclatino", "hislatino","hispanicall", "hispanicorlatinohispanicorlatino", "hispanicnative","latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofblackorafricanamericaninanyoftheblackracialgroupsofafrica",
                           "hispanicorlatinoallothers", "hisporlat", "hispa", "latino", "asianpacificislander", "hispancorlatino", "hispaniclatino", "iamhispaniclatinoa", "latina", "latinx", "latin"),
  "native hawaiian or other pacific islander" = c("native hawaiian or other pacific islander","nativehawiianorothrpacificisl",
                                                  "polynesian", "polynese", "polynesia", "melanesian", "melanese","melanesia","micronesia","micronesian","micronese",
                                                  "tuvalu","vanuatu","samoan","samoa","fijian","fiji","nauru","nauruan","palau","palauan","fillipino",
                                                  "aasianpacificislander", "americanindianpacificislander", "anativehawaiianorpacificislander", 
                                                  "aorpi", "ap", "api", "asainorpacificisland", "asianfilipino", "nativehawaiianpacislander","tagalogphilippines",
                                                  "asianfillipino", "asianindiannative", "asianislandpacific", "sopacific", "southpacific","malaysingapore",
                                                  "asiannativehawaiianorasianpacificislander", "asiannativehawaiianorpacificislander","pacificislandernativehawaiian",
                                                  "asiannativehawaiianpacificislander", "asianorp", "asianorpacificisl", "asianorpacificisland","tonga", "tongan",
                                                  "asianorpacificislander", "asianpac", "asianpacific", "asianpacifici", "nativehawiiothrpacislander",
                                                  "asianpacificisland", "asianpacificnative", "asianpacis", "asianpacisl", "pacificislandernativehawaiian",
                                                  "asianpacislander", "asianpasificislander", "aspi", "dnativehawaiianorotherpacificislander", "rresstraightislanderatralia",
                                                  "filipano", "filipino", "filipinoamerican", "gnativehawaiianorotherpacificislander", "nativehawaiiannofhispanic",
                                                  "haw", "hawaiian", "hawaiianorpacificislander", "hawaiianorpacislandr", "pacisl","nathawothpacislnd",
                                                  "hawaiianotherpacificisland", "hawaiianpacific", "hawaiianpacificisl", "nathawothpacislndhispanic",
                                                  "hawaiianpacificisland", "hawaiianpacificislander", "hawaiianpacificislands", "hawiianothpacisland",
                                                  "hawaiianpacislanders", "hawaiin", "hawaiinorpacificislander", "nativeorpacisl","hawaiianpacislnd",
                                                  "hawaiipac", "hawiaan", "hawiianorpacificislander", "hawiianpacisland", "nativehawaiianorotherpacificislanderasian",
                                                  "hawiianpacisnd", "hawpacif", "hawpacifnathawaiianothpacislander", "nativehawaiianorotherpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislandsasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent",
                                                  "hawpacifnathawaiianpacislander", "hawpi", "hipi", "hop", "hpacificislndr", "asianpacificisler",
                                                  "hpi", "islander", "knativehawaiianor", "nathawaiian", "nathawaiianoacislander", "maori",
                                                  "nathawaiianothpacificislander", "nathawaiianothpacislander", "hawaiipi","hapi",
                                                  "nathawaiianpacificislander", "nathawaiianpacisland", "nathawaiianpacislander", "hispanicpacificnative",
                                                  "nathawaiinothpacislandr", "nathawaiinthpacislander", "nathawaiipacis", "nativehawaiianorpacificisler",
                                                  "nathawothpacislander", "nathawothpacislndhislat", "nathawpacific", "hawaiianpcificislander",
                                                  "nathawpacificnislat", "native hawaiian or other pacific islander", "nativehawaiianorpacificislerapersonhavingoriginsinanyoftheoriginalpeoplesofhawaiiguamsamoaorpacificisls",
                                                  "nativeamericanalaskannative", "nativeamericanoralaskanative", "malay","nativehawaiinothpacisland",
                                                  "nativeamericanoralaskannative", "nativehawa", "nativehawaiannorpacificislander", 
                                                  "nativehawaiian", "nativehawaiianandpacificislander", "nativehawaiianor", "nativehawaiianorpacificislerinanofthealpeoplesofhawaiiguamsamoaorpacificisls",
                                                  "nativehawaiianorasianpacificislander", "nativehawaiianorasianpacificislanderasian", 
                                                  "nativehawaiianoro", "nativehawaiianorotherpacific", "nativehawaiianorotherpacificislander", 
                                                  "nativehawaiianorotherpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislands", 
                                                  "nativehawaiianorotherpacificislanderinanyofthepeoplesofhawaiiguamsamoaorpacificislands", 
                                                  "nativehawaiianorotherpacificislandernativehawaiianorotherpacificislander", 
                                                  "nativehawaiianorotherpacificislandernis", "nativehawaiianorotherpacificislanderorl", 
                                                  "nativehawaiianorotherpacificislanderorlatin", "nativehawaiianorotherpacisl", 
                                                  "nativehawaiianorpacif", "nativehawaiianorpacific", "nativehawaiianorpacificisland", 
                                                  "nativehawaiianorpacificislandaer", "nativehawaiianorpacificislande", "hawaiianpacificislanderasian",
                                                  "nativehawaiianorpacificislander", "nativehawaiianorpacificislanderasian", 
                                                  "nativehawaiianorpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislands", 
                                                  "nativehawaiianorpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislandsasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent", 
                                                  "nativehawaiianorpacificislanderinanyofthepeoplesofhawaiiguamsamoaorpacificislands", 
                                                  "nativehawaiianorpacificislandernativehawaiianorpacificislander", "nativehawaiianorotherpacificislanderv",
                                                  "nativehawaiianorpacificislandernis", "nativehawaiianorpacificislandernothispanicorl", 
                                                  "nativehawaiianorpacisl", "nativehawaiianorpacislander", "nativehawaiianotherpacific", 
                                                  "nativehawaiianotherpacificislander", "nativehawaiianothpacifis", "nativehawaiianorotherpacificislanderlatino",
                                                  "nativehawaiianothpacisland", "nativehawaiianpacific", "nativehawaiianpacificisland", 
                                                  "nativehawaiianpacificislande", "nativehawaiianpacificislander", "pacificnativepacificnative",
                                                  "nativehawaiianpacificislanderallpersonshavinginanyofthealpeoplesofthehawaiianislandsorpacificislandsincludingthephilippineislandsandsamoa", 
                                                  "nativehawaiianpacificislanderinanyofthepeoplesofhawaiiguamsamoapacificislands", 
                                                  "nativehawaiianpacificislndr", "nativehawaiianpacifis", "nativehawaiianpacisl", 
                                                  "nativehawaiianpacisland", "nativehawaiin", "nativehawaiinislander", "nativehawaiianorotherpacificislanderapersonhavingoriginsinanyoftheoriginalpeoplesofhawaiiguamsamoaorotherpacificislands",
                                                  "nativehawaiinorotherpacificislander", "nativehawaiinorpacificislander", "pacificnativepacific",
                                                  "nativehawaiinotherpacificislander", "nativehawaiinpacificislander", "nativehawaiianorrpacificislander",
                                                  "nativehawaiinpacisland", "nativehawaiipacifcisln", "nativehawhiteaiianorpacificislander", 
                                                  "nativehawianpacificislander", "nativehawiian", "nativehawiianorpacificislander", "pacifi",
                                                  "nativehawiianpacificisland", "nativehawiianpacificislander", "asianhawaiianpacificislander",
                                                  "nativhawiiothrpacisldr", "nh", "nhi", "nhopi", "nhorpi", "nhp", "nathawothpacislndlatino",
                                                  "nhpi", "pac", "pacif", "pacific", "pacificisland", "pacificislander", "americanindianoralaskanativeasianlatino",
                                                  "pacificislanderhawaiian", "pacificislandernofhispanic", "pacificislanderornativehawaiian", 
                                                  "pacificnative", "pacificnativeasian", "pacificnativeindiannative", "hispanicpacificnative",
                                                  "pacisland", "pacislander", "philippines", "pi", "ppacificislander", "hispanicpacificnativeasian",
                                                  "rnativehawaiianorotherpacificislander", "snativehawaiianorotherpacificislander",
                                                  "snativehawaiianorotherpacificislanderv", "snativehawaiianorpacificislander", 
                                                  "snativehawaiianorpacificislanderv"),
  "white" = c("white", "thispanicinformationtwoormoreraces", "europeannewzealand","europeannewzealand","europeans","unitedkingdom",
              "azeri", "whitenon", "whitenonsasa", "whitenofhispanichispanic","whiteite","whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricant",
              "whitemiddleeastern", "fwhite", "mwhite", "rwhite", "germany", "germanscottish", "cauwhite", "caasian", "abalatralia",
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricaasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent",
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhiteomaintaintribalaffiliationorcommunityattachment", 
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafriwhiteinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam", 
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafrica", "iwhite", "whitea", "whitenot", "ewhitenonhispanicorlatio", "whitenislat","european", "whitele", "swhite", "whitee", "whitenonhispanicorlation", "greekamerican", "bmwhite", "turkish", "bulgarian", "italian", "greek", "couwhite", "mediterranean", "wm", "wf", "infowhitehheld", 
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricanativehawaiianorpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislandsasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent", 
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricanativehawaiianorpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislands", "whitepuerrico", "statewhite", "atralia", "polish", "gbrwhiteeuropean", 
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachmentnot", 
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricant","blackafricanamlatino","nativeamericanalaskanatlatino","whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricanativehawaiianorotherpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislandsasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent",
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackafricanamericaninanyoftheblackracialgroupsofafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachment", "wit", "whiteh", "aribac", "cauc", "ca", "whitesouthafrica", 
              "wtt", "zafwhite", "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricanot", "whitecanada", "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricanthispanicorlatino","whitenothislatino","whiteinanofthealpeoplesofeuropemiddleeasrnorthafrica",
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackafricanamericaninanyoftheblackracialgroupsofafriwhiteinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent", "gbrwhiteirish", "whiteunitedkingdom", "whitenothispanicoflatino", 
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachment", "selfwhite", "gbrwhite", "whitebrazil", 
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackafricanamericaninanyoftheblackracialgroupsofafrica", "brawhite", "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafricanativehawaiianorotherpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislands",
              "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafriwhiteinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent", "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafricaasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinent",
              "whitecode", "ewhite", "gbrwhitebritish", "whitenonhispanic", "swhitev", "whitte", "whtie", "whiteor", "whitein", "whiteallpersonshavinginanyofthealpeopleofeuropenorthafricaorthemiddleeast","whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
              "awhite", "whiteirishunitedkingdom", "whiteie", "whiteeuropeanunitedkingdom", "whitebritishunitedkingdom","whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricaasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam",
              "nondiverse", "americanwhite", "whiteamerican", "wihite", "whitehdrew", "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafrica","whiteapersonhavingoriginsinanyoftheoriginalpeoplesofeuropemiddleeastornorthafricanot","whiteapersonhavingoriginsinanyoftheoriginalpeoplesofeuropemiddleeastornorthafrica",
              "whiteeuropean", "whitebritish", "while", "whie", "whiteornonhis","whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
              "whiteinanyoftheoriginalpeoplesofeuropethemiddleeastornorthafricaasianinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam",
              "whiteinanyoftheoriginalpeoplesofeuropethemiddleeastornorthafricaamericanindianoralaskanativeinanyoftheoriginalpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachment",
              "whiteinanyoftheoriginalpeoplesofeuropethemiddleeastornorthafricablackorafricanamericaninanyoftheblackracialgroupsofafrica","whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricanthispanic",
              "whitechoosenotto", "whitecaian", "ukranian", "whiteorwhite", "whte", "wt","whitete","whiteirish","sweden","whiteasperhonofccpreqresponse","whiteapersonhavingoriginsinanyoftheoriginalpeoplesofeuropemiddleeastornorthafrica",
              "whiteundisclosed", "caucasion", "nonminority", "whit", "whire", "uswhite", "caucasian", "whitewhite", "wh", "whitenothispanicorigin","whitev","nonhispanicwhite",
              
              "meast", "meast", "meast", "meast", "meast", "meast", "arabmiddleeastern", "arabian", "lebanese", "iraqi", "middleastern", "mideastern", "jordanian", "mideastrn", "middleeastern","westasianarabincludingarabianarmenianiranianisraelilebanesepalestiniansyrianturkishegyptianiraqi",
              "iraq", "greekamerican", "saudi", "turkish", "mideastern", "jordanian", "persianamerican", "middleeastren", "lebanese", "iraqi", "iranianamerican", "greek", "arabmiddleeastern", "whitetwotwo",
              "arabian", "syrian", "armenian", "arab", "afghani", "kurdish", "mediterranean", "iranian", "persian", "mideast", "arabicmiddleeastern", "aribac", "pakistani", "middleeastern", "middleeast", "arabic"),
  "two or more races" = c("two or more races", "nativeamericanalaskanathispanic","whiteblackorafricanamericanamericanindianoralaskanativetwoormoreraces","whiteblackorafricanamericantwoormoreraces","whitetwo","latinowhitetwoormoreraces",
                          "bhispanicorlatinowhiteballpersonsofmexicanpuerricancubancentralorsouthamericanorspanishcultureorandofthetwoormoreracesorlatino","americanindianoralaskanativeasianblackorafricanamericannativehawaiianorotherpacificislander",
                          "twoormoreracestwoormoreraces","blackorafricanamericanwhitetwoormoreraces","hispanicorlatinowhitetwoormoreraces", "hispanicorlatinotwoormoreraceswhite","allpersonswhowithmoraeoftheabacesnot",
                          "2ormore", "aamericanindianoralaskanativehispanicorlatino", "twoormoreracess", "hispanicorlatinotwoormoreraces","blackorafricanamericanhispanicorlatinotwoormoreraces","asianhislatino","mixedwhiteasian",
                          "mixedmixedunitedkingdom","mixedwhiteasianunitedkingdom","mixedwhiteblackafrican","mixedwhiteblackcarribeanunitedkingdom","mixedwhiteblackcarribean","hispanicblackorafricanamericantwoormoreraces",
                          "africanamericancaucasian", "africanamericanhispanic", "africanamericanwhite", "asiantwoormoreraceswhite","americanindianoralaskanativetwoormoreraces","hispanicorlatinonativehawaiianorotherpacificislander",
                          "afrihispanic", "americanindianalaskannativeasian", "whitetwoormoreraceswhitenativeamericanss", "blackorafricanamericantwoormoreraceswhite","americanindianoralaskanativetwoormoreraceswhite","whitesingapore",
                          "americanindianalaskannativeasianmultiracialwhite", "americanindianalaskannativeasiantwoormoreraces", "asiantwoormoreraces","asianblackorafricanamericantwoormoreraces","hispanicwhiteballpersonsofmexicanpuerricancubancentralorsouthamericanorspanishcultureorandofthetwoormoreraces",
                          "americanindianalaskannativeasianwhite", "americanindianalaskannativeblackafricanamerican", "blacklatino","americanindianoralaskanativeblackorafricanamericantwoormoreraces","twoormoreraceshislatino",
                          "americanindianalaskannativeblackafricanamericantwoormoreraces", "blackorafricanamericanwhitetwoormoreraces","asianblackorafricanamericantwoormoreraceswhite","blackorafricanamericantwoormoreraces","whitenativehawaiianorotherpacificislanderlatinoamericanindianoralaskanativetwoormoreraces",
                          "americanindianalaskannativeblackafricanamericantwoormoreraceswhite", "americanindianoralaskanativeasianblackorafricanamericanhispanicorlatinonativehawaiianorotherpacificislandertwoormoreraceswhite","twora",
                          "americanindianalaskannativeblackorafricanamerican", "americanindianalaskannativecaucasian", "americanindianalaskannativeblackorafricanamericantwoormoreraceswhite","twoormoreracesc","asianlatinowhite",
                          "americanindianalaskannativehispanicorlatino", "americanindianalaskannativehispanicorlatinowhite", "americanindianoralaskanativeblackorafricanamericantwoormoreraceswhite","twoorm","hispanicwhitetwoormoreraces",
                          "americanindianalaskannativetwoormoreraceswhite", "americanindianalaskannativewhite", "americanindianoralaskanativeblackorafricanamericanhispanicorlatinowhite","americanindianoralaskanativehispanicwhite",
                          "americanindianandwhite", "americanindianblack", "americanindianlatino", "americanindianoralaskanativenativehawaiianorotherpacificislandertwoormoreraces","americanindianoralaskanativehispanic","blackandafricanamericanlatinowhite",
                          "americanindianoralaskanativeasian", "americanindianoralaskanativeasianblackafricanamerican", "americanindianoralaskanativenativehawaiianorotherpacificislandertwoormoreraces","blackorafricanamericanlatino",
                          "americanindianoralaskanativeasianblackafricanamericanhispanicorlatino", "nativehawaiianorotherpacificislandertwoormoreraces", "americanindianoralaskanativeasianblackorafricanamericantwoormoreraceswhite","nativehawaiianorotherpacificislanderlatino",
                          "americanindianoralaskanativeasianblackafricanamericanhispanicorlatinonativehawaiianorpacificislandertwoormoreraces", "americanindianoralaskanativeasianblackorafricanamericanhispanicorlatinonativehawaiianorotherpacificislandertwoormoreraces",
                          "americanindianoralaskanativeasianblackafricanamericanhispanicorlatinonativehawaiianorpacificislandertwoormoreraceswhite", "blackorafricanamericannativehawaiianorotherpacificislandertwoormoreraces","americanindianoralaskanativeasianlatino",
                          "americanindianoralaskanativeasianblackafricanamericanhispanicorlatinonativehawaiianorpacificislanderwhite", "asiannativehawaiianorotherpacificislandertwoormoreraces","hispanicwhitetwo","americanindianoralaskanativeasianlatinonativehawaiianorotherpacificislandertwoormoreraces",
                          "americanindianoralaskanativeasianblackafricanamericannativehawaiianorpacificislander", "americanindianoralaskanativeblackorafricanamericanhispanicorlatinotwoormoreraceswhite","americanindianoralaskanativeblackorafricanamericanhispanicorlatinotwoormoreraces",
                          "americanindianoralaskanativeasiannativehawaiianorotherpacificislandertwoormoreraces","americanindianoralaskanativeasianblackorafricanamericantwoormoreraceswhite","whitelatio","americanindianoralaskanativeasianblackorafricanamericanlatinonativehawaiianorotherpacificislanderunitedsofame",
                          "americanindianoralaskanativeasianblackafricanamericannativehawaiianorpacificislandertwoormoreraceswhite", "nativehawaiianorotherpacificislandertwoormoreraceswhite","twosormore","latinotwoormoreraceswhite",
                          "americanindianoralaskanativeasianblackafricanamericannativehawaiianorpacificislanderwhite", "twoormoreracesallpersonswhiteowhitehmorethanoneoftheabovefives","ormoreraces","latinoblackorafricanamericantwoormoreraces",
                          "americanindianoralaskanativeasianblackafricanamericantwoormoreraces", "blackorafricanamericantwoormoreraces","blackorafricanamericantwoormoreraces","ormoreracesapersonwhiteoidentifieswhitehmoraeofthefollowingasdefinedabovewhiteblackorafricanamericannativehawaiianorpacificislerasianoramericanindianoralaskanative",
                          "americanindianoralaskanativeasianblackafricanamericantwoormoreraceswhite", "asianblackorafricanamericanhispanicorlatinotwoormoreraceswhite","nathawothpacislndhispanic","americanindianoralaskanativeasianblackorafricanamericanlatinonativehawaiianorotherpacificislandertwoormoreraceswhite",
                          "americanindianoralaskanativeasianblackafricanamericanwhite", "americanindianoralaskanativeasianblackorafricanamerican","blackorafricanamericanamericanindianoralaskanativetwoormoreraces","americanindianoralaskanativeblackorafricanamericanlatinotwoormoreraceswhite",
                          "whitelatinoblackorafricanamerican","whitelatinoblackorafricanamericanamericanindianoralaskanativetwoormoreraces","whiteblackasiannativeamerican","twoblackorafricanamericanhispaniclatino","blackwhitetwos",
                          "americanindianoralaskanativeasianblackorafricanamericanhispanicorlatinonativehawaiianorotherpacificislanderwhite", "asianindianoralaskanative","twoormoreraceshawaiianpacificislanderasianindianalaskannativeafricanamerican",
                          "americanindianoralaskanativeasianhispanicorlatino", "americanindianoralaskanativeasianhispanicorlatinonativehawaiianorpacificislandertwoormoreraces", "twoormoreracesapersonwhiteoidentifieswhitehmoraeofthefollowingasdefinedabovewhiteblackorafricanamericannativehawaiianorpacificislerasianoramericanindianoralaskanative",
                          "americanindianoralaskanativeasiannativehawaiianorpacificislandertwoormoreraces", "americanindianoralaskanativeasianhispanicorlatinonativehawaiianorotherpacificislandertwoormoreraces","latinoblackorafricanamerican",
                          "americanindianoralaskanativeasiannativehawaiianorpacificislandertwoormoreraceswhite", "asianblackorafricanamericanhispanicorlatino","personsofmixed","twotwoormoreraces","latinonativehawaiianorotherpacificislander",
                          "americanindianoralaskanativeasiannativehawaiianorpacificislanderwhite", "blackorafricanamericanhispanicorlatinotwoormoreraceswhite","twoormoreracesallpersonswhitehmorethanoneoftheabovesixs","americanindianoralaskanativelatinowhite",
                          "americanindianoralaskanativeasiantwoormoreraces", "americanindianoralaskanativeasiantwoormoreraceswhite", "whiteblackorafricanamericanasian","twoormoreracesapersonwhiteoidentifieswhitehmorethanoneofthefollowingasdefinedabovewhiteblackorafricanamericannativehawaiianorotherpacificislanderasianoramericanindianoralaskanative",
                          "americanindianoralaskanativeasianwhite", "americanindianoralaskanativeblackafricanamerican", "whiteasiantwoormoreraces","personsofmixedincludingpersonswhitehoneparentinoneofthevisibleminoritygroupslistedabove",
                          "americanindianoralaskanativeblackafricanamericanhispanicorlatino", "whiteblackorafricanamericanamericanindianoralaskanative","africanamericanamericanindianalaskannative","twoormoreracesallpersonswhiteoidentifwhitehmoraeoftheabaces",
                          "americanindianoralaskanativeblackafricanamericanhispanicorlatinonativehawaiianorpacificislander", "twoormoreracesallpersonswhiteowhitehmorethanoneoftheaboves","latinotwoormoreraces","twobr","americanindianoralaskanativelatinotwoormoreraceswhite",
                          "americanindianoralaskanativeblackafricanamericanhispanicorlatinotwoormoreraces", "nativehawaiianorotherpacificislanderhispanic","whiteamericanindianalaskannativehispanicafricanamerican","americanindianoralaskanativelatinotwoormoreraces",
                          "americanindianoralaskanativeblackafricanamericanhispanicorlatinotwoormoreraceswhite", "twoormoreracesblackorafricanamericanamericanindianoralaskanativetwoormoreraces","blackandafricanamericanamericanindianalaskannativeasianlatinonativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativeblackafricanamericanhispanicorlatinowhite", "twoormoreracesallpersonswhiteowhitehmorethanoneoftheaboves","mtwoormoreraces","blackandafricanamericanamericanindianalaskannativelatinowhite","blackorafricanamericanlatinotwoormoreraceswhite",
                          "americanindianoralaskanativeblackafricanamericannativehawaiianorpacificislander", "twoormoreracesforipedseeaap","asianindianalaskannative","blackandafricanamericanamericanindianalaskannativelatino","americanindianoralaskanativenothispanicotlatino",
                          "americanindianoralaskanativeblackafricanamericannativehawaiianorpacificislandertwoormoreraces", "asianhispanicwhitenofhispanic","blackandafricanamericanasianhispanicwhite","americanindianalaskannativelatinonativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativeblackafricanamericantwoormoreraces", "whiteoormore","asianhispanichawaiianpacificislander","nativeamericanorpacificislander","twob","blackandafricanamericanasianlatinonativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativeblackafricanamericantwoormoreraceswhite", "asianhispanicnativehawaiianorotherpacificislander","americanindianalaskannativenativehawaiianorotherpacificislanderwhite","asianlatinotwoormoreraces",
                          "americanindianoralaskanativeblackafricanamericanwhite", "americanindianoralaskanativeblackorafricanamerican", "asiannativehawaiianorotherpacificislanderwhitenofhispanic","americanindianoralaskanativeblackorafricanamericanlatinowhite",
                          "americanindianoralaskanativeblackorafricanamericanhispanicorlatino", "blackorafricanamericanamericanindianalaskan","asianwhitenofhispanic","blackandafricanamericanasianhispanicnativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativeblackorafricanamericantwoormores", "nativehawaiianorotherpacificislanderblackorafricanamericantwoormoreraces","blackandafricanamericanamericanindianalaskannativenativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativeblackorafricanamericantwoormoreswhite", "nativehawaiianorotherpacificislanderblackorafricanamerican","blackandafricanamericanamericanindianalaskannativehispanicwhite","latinoamericanindianoralaskanative",
                          "americanindianoralaskanativeblackorafricanamericanwhite", "americanindianoralaskanativehispanicorlatino", "asianwhiteamericanindianalaskannative","blackandafricanamericanasiannativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativehispanicorlatinonativehawaiianorpacificislander", "asianhispanicnativehawaiianorotherpacificislanderwhitenofhispanic","blackandafricanamericanhispanicnativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativehispanicorlatinonativehawaiianorpacificislandertwoormoreraceswhite", "hispanicwhiteasian","asianhispanicnativehawaiianorotherpacificislanderwhite","americanindianoralaskanativeasianblackorafricanamericanlatinonativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativehispanicorlatinotwoormoreraces", "whiteamericanindianoralaskanativetwoormoreraces","whiteamericanindianalaskannativeafricanamerican","asianlatinonativehawaiianorotherpacificislander","asianblackorafricanamericanlatino",
                          "americanindianoralaskanativehispanicorlatinotwoormoreraceswhite", "hispanicamericanindianalaskannative","hispanicwhitenofhispanic","blackandafricanamericanamericanindianalaskannativeasianhispanicwhite","nathawothpacislndhislatino","blackandafricanamericanamericanindianalaskannativeasianlatinowhite",
                          "americanindianoralaskanativehispanicorlatinowhite", "americanindianoralaskanativeinanyoftheoriginalpeoplesofnorthandsouthamericaincludingcentralamericaandwhomaintaintribalaffiliationorcommunityattachment", "americanindianoralaskanativeasianblackorafricanamericanlatinonativehawaiianorotherpacificislanderunitedstatesofame",
                          "americanindianoralaskanativenativehawaiianorpacificislander", "americanindianoralaskanativeasianblackorafricanamericannativehawaiianorotherpacificislanderwhite","blackandafricanamericanasianlatino","aamericanindianoralaskanativelatino",
                          "americanindianoralaskanativenativehawaiianorpacificislandertwoormoreraces", "americanindianalaskannativeblackorafricanamericantwoormoreraces","blackandafricanamericannativehawaiianorotherpacificislanderwhite","asianlatinonativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativenativehawaiianorpacificislanderwhite", "americanindianoralaskanativeblackorafricanamericanhispanic","blackandafricanamericanamericanindianalaskannativeasianwhite","blackandafricanamericanasianlatinowhite","blackorafricanamericanlatinotwoormoreraces",
                          "americanindianoralaskanativenothispanicrolatino", "americanindianoralaskanativeorlatino", "hispanicnativehawaiianorotherpacificislanderwhitenofhispanic","americanindianoralaskanativelatino","latinowhitetwo","americanindianalaskannativeasianlatinonativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativetwoormoreraceswhiteasian", "americanindianoralaskanativetwoormoreraceswhiteblackafricanamerican", "whiteasianafricanamerican","latinonativehawaiianorotherpacificislanderwhite","blackorafricanamericanlatinowhite","blackandafricanamericanamericanindianalaskannativelatinonativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskanativetwoormores", "americanindianoralaskanativetwoormoreswhite", "asianblackorafricanamericanhispanicwhite","twowhite","blackandafricanamericannativeamericanorpacificislanderwhite","americanindianoralaskanativeblackorafricanamericanlatino",
                          "americanindianoralaskanativewhite", "americanindianoralaskanativewhiteblackafricanamerican", "hispanicwhiteamericanindianalaskannative","blackandafricanamericanamericanindianalaskannativehispanicnativehawaiianorotherpacificislanderwhite","blackandafricanamericanamericanindianalaskannativeasianlatino",
                          "americanindianoralaskanativewhiteblackorafricanamerican", "americanindianoralaskannativeblackafricanamerican", "whiteafricanamericanhispanic","hawaiianpacificislanderhispanicwhite","americanindianalaskannativeasianlatino","americanindianalaskannativeasianlatinowhite",
                          "americanindianoralaskannativeblackafricanamericantwoormoreraces", "americanindianoralaskanativeblackorafricanamericanhispanicwhite","whitehislatino","americanindianoralaskanativeasianblackorafricanamericanlatinonativehawaiianorotherpacificislanderunitedsofame",
                          "americanindianoralaskannativeblackorafricanamerican", "americanindianoralaskannativehispanicorlatino", "hispanicwhitehawaiianpacificislander","blackandafricanamericanlatino",
                          "americanindianoralaskannativetwoormoreraces", "americanindianoralaskannativewhitenofhispanic", "americanindianoralaskanativenativehawaiianorotherpacificislanderwhite",
                          "americanindianoralaskannativewhitetwoormoreraces", "americanindianorpacificisland", "whiteblackorafricanamericanasianindianoralaskanative",
                          "americanindianwhite", "amindalaskanathislat", "amindianalaskanativetwoormoreraces", "asianblackorafricanamericanhispanic","americanindianoralaskanativehispanicnativehawaiianorotherpacificislander",
                          "armenianasian", "armenianasianamer", "asianamericanindianoralaskannative", "americanindianoralaskanativeblackorafricanamericannativehawaiianorotherpacificislandertwoormoreraces",
                          "asianandasianincludespakistanisindians", "asianapersonhavingoriginsinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislands", 
                          "asianblack", "asianblackafricanamerican", "asianblackafricanamericanhispanicorlatino", "hispanicafricanamericanasian","whitehawaiianpacificislander",
                          "asianblackafricanamericanhispanicorlatinotwoormoreraceswhite", "blackorafricanamericanhispanictwoormoreraces","whiteafricanamerican","blackandafricanamericanamericanindianalaskannativeasianlatino",
                          "asianblackafricanamericanhispanicorlatinowhite", "asianblackafricanamericantwoormoreraces", "hispanicafricanamericanwhite","eeotwob","americanindianalaskannativeasianlatinowhite",
                          "asianblackafricanamericantwoormoreraceswhite", "asianblackafricanamericanwhite", "hawaiianpacificislanderwhiteasian","blackorafricanamericanasianindianoralaskanative",
                          "asianblackorafricanamerican", "asianblackorafricanamericantwoormores", "asiantwoormoreracesblackorafricanamerican","whiteafricanamericanasian",
                          "asianblackorafricanamericanwhite", "asiancaucasian", "asianchinesewhite", "hawaiianpacificislanderasianwhite","whiteafricanamericanamericanindianalaskannative",
                          "asianeorlatino", "asianhislat", "asianhispanic", "asianhispanicorlatino", "hawaiianpacificislanderhispanic","nativehawaiianorotherpacificislanderwhitenofhispanic",
                          "asianhispanicorlatinonativehawaiianorpacificislander", "asianhispanicorlatinotwoormoreraces", "hispanichawaiianpacificislander","twoormoreracesallpersonwhitehmorethanoneoftheabovefives",
                          "asianhispanicorlatinowhite", "asianinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislands", 
                          "asianinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam", "americanindianalaskannativeasianlatino",
                          "asianindiansubcontinenthispanic", "asianindiansubcontinentwhite", "americanindianoralaskanativeasianblackorafricanamericanwhite","blackandafricanamericanamericanindianalaskannativeasianlatinowhite",
                          "asianlatino", "asiannativehawaiianorotherpacificislander", "asiannativehawaiianorotherpacificislanderwhite", "blackandafricanamericanamericanindianalaskannativeasianwhite","blackandafricanamericanasianlatinonativehawaiianorotherpacificislander",
                          "asiannativehawaiianorpacificislander", "asiannativehawaiianorpacificislandertwoormoreraces", "hawaiianpacificislanderwhite","blackandafricanamericanamericanindianalaskannativelatinonativehawaiianorotherpacificislanderwhite",
                          "asiannativehawaiianorpacificislanderwhite", "asiannativehawaiianpacificislandertwoormoreraces", "africanamericanwhitehawaiianpacificislanderamericanindianalaskannative","americanindianalaskannativeasianlatinonativehawaiianorotherpacificislander",
                          "asiannativehawaiianpacificislanderwhite", "asiannofhispanic", "hispanicapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofwhiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafrica",
                          "asianorpacificislanderblack", "asianorpacificislanderblacknativeamericanoralaskannative", "africanamericanasianhispanic","americanindianalaskannativeasianlatino","blackandafricanamericanamericanindianalaskannativeasianlatinonativehawaiianorotherpacificislander",
                          "asianorpacificislanderhispanic", "asianorpacificislandernativeamericanoralaskannative", "africanamericanamericanindianalaskannativetwoormoreraces","blackandafricanamericanlatinonativehawaiianorotherpacificislander",
                          "asianorpacificislandertwoormoreracesnativeamericanoralaskannativewhite", "africanamericanamericanindianalaskannativehispanicwhite","americanindianalaskannativeasianlatinowhite",
                          "asianorpacificislanderwhite", "asianorpacificislanderwhitenofhispanic", "africanamericanasianhispanicwhite","americanindianoralaskanativeasianblackorafricanamericanlatinonativehawaiianorotherpacificislanderunitedsofame",
                          "asianpacificnativewhite", "asiantwoormoreracesblackafricanamerican", "africanamericantwoormoreraces","blackandafricanamericanamericanindianalaskannativeasiannativehawaiianorrpacificislanderwhite",
                          "asiantwoormores", "asiantwoormoreswhite", "asianwh", "asianwhite", "africanamericanhispanicwhiteasianindianalaskannativehawaiianpacificislander","americanindianalaskannativelatinonativehawaiianorotherpacificislander",
                          "asianwhiteasian", "atwo or more races", "atwoormoreraces", "bhispanicorlatino", "africanamericanwhiteasianindianalaskannative","blackandafricanamericanamericanindianalaskannativeasianlatino",
                          "biracial", "bkwhite", "blackafricanamericanamericanindianalaskannative", "africanamericanwhiteamericanindianalaskannativehispanic",
                          "blackafricanamericanamericanindianoralaskanative", "blackafricanamericanamericanindianoralaskanativetwoormoreraces", 
                          "blackafricanamericanamericanindianoralaskanativetwoormoreracesorlatino", "americanindianoralaskannativewhite",
                          "blackafricanamericanasian", "blackafricanamericanasianjapanese", "hispanictwoormoreraceswhite","whitetwoormoreraceswhiteasian",
                          "blackafricanamericaneuropeanasian", "blackafricanamericanhispanic or latino", "twowhitetwoasian",
                          "blackafricanamericanhispanicorlatino", "blackafricanamericanhispanicorlatinonativehawaiianorpacificislanderwhite", 
                          "blackafricanamericanhispanicorlatinotwoormoreraces", "blackafricanamericanhispanicorlatinotwoormoreraceswhite", 
                          "blackafricanamericanhispanicorlatinowhite", "blackafricanamericanmultiracial", "twoormoreraceswhiteasian",
                          "blackafricanamericanmultiracialasian", "blackafricanamericanmultiracialwhite", "whitetwoormoreraceshispanicwhiteasianamericanindianoralaskanative",
                          "blackafricanamericannativehawaiianorasianpacificislander", "blackafricanamericannativehawaiianorpacificislander", 
                          "blackafricanamericannativehawaiianorpacificislandertwoormoreraces", "whitenativehawaiianorotherpacificislanderhispanicamericanindianoralaskanativetwoormoreraces",
                          "blackafricanamericannofhispanic", "blackafricanamericanstatehispanicorlatino", "whitetwoormoreraceshispanicwhiteasiannativeamerican",
                          "blackafricanamericantwo or more races", "blackafricanamericantwoormoreraces", "whitenativehawaiianorotherpacificislander",
                          "blackafricanamericantwoormoreraceswhite", "blackafricanamericanwhite", "whitenativehawaiianorotherpacificislanderblackorafricanamerican",
                          "blackafricanamericanwhiteamericanindian", "blackafricanamericanwhitemultiracial", 
                          "blackafricanamericanwhitemultiracialamericanindianalaskannative", "asianindianoralaskanative",
                          "blackafricanamericanwhitenativehawaiianorasianpacificislander", "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafricanativehawaiianorotherpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislandsasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnamamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                          "blackafricanamericanwhitetwoormoreraces", "blackafricanamhislat", 
                          "blackamericanindian", "blackandafricanamericanamericanindianalaskannative", 
                          "blackandafricanamericanamericanindianalaskannativeasianhispanicorlatinonativehawaiianorotherpacificislanderwhite", 
                          "blackandafricanamericanamericanindianalaskannativewhite", "blackandafricanamericanasian", 
                          "blackandafricanamericanasianwhite", "blackandafricanamericanhispanicorlatino", 
                          "blackandafricanamericanhispanicorlatinowhite", "blackandafricanamericannativehawaiianorotherpacificislander", 
                          "blackandafricanamericanwhite", "blackandasian", "blackandwhite", 
                          "blackasian", "blackasianpacificislander", "blackhispaniwhite", "blackorafricanamericanlatinotwoormoreraceswhite",
                          "blackindiannative", "blacknativeamerican", "blacknativeamericanoralaskannativewhite", 
                          "blacknofhispanic", "blackorafricanamericanamericanindianalaskannative", 
                          "blackorafricanamericanamericanindianoralaskanative", "blackorafricanamericanamericanindianoralaskanativetwoormores", 
                          "blackorafricanamericanasian", "blackorafricanamericanasianjapanese", 
                          "blackorafricanamericaneuropeanasian", "blackorafricanamericanhawaiianorpacificislander", 
                          "blackorafricanamericanhispanicorlatino", "blackorafricanamericanhispanicorlatinotwoormores", 
                          "blackorafricanamericanhispanicorlatinowhite", "blackorafricanamericanmultiracial", 
                          "blackorafricanamericanmultiracialasian", "blackorafricanamericanmultiracialwhite", 
                          "blackorafricanamericannativehawaiianorasianpacificislander", "africanamericanamericanindianalaskannativeasian",
                          "blackorafricanamericannativehawaiianorotherpacificislander", "africanamericanamericanindianalaskannativeasianwhite",
                          "blackorafricanamericannativehawaiianorotherpacificislanderwhite", "africanamericanamericanindianalaskannativehispanicstate",
                          "blackorafricanamericannativehawaiianorpacificislander", "blackorafricanamericannativehawaiianorpacificislanderwhite", 
                          "blackorafricanamericantwoormores", "blackorafricanamericantwoormoreswhite", 
                          "blackorafricanamericanwhite", "blackorafricanamericanwhiteamericanindian", 
                          "blackorafricanamericanwhitemultiracial", "blackorafricanamericanwhitemultiracialamericanindianalaskannative", 
                          "blackorafricanamericanwhitenativehawaiianorasianpacificislander", "nativeamericanorpacificislander",
                          "blackorblackericanwhite", "blackpacificnative", "blackpacificnativeblack", 
                          "blackwhite", "blackwhiteblack", "blackwhites", "brazilianjapanese", "twoormoreracesamericanindianalaskannativeafricanamericanhawaiianpacificislander",
                          "bw", "caucasianafricanamerican", "caucasianasian", "caucasianhispanic", "latinoasiantwoormoreraces",
                          "cblackorafricanamericanhispanicorlatino", "combinationoformorecategories", 
                          "combinationoftwoormorecategories", "diverse", "etwoormoreracesorlatino", 
                          "ewhitelatio", "filipinomexican", "ftwoormoreraces", "ftwoormoreraceshispanicorlatino", 
                          "ftwosormore", "gbrmixed", "gnativehawaiianorotherpacificislanderhispanicorla", 
                          "greekandmexican", "hawaiianorpacificislanderwhite", "hispanicafricanamerican", 
                          "hispanicamericanindianoralaskannative", "hispanicasian", "hispanicblack", 
                          "hispanicblackindiannative", "hispanicblackwhite", "hispaniccaucasian", "whiteamericanindianalaskannativeasian",
                          "hispanichispaniwhiteasian", "hispanicindiannative", "hispaniclatinowhite", 
                          "hispanicmultiracial", "hispanicnativeamericanoralaskannative", "twoormorenlatino",
                          "hispanicorlatino white", "hispanicorlatinoamericanindianoralaskanative", 
                          "hispanicorlatinoasian", "hispanicorlatinoasiantwoormores", "hispanicorlatinoblackafricanamerican", 
                          "hispanicorlatinoblackafricanamericantwoormoreraces", "hispanicorlatinoblackorafricanamerican", 
                          "hispanicorlatinoblackorafricanamericantwoormores", "hispanicorlatinonativehawaiianorotherpacificislanderwhite", 
                          "hispanicorlatinonativehawaiianorpacificislander", "hispanicorlatinonativehawaiianorpacificislanderwhite", 
                          "hispanicorlatinootherwhites", "hispanicorlatinotwoormore", "hispanicorlatinotwoormores", 
                          "hispanicorlatinotwoormoreswhite", "hispanicorlatinowhite", "hispanicorlatinowhiteasian", "blackandafricanamericanlatinonativehawaiianorotherpacificislanderwhite",
                          "hispanicorlatinowhiteonly", "hispanicorlatinowhites", "hispanicorlatinowhitetwoormores", "asianblackorafricanamericanlatinowhite",
                          "hispanicwhite", "hispanicwhiteafricannative", "hispanicwhiteblack", "blackorafricanamericanhispanic",
                          "hispanicwhiteblackpacificnativeasian", "hispanicwhiteindiannative", "asianhispanicwhite","twor",
                          "hispanicwhitepacificnativeasian", "hispaniwhite", "hispwhite", "americanindianoralaskanativenativehawaiianorotherpacificislander",
                          "iamhispaniclatinoaofcubanmexicanpuertoricancentralorsouthamericanorotherspanishcultureororiginregardlessof", "asianhispanicnativehawaiianorrpacificislanderwhite",
                          "idonotwishtofurnishthisinformationtwoormores", "indiannativeasianblackwhite", "blackorafricanamericanhispanicwhite","blackandafricanamericannativehawaiianorrpacificislanderwhite",
                          "indiannativeasianpacificnativewhite", "indiannativeblack", "indiannativeblackwhite", "hispanicnativehawaiianorotherpacificislander","blackandafricanamericanamericanindianalaskannativelatinonativehawaiianorotherpacificislander",
                          "indiannativepacificnativeblackwhite", "indiannativewhite", "interracial", "hispanicblackorafricanamerican","americanindianalaskannativeasiannativehawaiianorrpacificislanderwhite",
                          "latinowhite", "latinwhite", "mix", "mixed", "mixedbw", "mixedtwoormore", "hispanicnativehawaiianorotherpacificislander","americanindianalaskannativenativehawaiianorrpacificislanderwhite",
                          "mixedtwoormoreraces", "mixedtwoormores", "mixedwhiteblackafricanunitedkingdom", "hispanictwoormoreraces","blackandafricanamericanasianhispanicwhite","whitelatinotwoormoreraces",
                          "morethan1choice", "morethanchoice", "morethanone", "mormore", "blackandafricanamericanasianhispanicnativehawaiianorotherpacificislander","blackandafricanamericanasianhispanicnativehawaiianorrpacificislanderwhite",
                          "mr", "mu", "mulits", "mult", "multi", "multirac", "multiracial", "blackandafricanamericanasianhispanicnativehawaiianorotherpacificislanderwhitenofhispanic",
                          "multiracialasiannativehawaiianorasianpacificislander", "multiracialasianwhite", "blackandafricanamericanhispanicnativehawaiianorotherpacificislander","blackandafricanamericanasiannativehawaiianorrpacificislanderwhite",
                          "multiracialblackafricanamerican", "multiracialblackafricanamericanwhite", "blackandafricanamericanasianhispanicwhitenofhispanic","americanindianalaskannativeasianhispanicnativehawaiianorrpacificislanderwhite",
                          "multiracialblackorafricanamerican", "multiracialblackorafricanamericanwhite", "blackandafricanamericanasiannativehawaiianorotherpacificislanderwhitenofhispanic",
                          "multiracialindiannative", "multiracialnativehawaiianorasianpacificislanderblackafricanamericanamericanindianalaskannativeasian", "blackandafricanamericanamericanindianalaskannativehispanicwhite",
                          "multiracialnativehawaiianorasianpacificislanderblackorafricanamericanamericanindianalaskannativeasian", "blackandafricanamericanwhitenofhispanic","blackandafricanamericanhispanicnativehawaiianorrpacificislanderwhite",
                          "multiracialnativehawaiianorasianpacificislanderwhiteasian", "blackafricanamhispanic","ormoresc","twoormoresc","blackandafricanamericanhispanicwhitenofhispanic",
                          "multiracialtwoormoreraces", "multiracialtwoormores", "multiracialwhite", "blackandafricanamericanasiannativehawaiianorotherpacificislander","americanindianalaskannativehispanicnativehawaiianorrpacificislanderwhite",
                          "multiracialwhiteblackafricanamerican", "multiracialwhiteblackorafricanamerican", "blackandafricanamericanasianwhitenofhispanic","ormoresallpersonswhiteoidentifwhitehmoraeoftheabaces",
                          "mults", "mutliracial", "nativeamericanalaskannativetwoormoreraces", "blackandafricanamericanamericanindianalaskannativenativehawaiianorotherpacificislander","latinoasian",
                          "nativeamericanalaskannativetwoormoreraceswhite", "nativeamericanalaskannativetwoormores", "blackandafricanamericanhispanicnativehawaiianorotherpacificislanderwhitenofhispanic",
                          "nativeamericanalaskannativetwoormoreswhite", "nativeamericanalaskannativewhite", "blackandafricanamericanasianhispanic","whiteasianhispanic","blackandafricanamericanamericanindianalaskannativehispanicnativehawaiianorrpacificislanderwhite",
                          "nativeamericanhispanic", "nativeamericanoralaskannativewhite", "twoormoreracesallpersonswhitehmorethanoneoftheabovefives","americanindianoralaskanativeasianhispanicnativehawaiianorpacificislerwhite",
                          "nativeamericanwhite", "nativehawaiianorasianpacificislandermultiracialwhiteblackafricanamerican", "blackandafricanamericannativehawaiianorotherpacificislanderwhitenofhispanic",
                          "nativehawaiianorasianpacificislandermultiracialwhiteblackorafricanamerican", "blackandafricanamericanamericanindianalaskannativewhitenofhispanic","blackandafricanamericanamericanindianalaskannativenativehawaiianorrpacificislanderwhite",
                          "nativehawaiianorasianpacificislanderwhite", "nativehawaiianorotherpacificislanderinanyofthepeoplesofhawaiiguamsamoaorotherpacificislands", "blackandafricanamericannativehawaiianorotherpacificislanderwhite",
                          "nativehawaiianorotherpacificislanderwhite", "nativehawaiianorpacificisland", "blackandafricanamericanamericanindianalaskannativenativehawaiianorotherpacificislanderwhitenofhispanic",
                          "nativehawaiianorpacificislanderblackafricanamerican", "nativehawaiianorpacificislanderblackafricanamericantwoormoreraces", "blackandafricanamericanamericanindianalaskannativeasiannativehawaiianorotherpacificislander",
                          "nativehawaiianorpacificislanderblackorafricanamerican", "nativehawaiianorpacificislanderhispanicorlatino", "americanindianalaskannativehispanicnativehawaiianorotherpacificislanderwhitenofhispanic",
                          "nativehawaiianorpacificislandertwoormoreraces", "nativehawaiianorpacificislandertwoormoreraceswhite", "hawaiianpacificislanderafricanamerican","americanindianalaskannativeasianhispanicnativehawaiianorotherpacificislanderwhite",
                          "nativehawaiianorpacificislanderwhite", "nativehawaiianpacificislanderlatino", "twoormoreracespersonswhitehmorethanoneoftheabovefives","hawaiianpacificislanderasianhispanic","americanindianalaskannativelatinowhite",
                          "nativehawaiianpacificislandertwoormoreraceswhite", "nativehawaiianpacislandino", "blackandafricanamericanamericanindianalaskannativehispanicwhitenofhispanic","americanindianalaskannativeasianhispanicwhite",
                          "nativehawaiianwhite", "ntwoormoreraces", "oneormores", "ormore","twoormore", "blackandafricanamericanamericanindianalaskannativeasianhispanicnativehawaiianorotherpacificislanderwhitenofhispanic",
                          "ormorenisp","twoormorenisp", "ormorenothisp", "twoormorenothisp","ormores","twoormores", "ormoreselected", "ormoresnispanc","twoormoreselected", "twoormoresnispanc", "blackandafricanamericanamericanindianalaskannativehispanicnativehawaiianorotherpacificislanderwhitenofhispanic",
                          "ormoresnothispanc", "ormoresnothispancw312w41a", "ormoretwoormoreraces","twoormoresnothispanc", "twoormoresnothispancw312w41a", "twoormoretwoormoreraces", "blackandafricanamericanamericanindianalaskannativeasianhispanicwhitenofhispanic",
                          "ormoretwoormores", "twoormoretwoormores", "othercombinationoftwoormorecategories", "twoormoreracesallpersonswhitehmorethanoneoftheaboves","whiteasianhawaiianpacificislander","ormoreracespersonwhitehmoraeoftheabovefives","twoormoreracespersonwhitehmoraeoftheabovefives",
                          "pacificnativeblack", "pacificnativeblackwhite", "pacificnativewhite", "blackandafricanamericanamericanindianalaskannativeasiannativehawaiianorotherpacificislanderwhitenofhispanic","blatinowhiteballpersonsofmexicanpuerricancubancentralorsouthamericanorspanishcultureorandofthewhitelatino",
                          "rtwoormoreraces", "stwoormoreraces", "stwoormoreracesv","twoormoreracesv", "ttwoormoreraces", "blackandafricanamericanamericanindianalaskannativehispanicnativehawaiianorotherpacificislander",
                          "twnnonindigeno", "two or more races", "two or more racesallpersonswho", "blackandafricanamericanamericanindianalaskannativehispanic","americanindianalaskannativeafricanamericanhispanicwhite",
                          "two or more racesifyouwhitehtwo or more raceslistedabove", "twomore", "blackandafricanamericanamericanindianalaskannativeasianwhitenofhispanic","ormoreracesallpersonswhowithmoraeoftheabaces","twoormoreracesallpersonswhowithmoraeoftheabaces",
                          "twomores", "twomr", "twoofmores", "twoor", "twoormor", "twoormore", "americanindianalaskanativeblackorafricanamericanwhite","americanindianalaskannativehispanicafricanamericanwhiteasianhawaiianpacificislander",
                          "twoormorefound", "twoormorenothispnlatino", "twoormoreraces", "americanindianoralaskannativeblackorafricanamericantwoormoreraces","americanindianalaskannativeafricanamericanhispanic","americanindianalaskannativenativehawaiianorotherpacificislanderwhite",
                          "twoormoreracesallpersonswhiteo", "twoormoreracesallpersonswhiteowhitehmorethanoneoftheabovesixs", "americanindianalaskannativetwoormoreracesafricanamerican","americanindianalaskannativeasiannativehawaiianorotherpacificislanderwhite",
                          "twoormoreracesallpersonswho", "twoormoreracesforipedseeoaap", "blackorafricanamericanhispaniclatino","hispanicafricanamericanamericanindianalaskannative","americanindianalaskannativehispanicnativehawaiianorotherpacificislanderwhite",
                          "twoormoreraceshislat", "twoormoreraceshispanic", "twoormoreraceshispaniwhite", "blackandafricanamericanamericanindianalaskannativeasianhispanic","americanindianalaskannativeafricanamericanhawaiianpacificislander",
                          "twoormoreraceshispaniwhiteasian", "twoormoreraceshispaniwhitenativeamerican", "blackandafricanamericanamericanindianalaskannativeasianhispanicnativehawaiianorotherpacificislander",
                          "twoormoreraceshispaniwhitess", "twoormoreracesifyouwhitehtwoormoreraceslistedabove", "asianhawaiianpacificislanderhispanic","americanindianoralaskanativeblackorafricanamericannativehawaiianorotherpacificislander",
                          "twoormoreraceslatino", "twoormoreracesn", "twoormoreracesnofhispanic", "asianindianoralaskannative","hispanicwhiteafricanamerican","americanindianalaskannativeasianafricanamerican",
                          "twoormoreracesnonhispanic", "twoormoreracesorlatino", "twoormoreracesorlatinoamericanindianoralaskanativeblackafricanamerican", "americanindianalaskannativewhiteafricanamericanhispanic",
                          "twoormoreracesorlatinoblackafricanamericanamericanindianoralaskanative", "blackandafricanamericanamericanindianalaskannativeasian","americanindianalaskannativetwoormoreraces",
                          "twoormoreracesorlatinoblackafricanamericanamericanindianoralaskanativetwoormoreraces", "asianhawaiianpacificislanderwhite","asianafricanamericanamericanindianalaskannative",
                          "twoormoreracesorlatinoblackorafricanamerican", "twoormoreracesp", "hispanicnativehawaiianorpacifictwoormoreraces","asianafricanamericanhispanic","americanindianalaskannativeafricanamericanasianwhitehawaiianpacificislanderhispanic",
                          "twoormoreracespersonswhiteo", "twoormoreracespersonwhiteowhitehmorethanoneoftheabovefives", "asianhawaiianpacificislanderamericanindianalaskannativewhite","americanindianalaskannativelatino",
                          "twoormoreraceswhite", "twoormoreraceswhitess", "twoormoreracesyoumaybyselectingtwoormoreracesabove", "asianhawaiianpacificislanderamericanindianalaskannativewhite",
                          "twoormoreraices", "twoormores", "twoormoresallpersonswhoidentifywithmorethanoneoftheabovefives", "asianhispanicamericanindianalaskannative","americanindianalaskannativeafricanamericanhispanichawaiianpacificislanderasianwhite",
                          "twoormoresallpersonswhowhitehmorethanoneoftheabovefives", "twoormoresapersonwhoidentifieswithmorethanoneofthefollowingasdefinedabovewhiteblackorafricanamericannativehawaiianorotherpacificislanderasianoramericanindianoralaskanative", 
                          "twoormoreschoosenotto", "twoormoresforipedseeoaap", "twoormoreswhite", "americanindianalaskannativehispanic","asianafricanamericanwhitehawaiianpacificislander","americanindianoralaskanativeblackorafricanamericanlatinotwoormoreraces",
                          "twoors", "twos", "wai", "wandb", "wb", "whiteafrican", "whiteamericanindian", "americanindianalaskannativehispanicnativehawaiianorotherpacificislander","americanindianoralaskanativeblackorafricanamericanlatinotwoormoreraceswhite",
                          "whiteamericanindianalaskanative", "whiteamericanindianalaskannative", "americanindianalaskannativeasiannativehawaiianorotherpacificislander","ormoreracesamericanindianalaskannative","twoormoreracesamericanindianalaskannative",
                          "whiteamericanindianalaskannativeblackafricanamerican", "whiteamericanindianalaskannativeblackorafricanamerican", "asianafricanamericanwhite","twoormoreracespersonwhitehmorethanoneoftheabovefives",
                          "whiteamericanindianoralaskanative", "whiteamericanindianoralaskanativetwoormores", "americanindianalaskannativehispanicwhite","americanindianalaskannativewhiteasian",
                          "whiteamericanindianoralaskannative", "whiteamericanindianoralaskannativeasian", "americanindianalaskannativeasiannativehawaiianorotherpacificislanderwhitenofhispanic",
                          "whiteapersonhavingoriginsinanyoftheoriginalpeoplesofeuropethemiddleeastornorthafrica", "americanindianalaskannativehispanicwhitenofhispanic","whitetwoormoreraceshispanicwhitenativeamerican",
                          "whiteasian", "whiteasianindiannative", "whiteasianmultiracial", "africanamericanwhiteasian","americanindianalaskannativenativehawaiianorotherpacificislander",
                          "whiteasianorpacificislander", "whiteasianss", "whiteasiantwoormores", "americanindianalaskannativeafricanamerican","americanindianoralaskanativeasianblackorafricanamericanhispanicnativehawaiianorotherpacificislanderunitedstatesofame",
                          "whiteblack", "whiteblackafricanamerican", "whiteblackafricanamericanamericanindianoralaskanative", "americanindianalaskannativewhiteafricanamerican",
                          "whiteblackafricanamericanamericanindianoralaskanativetwoormoreraces", "americanindianalaskannativeafricanamericanasian","americanindianoralaskanativeasianhispanicnativehawaiianorotherpacificislanderwhite",
                          "whiteblackafricanamericanasian", "whiteblackafricanamericanasianamericanindianoralaskanative", "americanindianalaskannativenativehawaiianorotherpacificislanderwhitenofhispanic",
                          "whiteblackafricanamericanmultiracial", "whiteblackafricanamericantwoormoreraces", "americanindianalaskannativeasianhispanicnativehawaiianorotherpacificislander",
                          "whiteblackasian", "whiteblackindiannative", "whiteblackorafricanamerican", "americanindianalaskannativeafricanamericanwhite","americanindianoralaskanativeasiannativehawaiianorotherpacificislanderwhite",
                          "whiteblackorafricanamericanamericanindianoralaskanativetwoormores", "africanamericanwhiteamericanindianalaskannative","asianafricanamerican",
                          "whiteblackorafricanamericanmultiracial", "whiteblackorafricanamericantwoormores", "americanindianalaskannativeasianhispanic","americanindianalaskanativetwoblackorafricanamericanwhite",
                          "whiteblackpacificnative", "whitecaucasianinanyoftheoriginalpeoplesofeuropethemiddleeastornorthafrica", "americanindianalaskannativewhitenofhispanic",
                          "whitehiispanic", "whitehislat", "whitehispanicorlatino", "whitehispanicorlatinotwoormores", "americanindianalaskannativeasianhispanicnativehawaiianorotherpacificislanderwhitenofhispanic",
                          "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricablackorafricanamericaninanyoftheblackracialgroupsofafricanativehawaiianorotherpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislandsasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnamamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhiteomaintaintribalaffiliationorcommunityattachment", 
                          "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricanativehawaiianorotherpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislands", 
                          "whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricanativehawaiianorotherpacificislanderinanyofthealpeoplesofhawaiiguamsamoaorpacificislandsasianinanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislandsthailandandvietnam", 
                          "whiteinanyoftheoriginalpeoplesofeuropethemiddleeastornorthafrica", "africanamericanamericanindianalaskannativewhite","africanamericanhawaiianpacificislanderwhite",
                          "whiteindiannative", "whitelatino", "whitemultiracial", "whitemultiracialasian", "africanamericanhawaiianpacificislander","americanindianalaskannativehispanicafricanamerican",
                          "whitemultiracialblackafricanamerican", "whitemultiracialblackorafricanamerican", "africanamericanhispanicamericanindianalaskannative","americanindianalaskannativehispanicafricanamericanwhite",
                          "whitenativeamerican", "whitenativehawaiianorasianpacificislander", "africanamericanasian","americanindianalaskannativeasianhispanicwhitenofhispanic",
                          "whitenativehawaiianorpacificislander", "whitenativehawaiianorpacificislanderasianamericanindianoralaskanative", "twoormoreracesafricanamerican",
                          "whitenativehawaiianorpacificislanderblackafricanamerican", "whitenativehawaiianorpacificislanderhispanicorlatinoamericanindianoralaskanativetwoormoreraces", 
                          "whitenofhispanic", "whitepacificnative", "whitepacificnativeasian", "blackorafricanamericanandhispaniclatino","africanamericanhispanicamericanindianalaskannativewhite",
                          "whitetwoormoreraces", "whitetwoormoreraceshispanics", "whitetwoormoreraceshispanictwoormoreracess", "americanindianalaskannativeasianwhitenofhispanic",
                          "whitetwoormoreraceshispanicwhite", "whitetwoormoreraceshispanicwhiteasian", "africanamericanasianwhite","hispanicafricanamericanamericanindianalaskannative",
                          "whitetwoormoreraceshispanicwhitenativeamericans", "whitetwoormoreraceslatino", "africanamericanhispanicwhite","multiracialtwo","htwo",
                          "whitetwoormoreracesnativeamerican", "whitetwoormoreraceswhite", "asianindiansubcontinentblack","multitwos","blackwhitetwos","twosb",
                          "whitetwoormoreraceswhitenativeamerican", "whitetwoormores"),
  "NA" = c("NATOEVERYONE", "notspecified", "memberofvisibleminoritycanada")
)




race_list <- mapply(c, 
                    (race_list_short <- lapply(race_list_short, sort)), 
                    (extra_race_vals_list <- lapply(
                      list("american indian or alaska native" = c("ai", "nata", "5", "05", "005", "five", "an", "ami"), 
                           "asian" = c("in", "4", "04", "004", "four", "as"), 
                           "black or african american" = c("b", "2", "02", "002", "two"), 
                           "hispanic or latino" = c("h", "3", "03", "003", "three", "hi", "hp"), 
                           "native hawaiian or other pacific islander" = c("6", "06", "006", "six", "n", "p"), 
                           "white" = c("w", "wi", "ww", "1", "01", "001", "one"), 
                           "two or more races" = c("m", "bnw", "wbh", "wbhn", "wbn", "nla", "bhn", "ash", "nb", "bh", "mul", "7", "07", "007",
                                                   "otwos","ah","bipw","pw","hiw","apw","bhiw","bhw","bn","etwo","ltwo","twoa","taltwo","qaltwo",
                                                   "seven", "t", "two", "aw", "hn", "hw", "anh", "ahw", "aiw", "wn", "wna", "hb", "abiw", "ab","ahp"), 
                           "NA" = c("tals", "VALUE","NA", 
                                    "nr","ng",
                                    "na", "na", "EVENTUALLYWEWILLDELETEITBUTWENEEDTHEPLACEHOLDER","ho", "qal", "dti", "fax", "perfer", "ofcolor", "isclose","nolatino", "oth", "preferrednot","icurrentlyworkatforest", "nmin", 
                                    "mm","min", "ielectnot", "frxcom","onlinejobsite", "abal", "thomas","smith", "notdeclaringunitedkingdom", "bragray","poc", "canvisibleminority", "remove", 
                                    "nfalt", "missingor", "single", "ni", "aeorai", "visual", "sbentivegna", "does", "dn", "sb", "nd", "bkorkuch", "sgps", "didindentify", "noinformation", "declinded", "refe", "sg", "employermt", "dnr", "leftblank", 
                                    "iama", "osed", "uknown", "anchoice", "unnown", "apprentice", "gbrnotdeclared", "electrician", "lack", "followstepscompleteeeoinformation", "notinformation", "cannoneofthecategoriesapply", 
                                    "report", "missingblank", "br", "tmr", "undefined", "respond", "needhrreview", "say", "prefer", "observationneeded", "ielect", "multiples", "thispanicquestion",
                                    "aiisclose", "true", "expatriates", "iisclose", "notsupplied", "notselected", "choose", "william", "s", "answeredyeshispaniclatinoquestion","unotknow",
                                    "to", "do", "nonpoc", "state", "nonhispanic", "id", "furnish", "twas", "answer", "g", "aa", "refed", "yes", "un", "ukn", "ud", "u", "c", "y", "to", "sv", "r", "ndr", "or", "did",
                                    "avisual", "not", "non", "entify", "not", "nanonnon", "ola", "oim", "oca", "gyr", "s", "diversity", "nothispanicorlanito", "mothispanic", "welder", "hire", "un", "oai", 
                                    "eeob", "e", "empty", "none", "orm", "desc", "gender", "gmna", "minority", "no", "ns", "other", "q", "o", "blank", "8", "9", "10", "11", "0", "u", "a", "x", "z", "na", "null", "unk", "answer", 
                                    "d", "o", "nralien", "tr", "ax", "dw", "dx", "i", "ad", "iw", "nspec", "ow", "bi", "biw", "unknowen", "f","ed","monstercom",
                                    "didnotdisclose", "unknownphone", "notdelcared", "didnotwishtoanswer", "iamnothispaniclatinoa", "nothispaniclatino", "nothispaniclatino"))
                      , sort)), 
                    SIMPLIFY=FALSE)


partials <- c("african", "hispanic", "americanindian", "american indian", "hawaiian", "pacificislander", "hispanic",
              "pacific islander", "indian", "female", "woman", "women", "feminine", "masculine", "white", "black", "black or african american", "blackorafricanamerican", "hispanic or latino", "hispanicorlatino", "asian",
              "americanindianoralaskanative", "american indian or alaska native", "nativehawaiianorotherpacificislander", 
              "native hawaiian or other pacific islander", "twoormoreraces", "two or more races", 
              "female", "male", "fem", "man", "woman", "men", "women", "girl", "boy", "feminine", "masculine", "mannlich")

preprocess_names_other_getridofstrregex <- paste0(
  c("macro", "staffmark", "stafffmark","information", "informatio", "informati", "informat", "informa", "inform", "infor", "info",
    "ofor", "ofor", "worker", "fake", "reportingformat", "public", "describe", "description", "desc", "discription", "renglish", "english", "other",
    "accurate", "ofor", "eem", "eeo", "forms", "form", "applicant", "legal", "employee", "paygapanalysis", "lastfirst", "selfidentification", "taleo", "detail",
    "promoted", "specify", "iprefernottoprovide",
    "text", "fnf", "complete", "blackor", "itly", "whatisyour", "corrected", "iparttimeion", "lication", "background", "jacksonlewis", "description",
    "ifno", "hmlm", "orininator", "former", "localesensitive", "makeyourselection", "upper", "lower",
    "grp", "current", "applicant", "person", "primary", "candidate", "app", "spriden", "spbpers", "ipeds", "new", "individual", "indiv", "ind", "ming",
    "reported", "preferred", "optional", "recipient", "legal", "previous", "previou", "secondary", "second", "avisu", "translate", "workforce", "horizon",
    "start", "candidate", "unitedstatesglobal", "information", "contact", "identified", "original", "origin", "orig", "recruitingworkflowprofile", "label",
    "wfn", "fixed", "updated", "fields", "donor", "colleague", "legacy", "alpha", "expand", "combined", "dra", "full", "jl", "pleaseselectyour", "further", 
    "candidate", "candidat", "candid", "candi", "cand", "groupof", "knowna", "dnu", "questioncode", "ofprior",
    "associate", "fsu", "value", "emp", "adminentered", "reported", "selfentered", "essee", "final", "sterms", "ofindividual", "rtiption", "codemf",
    "same", "identification", "affiliations", "affiliation", "ination", "used", "translation", "deccription", "definition", "ination", "dentification", "norhispanicorlat", "nisorlat", "nispnc",
    "category", "voluntaryselfid", "codeaap", "morf", "haassoc", "asofnov", "further", "ming", "cd$", "^hrm", "^loy", "^lfull"), 
  collapse = "|")


turntoracestrregex <- paste0(
  c("raceethnicity", "raceethn$", "nativehawaiianotherpacificislander", 
    "xethnicity", "ethnicgroup", "ethnicity", "ethnicit", "ethnici", 
    "ethnic", "ethni", "ethno", "ethn", "eth$", "racecodes", "racecode", 
    "race", "raceno", "twoormoreraces", "africanamerican", "raceance", 
    "racefor", "racecategory", "racer$", "raceic", "racehr", "eeorace", 
    "eerace", "lwrace", "racename", "namerace", "raceid", "psrace", 
    "^srace", "racerace", "racesee", "raceitly", "raceiparttimeion", 
    "drarace", "inictyname", "racety", "racedscr", "white", "thispanic", 
    "hispaniclatino", "alrace", "orrace", "ifrace", "twomoreraces", 
    "racepre$", "racedecr", "raceal", "raceip", "raceraceentity", 
    "racecd", "racep", "allrace", "finrace", "eprace", "raceer$", 
    "adprace", "^erace", "asian", "black", "americanindianalaskannative", 
    "^lrace", "^rce$", "ethrace", "racey$", "racex$", "racecitiy", 
    "raceand", "andrace", "^prace", "racedistributiongraph", "racestaffmark", 
    "racedsc", "raceiannative", "raceinev", "loyrace", "raceiy", 
    "racems", "raceminority", "nativehawaiianorotherpacificislander", 
    "nativeamerican", "hispanic", "raceming", "furtherinforace", 
    "crace", "prace", "srace", "racekey", "lrace", "races$", "rcename", 
    "raceid$"), 
  collapse="|")


turntogenderstrregex <- paste0(c("female", "male$", "^male", "adpgender", "genderer$", "gendercodes", 
                                 "gendercode", "gendery$", "genderx$", "gnder", "gndr", "gender", 
                                 "sex", "hpigenderr", "^sgender", "^xgender", "psgender", "^lgender", 
                                 "gendermf", "gendereeo", "genderr$", "genderhr", "aagender", 
                                 "algender", "genderkey", "genderid", "apgender", "submissiongenderternal", 
                                 "gendercategory", "eegender", "eeogender", "argender", "^cgender", 
                                 "pmgender", "mmaleffemale", "^fgender", "^mgender", "hpigender", 
                                 "genderfield", "gendername", "genders$", "^pgender", "$pgender", 
                                 "clmntgender", "^epgender", "emgender", "revisedgender", "negender", 
                                 "genderming", "furthergender", "fieldgender", "genderaap", "genderand", 
                                 "andgender", "fgender", "gendercd", "mgender", "^egender", "gendergendergender", 
                                 "gendergender", "namegender"), collapse="|")


recode_na_getridofstrregex <- paste0(
  c("wishto", "vicepresidents", "vicepresident", "unspecified", 
    "unreported", "unkown", "unknwon", "unknown", "unitedstatesofamerica", 
    "united states of america", "undisclosed", "undeclared", "undata", 
    "unallocated", "transporter", "tostate", "toself", "toanswer", "iidentif$",
    "thisquestion", "thisinformation", "thisinfo", "testtest", "technologyl", 
    "technology", "sysco", "supportworkers", "strategy", "specify", 
    "specialist", "socialwork", "shouldbe", "seniorassociate", "selected", 
    "security", "sdeclinetov", "rmation", "respiratory", "researchers", 
    "researcher", "research", "requisition", "rehabilitation", "rathernot", 
    "questions", "providisinfo", "provided", "provide", "programs", "sidentifv",
    "program", "professofafricanastudies", "products", "productive", 
    "production", "product", "priorworker", "presidents", "president", 
    "presented", "prefernot", "phonescreen", "phoneinterview", "pharmacy", 
    "personwasmanuallentered", "parttime", "orlatino", "officeservices", 
    "notspecified", "notself", "notrmation$", "notofhispanicorigin", 
    "notinsap", "notidentified", "nothispnc$", "nothispanicorlatino", "nothispanicc$","nothispanicnlatino",
    "nothispanicorlatin$", "nothispanicorlat$", "nothispanicorla$", "nonhispanicorlatio$",
    "nothispanicorl$", "nothispanicorhispanic", "nothispanicorhispan$", 
    "nothispanicorhispa$", "nothispanicorhisp$", "nothispanicorhis$", 
    "nothispaniclatino$", "nothispaniclatin$", "nothispaniclat$", 
    "nothispanicl$", "nothispanic$", "nothisorlat$", "notentered", 
    "notanswer", "norhispanicorlat$", "nonhispanicorhispanic$", "nonhispanicorhispan$", 
    "nonhispanicorhispa$", "nonhispanicorhisp$", "noidentif$", "noapplication", 
    "nisporlat$", "newjob", "neverinterviewed", "never", "morequalifiedcandidates", 
    "medical", "marketing", "managersap", "managers", "manager", 
    "management", "laboratory", "jobfairs", "jobfair", "irespond", 
    "iprefernot", "iprefer", "interviews", "interviewing", "interviewer", 
    "interviewee", "interviewed", "interview", "international", "internal", 
    "inanyofthealpeoplesofthefareastsoutheastasiathesubcontinentincludingfexamplecambodiachinaindiajapankeamalaysiapakistanthephilippineisls", 
    "inanyofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineisls$", 
    "inactive", "ielectnotto", "idontanswer", "idonottoprovidethisinformation", 
    "idonottoanswer", "idonot", "identify", "identified", "identification", 
    "ichoosenot", "ichoose", "hiring", "generalmanagement", "function", 
    "fulltime", "extendedprofile", "employing", "employer", "employees", 
    "employeename", "employee", "employed", "email", "eeob", "dtoidentif$", 
    "donottoanswer", "donotselectthisoptionifoutoour$", "donotselectthisoptionifouour", 
    "doesnot", "distributiongraph", "disclosed", "disclose", "directors", 
    "directorof", "director", "diiweekly", "diibiweekly", "didnot", 
    "development", "description", "departments", "department", "declinto", 
    "declined", "decline", "databases", "database", "creativedesign", "missinlank",
    "corporate", "consulting", "consultants", "consultant", "chosenot", 
    "choosenotto", "choosenot", "chieffinancialofficer", "centralretail", 
    "businesses", "business", "benefitsadminretail", "behavioralhealth", 
    "bartenders", "bartender", "available", "atthistime", "associates", "none listed",
    "associate", "assistants", "assistant", "applicant", "apersonwhiteoidentifieswhitehmoraeofthefollowingasdefinedabovewhiteblackorafricanamericannativehawaiianorpacificislerasianoramericanindianoralaskanative$", 
    "apersonhavingoriginsinanyoftheoriginalpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalaysiapakistanthephilippineislsthailvietnam$", 
    "apersonhavingoriginsinanyoftheoriginalpeoplesofeuropemiddleeastornorthafrica$", "applicationreceivedtoolate",
    "answer$", "anonymized", "advancedpractice", "administrative", "^stoidentifv","^tospecif$","newspapermagazinead",
    "accounting", "â", "^toidentif$", "^stov$", "^specif$", "^snav$", "universityjobposting","craigslist","monstercom","universitjobposting",
    "^self$", "^oput$", "^nottom$", "^notto$", "^nottmation", "^nothis$", "notrpted","information", "followstepscompleteeeoinfo","technologl","newspapermagazinead",
    "^noidentif$", "^noident>\\", "^iself$", "^identif$", "^id$", "donotidentif$","noidentidentif$","simplhired","$rnotdeclared","^hourl$","applicationreceivedolate","temporaroncall",
    "^fillin$", "^donot$", "^answer$", "^answer", "semimonthly", "wwwindeedcom", "careerfair", "internet", "cidatetakesteps", 
    "morequalifiedcidate", "technolog$", "monthly","identif$",
    "followstepscompleteeeoinfo", "followstepstocompleteeeoinfo", "notclassified", "ziprecruiter", "lackseducationrequiredfortheposition", "pharmadiversit$", "americanjobexchange"),
  collapse = "|")



recode_na_list <- list(
  "NA" = c(" ", ",", "active", "admin", "agency", "aliennonresident", 
           "american", "anonymized", "answe", "answer1", "answeredhispaniclatinoquestion", 
           "arbpayroll", "blackafricanamerican", "brazilmonthly", "candidateeeo", 
           "centralislipny", "choosenotrodisclose", "choosenottodisclose", 
           "choosenottoidentify", "chosenottodisclose", "chosenottoselfidentify", 
           "claimingdisabilitystat", "clericalunitclerk", "cocreatepayroll", 
           "coders", "comalepanyjobboard", "dateofinterview", "declin", 
           "decline", "declined", "declinedtoanswer", "declinedtoselfidentify", 
           "declinedtostate", "declineselfidentification", "declinetoanswer", 
           "declinetodisclose", "declinetoid", "declinetoidentify", "declinetoselfidentify", 
           "declinetostate", "declintoself", "demoapplication", "deputyprojectmanager", 
           "description", "didnotanswer", "didnotdisclose", "didnotintvwell", 
           "didnotrespond", "didnotselfid", "didnotspecify", "didnotwishtoanswer", 
           "didntid", "directsourcedlinkedin", "doesindentify", "doesnot", 
           "doesnotwishtoidentify", "doesnotwishtoprovide", "doesnt", "donotidentify", 
           "donotselectthisoptionifyoutoyour", "donotselectthisoptionifyouyour", 
           "donotwishto", "donotwishtoanswer", "duplicate", "eeo", "eeob", 
           "eeoc", "emailedsurvey", "ethnicorigin", "eval", "EVENTUALLYWEWILLDELETEITBUTWENEEDTHEPLACEHOLDER", 
           "executive", "external", "fax", "firstname", "fisbiweeklyhourly", 
           "fissemimonthly", "flsaid", "ft", "ftpt", "fulltime", "functiontesttechnician", 
           "gender", "grp", "ichoosenotoselfidentify", "ichoosenotprovideinformation", 
           "ichoosenotselfidentify", "ichoosenottodisclose", "ichoosenottodisclosethisinformation", 
           "ichoosenottoidentify", "ichoosenottoprovideinformation", "ichoosenottoprovidemy", 
           "ichoosenottorespond", "ichoosenottoselfid", "ichoosenottoselfidenitfy", 
           "ichoosenottoselfidentify", "ichoosenottoselfidentifyatthistime", 
           "ideclinetoanswer", "ideclinetorespond", "ideclinetoselfidentify", 
           "identifiedasunknown", "idonottoanswer", "idonottoprovidethisinformation", 
           "idonottoself", "idonotwishtofurnishthisinformation", "idonotwishtoprovidethisinfo", 
           "idonotwishtoprovidethisinformation", "idonotwishtoselfidentify", 
           "idonotwishtoselfidentifyatthistime", "idont", "idonttoanswer", 
           "idontwishtoanswer", "ielectnotto", "ielectnottoselfidentify", 
           "ignore", "incomplete", "internal", "interviewcancelled", "interviewdate", 
           "iprefernotanswer", "iprefernottoanswer", "iprefernottodisclose", 
           "jonesboroar", "lacksrequiredqualifications", "lastname", "management", 
           "materialcontrolattendant", "miami", "missing", "missingor", 
           "more", "morequalifiedcandidates", "mtgbankingdivisionadmin", 
           "mtspayrollus", "multiple", "nainquire", "nameofindividualcompletingform", 
           "nanon", "nanonnotapplicablenon", "ncsd", "newyorkny", "nhispanic", 
           "nnotspecified", "noanswer", "noanswerseeinperson", "noanswerselected", 
           "noapplicablecode", "noapplication", "nodatacollected", "nodisclosure", 
           "noentify", "noid", "noident", "noidentified", "noidentify", 
           "noinfo", "noinformation", "nondisclosed", "nondisclosure", "none", 
           "nonegiven", "nonelisted", "noneprovided", "nonespecified", "nonreported", 
           "nonresalien", "nonresidentalien", "noreponse", "noresponse", 
           "norinfo", "nortspecify", "noselection", "noselfid", "notappearinginhrm", 
           "notapplicable", "notapplicablenon", "notassigned", "notavailable", 
           "notcaptured", "notcollected", "notcompleted", "notdeclared", 
           "notdefinedinsap", "notdesignatedinsap", "notdisabledofccp", 
           "notdisclosed", "notentered", "notgiven", "nothispanic", "notidentified", 
           "notindicated", "notinformation", "notinsap", "notknown", "notlisted", 
           "notlistedinsap", "notprovided", "notrecorded", "notreported", 
           "notreporting", "notreturned", "notself", "notspec", "notspecif", 
           "notspecified", "notspecifiedinactive", "notspecify", "notspecnotspecified", 
           "notthisquestion", "null", "nurse", "nursesaide", "nursing", 
           "nyitbeijing", "nyitnanjing", "nyitvancouver", "octagmktng", 
           "oldwestburyny", "optedout", "optout", "oput", "orderly", "other", 
           "parttime", "personal banker", "phonescreeen", "prefernotsay", 
           "prefernottoanswer", "prefernottodisclose", "prefernottosay", 
           "prefernottospecify", "prefers", "prefersnottoanswer", "professional", 
           "professionals", "professor", "provide", "providisinfo", "providisinformation", 
           "pt", "ptft", "racialcategory", "railcartechnician", "rathernot", 
           "referral", "referralsource", "refetodisclose", "refused", "rehire", 
           "reloagent", "requisitionfilledresumenotreviewed", "requisitionsid", 
           "rmation", "sales", "salesworkers", "sandiego", "sanjose", "sdeclinetov", 
           "sentemail", "service", "sgother", "snav", "staffnurse", "stov", 
           "technical", "thisquestion", "totals", "ukn", "unavail", "undata", 
           "undeclared", "undisclosed", "unknown", "unknowndeclinedtodisclose", 
           "unknownpersonwasmanuallyentered", "unknwn", "unkown", "unkwn", 
           "unotknow", "unspec", "unspecified", "usbiweekly", "xx")
)




recode_race_getridofstrregex <- paste0(c("yes", "wish", "us", "unitedstatesofamerica", "to", "sgp","state",
                                         "self", "selected", "region", "raceethnicity", "race", "other", 
                                         "othe", "oth$", "origins", "origin", "only", "ofany", "obsolete", 
                                         "notofhispanic$", "notlatino$", "nothisporlatino", "nothispnc$", 
                                         "nothispanicorlatino", "nothispanicorl$", "nothispanicor$", "nothispanicoflatino", 
                                         "nothispaniclatinononhispanic$", "nothispaniclatino", "nothispanichispanic$", 
                                         "nothispanichis$", "nothispaniceorlatino", "nothispanic$", "nothislatino$", 
                                         "nothislat$", "nothis$", "norhispanic$", "norhislatino$", "norhis$", 
                                         "noofhispanic$", "nonwhite", "nonhispanicorlatino", "nonhispanicor$", 
                                         "nonhispaniclatino", "nonhispanichispanic$", "nonhispanichis$", 
                                         "nonhispanic$", "nonhislat$", "nonhis$", "nonexempttoexempt", 
                                         "nofhispanic$", "nispnlatino$", "nisplatino", "nisplat$", "nispanicorlatino", 
                                         "nispanic$", "nispanc$", "nislatino$", "nislatin$", "nislat$", 
                                         # "males", "male", 
                                         "lessthan", "ï", "hours", "heritage", "half", 
                                         "gender", "gb", "furnish", 
                                         # "females", "female", 
                                         "ethnicity", 
                                         "ethnic", "eorlatino", "dta", "donottoself", "donotto", "cmty", 
                                         "apersonhavingorigins", "alls", "^nhispanic$", "^dstate"), 
                                       collapse="|")



recode_gender_getridofstrregex <- paste0(c("gender", "sexis", "earnings", "sex", "tospecife", "lessthan", "over", "other", 
                                           "inanyofthealpeoplesofthefareastsoutheastasiathesubcontinentincludingfexamplecambodiachinaindiajapankeamalaysiapakistanthephilippineisls",
                                           "noselfid", "assume", "senior", "junior", "applicantselected","other", "non","unotknow","y","ukn",
                                           "apersonhavingiginsinanyoftheiginalpeoplesofthefareastsoutheastasiathesubcontinentincludingfexamplecambodiachinaindiajapankeamalaysiapakistanthephilippineislsthailvietnam",
                                           "and", "selected", "tonot", "self", "professionals", "professional", "toidentif$",
                                           # "afircanamerican", "africanamerican", "hispanic", "africanamerican", "amerindalas", #"ornotorlatino", "notorlatino", "ormoreraces",# "white", "black", "non", "hisanic", "asian", "nativeamerican", # "hisanic",
                                           "ethnicity", "ethnicit", "ethnici", "ethnic", "ethni", "ethn", "eth", "donotselectthisoptionifyouyour", "apersonhavingiginsinanyoftheiginalpeoplesofeuropemiddleeastnthafrica"
), 
collapse="|")




dfsample <- data.frame(gender = c("male", "female", "female", 
                                  "female", "female", "female", 
                                  "male", "female", "male", 
                                  "male", "male", "male", 
                                  "female", "female", "female",
                                  "female", "female", "female", 
                                  "female", "female", "male",
                                  "male"), 
                       race = c("white", "white", 
                                "native hawaiian or other pacific islander", 
                                "black or african american", 
                                "american indian or alaska native", 
                                "hispanic or latino", 
                                "asian",
                                "black or african american", 
                                "american indian or alaska native", 
                                "white", 
                                "two or more races", "two or more races", 
                                "american indian or alaska native",
                                "american indian or alaska native", 
                                "native hawaiian or other pacific islander",
                                "hispanic or latino", 
                                "hispanic or latino",
                                "white", "white", 
                                "hispanic or latino", 
                                "hispanic or latino",
                                "native hawaiian or other pacific islander"), 
                       name = c("jason o'rawe", "samantha karlaina rhoads", "keisha castle-hughes", 
                                "oprah winfrey", "shoni schimmel", "alexandria ocasio-cortez", 
                                "kendrick kang-joh jeong","purdie greenaway, valerie", "silverheels, jay", 
                                "jadrian charles guy", "jordan peele", "keegan-michael key", 
                                "davids, sharice", "deb haaland", "dinah jane hansen",
                                "ochoa, ellen", "sonia sotomayor", "ruth bader ginsburg", 
                                "natalia nikolaevna zakharenko", "kahlo, frida", "diego rivera",
                                "momoa, jason"), stringsAsFactors = F)


dfincase <- data.frame(name=c('charlene teters', 'sandra sunrising osawa'), 
                       firstname=c('charlene', 'sandra sunrising'), 
                       lastname=c('teters', 'osawa'), 
                       gender=c('female', 'female'), 
                       race=c('american indian or alaska native', 'american indian or alaska native'), 
                       stringsAsFactors = F)





types <- c('ADDL COMP', 'APPLICANTS', 'NEW HIRES', 'PROMOTIONS', 'TERMINATIONS', 'WORKFORCE')






# functions.R



#' A function to paste a vector in a regex way with '|' partial
#'
#' This function allows you to paste a vector in a regex way with '|' partial
#' @export
#' @examples
#' paste_regex_partial()
paste_regex_partial <- function(v, collapse='|') paste0(v, collapse=collapse)


#' A function to paste a vector in a regex way with '|' exact with '^' in front and '$' in back
#'
#' This function allows you to paste a vector in a regex way with '|' exact with '^' in front and '$' in back
#' @export
#' @examples
#' paste_regex_exact()
paste_regex_exact <- function(v, collapse='|') paste0('^', v, '$', collapse=collapse)


#' A function to paste a vector in a regex way with '|' options for exact or partial
#'
#' This function allows you to paste a vector in a regex way with '|' options for exact or partial
#' @export
#' @examples
#' paste_regex(v, collapse='|', exact=F)
paste_regex <- function(v, collapse='|', exact=F){
  if(exact) paste_regex_exact(v)
  else paste_regex_partial(v)
}


#' A function to paste a vector in a regex way with '|'
#'
#' This function allows you to 
#' @export
#' @examples
#' drop_repeat_cols(d, fromall=F, fromlast=F, fromfirst=F)
drop_repeat_cols <- function(d, fromall=F, fromlast=F, fromfirst=F){
  df <- d
  lastd <- duplicated(as.list(d), fromLast=F)
  firstd <- duplicated(as.list(d), fromLast=T)
  if(fromall) df <- d[ ! as.logical(firstd + lastd) ]
  if(fromlast) df <- d[ ! firstd ]
  if(fromfirst) df <- d[ ! lastd ]
  df
}

#' A function to ask if it's a character or factor logical
#'
#' This function allows you to
#' @export
#' @examples
#' is.factorchar()
is.factorchar <- is.charorfact <- function(x) ifelse(is.character(x) | is.factor(x), T, F)

#' A Function
#'
#' This function allows you to
#' @export
#' @examples
#' dply_all()
dply_all <- function(x, fun) data.frame(lapply(x, fun), stringsAsFactors = F)


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' lapply_on_nonumeric()
lapply_on_nonumeric <- function(df, fun) {
  lapply(df, function(v){
    if(is.character(v) | is.factor(v)) v <- fun(v); v
  })
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' dply_nonnumeric()
dply_nonnumeric <- function(x, fun) data.frame(lapply_on_nonumeric(x, fun), stringsAsFactors = F)
dply <- function(df, fun, num=T){
  if(num) df <- dply_all(df, fun=fun); df
  if(!num) df <- dply_nonnumeric(df, fun=fun); df
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ply()
ply <- function(x, fun, num=T){
  if(is.data.frame(x)) x <- dply(x, fun=fun, num=num); x
  if(is.character(x)|is.factor(x)|is.numeric(x)|is.vector(x)) x <- fun(x); x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' dply_to_title()
dply_to_title <- string_to_title <- vecs_to_title <- to_title <- function(x, stringsAsFactors=T) {
  if(is.data.frame(x)|is.list(x)) x <- lapply(x, function(xx){
    if(is.character(xx)|is.factor(xx)) xx <- stringr::str_to_title(xx); xx
  }) %>% data.frame(., stringsAsFactors=stringsAsFactors); x
  if(is.vector(x)|is.array(x)|is.character(x)|is.factor(x)) x <- stringr::str_to_title(x); x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' gsub_ply()
gsub_ply <- function(from, to, x, ignore.case=T, num=T) ply(x, function(xx) gsub(from, to, xx, ignore.case=ignore.case), num=num)





#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' not_all_na()
not_all_na <- function(x) any(!is.na(x))

`%>%` <- magrittr::`%>%` 
`%<>%` <- magrittr::`%<>%`

## same as list.files() except default recursive=T and full.names=T
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' list_files()
list_files <- function(p, pattern=NULL, recursive=T, full.names=T) list.files(p, pattern=pattern, recursive=recursive, full.names=full.names)


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' pkg()
## load and/or install package first!
pkg <- function (package1, ...) {
  packages <- c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) 
      do.call(library, list(package))
    else {
      install.packages(package, 
                       repos = c("https://cloud.r-project.org", 
                                 "http://owi.usgs.gov/R/"), dependencies = NA, 
                       type = getOption("pkgType"))
      do.call(library, list(package))
    }
  }
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_data_frame()
try_data_frame <- function(x) tryCatch(data.frame(x., stringsAsFactors = F), error=function(e) x)


#' A function just like read_excel but better!
#'
#' This function allows you to 
#' @export
#' @examples
#' readexcel()
readexcel <- function(file, bindsheets=F){
  sheets <- readxl::excel_sheets(file)
  d <- lapply(sheets, function(sheet) readxl::read_excel(file, sheet))
  names(d) <- sheets
  d <- try_combine_compact(d)
  d <- drop_empty(d)
  if(bindsheets) d <- dplyr::bind_rows(d)
  d
}

#' A function to read excel file
#'
#' This function allows you to 
#' @export
#' @examples
#' try_read_excel()
try_read_excel <- function(file, bindsheets=F) tryCatch(readexcel(file, bindsheets=bindsheets), error=function(e) NULL)


#' A function to read excel files
#'
#' This function allows you to 
#' @export
#' @examples
#' read_excels()
read_excels <- function(filelist, bindsheets=F, bindrows=F, simplif=T){
  d <- lapply(filelist, function(x) try_read_excel(x, bindsheets=bindsheets))
  if(simplif) d <- try_combine_compact(d) %>% drop_empty()
  if(bindrows) d <- dplyr::bind_rows(d)
  d
}

#' A function to a read csv file
#'
#' This function allows you to 
#' @export
#' @examples
#' try_read_csv()
try_read_csv <- function(file){
  d <- tryCatch(read.csv(file), error=function(e) file)
  if(is.character(d)) d <- tryCatch(readr::read_csv(file), error=function(e) NULL)
  d
}


#' A function to read csv files
#'
#' This function allows you to 
#' @export
#' @examples
#' read_csvs()
read_csvs <- function(filelist, bindrows=F, simplif=T){
  d <- lapply(filelist, function(x) try_read_csv(x))
  if(simplif) d <- try_combine_compact(d) %>% drop_empty()
  if(bindrows) d <- dplyr::bind_rows(d)
  d
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_read_rda()
try_read_rda <- function(file) d <- tryCatch(get(load(file)), error=function(e) NULL)

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_read_feather()
try_read_feather <- function(file) d <- tryCatch(feather::read_feather(file), error=function(e) NULL)

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_read_table()
try_read_table <- function(f) tryCatch(data.frame(readr::read_table(f), stringsAsFactors = F), error=function(e) NULL)

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_read_delim()
try_read_delim <- function(f, delim=';') tryCatch(data.frame(readr::read_delim(f, trim_ws = T, delim=delim), stringsAsFactors = F), error=function(e) NULL)


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_dat()
read_dat <- function(f) {
  ddelim <- try_read_delim(f)
  dtable <- try_read_table(f)
  if(!is.null(dtable) & !is.null(ddelim)) d <- tryCatch(dplyr::bind_rows(dtable, ddelim), error=function(e) NULL)
  if(!is.null(dtable) & is.null(ddelim)) d <- tryCatch(dtable, error=function(e) NULL)
  if(is.null(dtable) & !is.null(ddelim)) d <- tryCatch(ddelim, error=function(e) NULL)
  d
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_read_dat()
try_read_dat <- function(f) tryCatch(read_dat(f), error=function(e) NULL)


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_dats()
read_dats <- function(flist, bind=F){
  d <- lapply(flist, try_read_dat)
  if(bind) d <- dplyr::bind_rows(d)
  d
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_file()
read_file <- function(file, bindsheets=F){
  d <- try_read_rda(file)
  if(is.null(d) | grepl("\\.xls$|\\.xlsx$", file, ignore.case=T)) d <- try_read_excel(file, bindsheets=bindsheets)
  if(is.null(d) | grepl("\\.csv$", file, ignore.case=T)) d <- try_read_csv(file)
  if(is.null(d) | grepl("\\.dat$", file, ignore.case=T)) d <- try_read_dat(file)
  if(is.null(d) | grepl("\\.f$|\\.feather$", file, ignore.case=T)) d <- try_read_feather(file)
  d <- try_combine_compact(d)
  try_data_frame(d)
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_read_file()
try_read_file <- function(file, bindsheets=F) tryCatch(read_file(file), error=function(e) NULL)


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_files()
read_files <- function(filelist, bindsheets=F, bindrows=F, simplif=T){
  d <- lapply(filelist, function(x) try_read_file(x, bindsheets=bindsheets))
  if(simplif) d <- try_combine_compact(d) %>% drop_empty()
  if(bindrows) d <- dplyr::bind_rows(d)
  d
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_df_all()
read_df_all <- function(filenames, bindrows=F, bindsheets=F, simplif=T) {
  x <- read_files(filenames)
  # names(x) <- gsub_NSRHOADS(gsub("[^_|[:alnum:]]", "", filenames))
  x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_dfs_process()
read_dfs_process <- function(filelist, by=10, outpath="AA/data/", prefix="clean_", startdoc=1){
  mylist <- filelist
  print(paste0("# of files: ", length(filelist)))
  docseq <- seq(1, (length(mylist)), by)
  lapply(docseq[grep(paste0("^", startdoc, "$"), docseq):length(docseq)], function(x){
    start <- x
    end <- x + (by - 1)
    diff <- end - length(mylist)
    end <- ifelse(diff <= 0, end, end - diff)
    sublist <- mylist[start:end]
    sublist <- lapply(sublist, function(xx) xx %>% read_df_all(.) %>% regulars_namesplit())
    filename <- paste0(outpath, prefix, round5(start), "to", round5(end), ".f")
    (sublist <- bind_rows(sublist) %>% dplyr::distinct()) %>%
      feather::write_feather(., filename)
    # return(nrow(sublist))
    print("")
    print(paste0("dim: ", paste0(dim(sublist), collapse=" row "), " col - ", filename))
    print(sample_n(filter(sublist, !is.na(fLname)), 3))
    # print(paste0("dim: ", dim(sublist)))
  }) %>% print(system.time())
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_dfs_process_by1()
read_dfs_process_by1 <- function(filelist, outpath="AA/data/", prefix='auto', startdoc=1){
  mylist <- filelist
  print(paste0("# of files: ", length(filelist)))
  lapply(startdoc:length(filelist), function(x){
    docnum <- x
    x <- filelist[x]
    ext <- tools::file_ext(x)
    print(system.time(sublist <- x %>% read_df_all(., bindsheets=T) %>% regulars_namesplit()))
    # sublist <- x %>% read_df_all(., bindsheets=T) %>% regulars_namesplit()
    splitfilename <- rev(unlist(strsplit(x, "/")))[1] %>% alnum()
    if(prefix=='auto') prefix <- splitfilename
    filename <- paste0(outpath, round5(docnum), "_", substr(prefix, start=1, stop=100), ext, "_", nrow(sublist), ".f")
    (sublist <- bind_rows(sublist) %>% dplyr::distinct())
    feather::write_feather(sublist, filename)
    print("")
    print(paste0("dim: ", paste0(dim(sublist), collapse=" row "), " col - ", filename))
    print(sample_n(filter(sublist, !is.na(fLname)), 3))
  })# %>% print(system.time())
}


#' a function gsubing stuff related to srhoads jl desktop comp path
#'
#' This function allows you to 
#' @export
#' @examples
#' gsub_NSRHOADS()
gsub_NSRHOADS <- function(x) x %>% gsub("[^_|[:alnum:]]", "", .) %>% gsub("NSRHOADSGitHubdatafilesunzip|srhoads", "", ., ignore.case=T, perl=T)


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_ydrive_write()
read_ydrive_write <- read_excel_allsheets <- function(filenames, csv=F, xlsx=F, xls=F, outpath="data/original/") {
  if(xls|xlsx){
    filenames <- readxl::excel_sheets(filenames)
    lapply(filenames, function(f) {
      print(filename <- paste0(outpath, gsub_NSRHOADS(f), ".rda"))
      tryCatch(readxl::read_excel(filenames, sheet = f), error=function(e) NULL) %>%
        save(., file=filename)
    })
  }
  if(csv) {
    lapply(filenames, function(f){ 
      print(filename <- paste0(outpath, gsub_NSRHOADS(f), ".rda"))
      tryCatch(read.csv(f), error=function(e) NULL) %>%
        save(., file=filename)
    })
  }
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_ydrive_clean_write()
read_ydrive_clean_write <- read_excel_allsheets <- function(filenames, csv=F, xlsx=F, xls=F, outpath="data/original/") {
  if(xls|xlsx){
    filenames <- readxl::excel_sheets(filenames)
    lapply(filenames, function(f) {
      print(filename <- paste0(outpath, gsub_NSRHOADS(f), ".rda"))
      d <- tryCatch(readxl::read_excel(filenames, sheet = f), error=function(e) NULL)
      if(is.list(d)) d %<>% try_combine_compact() %>% bind_rows()
      d <- regulars_namesplit(d)
      feather::write_feather(d, filename)
    })
  }
  if(csv) {
    lapply(filenames, function(f){ 
      print(filename <- paste0(outpath, gsub_NSRHOADS(f), ".rda"))
      d <- tryCatch(read.csv(f), error=function(e) NULL)
      if(is.list(d)) d %<>% try_combine_compact() %>% bind_rows()
      d <- regulars_namesplit(d)
      feather::write_feather(d, filename)
    })
  }
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' write_ydrive_originals()
write_ydrive_originals <- function(fl, outpath="AA/data/"){ # input = list of file names
  read_ydrive_write(csvfl <- fl[grep("csv$", fl, ignore.case=T)], csv=T)
  read_ydrive_write(xlsfl <- fl[grep("xls$", fl, ignore.case=T)], xls=T)
  read_ydrive_write(xlsxfl <- fl[grep("xlsx$", fl, ignore.case=T)], xlsx=T)
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
unzip_dir <- function(zipfile, outdir="unzip"){
  output <- gsub("\\.zip$", "", zipfile)
  output <- gsub("original", outdir, output)
  unzip(zipfile, exdir = output)
  file.remove(zipfile)
  output
  print(output)
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_unzip()
try_unzip <- function(zipfile){
  tryCatch(unzip_dir(zipfile), error = function(e) zipfile)
}
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' drop_empty()
drop_empty  <-  function(x_list) x_list[unlist(lapply(x_list, length) != 0)] 

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' trimspace()
trimspace <- function(vec){
  vec %>% 
    gsub("     |   |  ", " ", .) %>%
    trimws(., which="both") %>%
    trimws(., which="both") %>%
    dplyr::na_if(., "")
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' remove_precomma()
remove_precomma <- function(vec, trim=T){
  vec %>% gsub(".*,", "", .)
  if(trim) vec %<>% trimspace()
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' list_names()
list_names <- function(mylist, unique = F){
  mylist <- tryCatch(dplyr::combine(dplyr::combine(mylist)), 
                     error = function(e) {
                       tryCatch(dplyr::combine(mylist), 
                                error = function(e) mylist)
                     })
  
  mylist <- tryCatch(dplyr::combine(mylist), 
                     error = function(e) mylist)
  if(unique)
    return(purrr::map(mylist, ~names(.x)) %>% 
             unlist() %>% 
             unique())
  purrr::map(mylist, ~names(.x)) %>% 
    unlist()
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' split_original_rdas()
split_original_rdas <- function(filelist = NULL,
                                inpath = NULL,
                                pattern = NULL,
                                newdir = NULL,
                                subsets = NULL,
                                outpath = "~/",
                                by,
                                extra = NULL,
                                filename_prefix) {
  print(filename_prefix)
  print(by)
  
  if(is.null(filelist)) filelist <- list.files(inpath, pattern, full.names=T)
  
  d <- read_rdas(filelist = filelist) %>% tryCatch_combine_compact() %>% tryCatch_combine_compact()
  
  if(by < 1) by <-  length(d)
  if(by > length(d)) by <- length(d)
  
  lapply(seq(1, (length(d)), by), function (x) {
    start <- x
    end <- x + (by - 1)
    diff <- end - length(d)
    end <- ifelse(diff <= 0, end, end - diff)
    snippet <- d[start:end]
    filename <- cleanpath(paste0(outpath, "/", filename_prefix, "_", start, "to", end, ".rda"))
    save(snippet, file=filename)
    # print(paste0(dim(snippet), " -- ", filename))
    print(paste0(filename, " | ", length(snippet)))
  }
  )
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' reduce_name_bytes()
reduce_name_bytes <- function(x){
  names(x) <- substr(names(x), start = 1, stop = 93)
  if (is.list(x) & ! is.data.frame(x)) 
    x <- lapply(X = x, FUN = reduce_name_bytes)
  x
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' select_not_race_gender_cols()
select_not_race_gender_cols <- function(mylist) {
  nrg <- purrr::map(mylist,
                    ~dplyr::select(.x, #dplyr::matches('name|x'), dplyr::contains(dplyr::everything()),
                                   -dplyr::matches('gender|race|date|time')
                                   #dplyr::matches(minuscontainsregex)
                    ))
  nrg
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' select_nrg_cols_list()
select_nrg_cols_list <- function(mylist, output = c("list", "names")) {
  output <- match.arg(output)
  mylist <- purrr::map(mylist, ~dplyr::select(.x, 
                                              dplyr::matches('name|gender|gendr|gndr|gnder|sex|race|ethnic'),
                                              -dplyr::matches(minuscontainsregex)))
  if(output == "names")
    return(purrr::map(mylist, ~names(.x)) %>% unlist() %>% unique())
  if(output == "list")
    return(mylist)
  mylist
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' select_nrg_cols_df()
select_nrg_cols_df <- function(df, output = c("df", "names")) {
  output <- match.arg(output)
  df <- dplyr::select(df, dplyr::matches('name|gender|gendr|gndr|gnder|sex|race|ethnic'),-dplyr::matches(minuscontainsregex))
  if(output == "names")
    return(names(df))
  if(output == "df")
    return(df)
  df
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' select_nrg_cols()
select_nrg_cols <- function(x, type = c("list", "df"), output = NULL) {
  type <- match.arg(type)
  if(type == "list")
    return(select_nrg_cols_list(x, output = output))
  if(type == "df")
    return(select_nrg_cols_df(x, output = output))
  return(select_nrg_cols_list(x, output = output))
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_name_cols_df <- function(df, output = c("df", "names")) {
  output <- match.arg(output)
  df <- dplyr::select(df, 
                      dplyr::matches('name'),
                      -dplyr::matches(minuscontainsregex))
  if(output == "names")
    return(names(df))
  if(output == "df")
    return(df)
  df
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_gender_cols_df <- function(df, output = c("df", "names", "dfnewnames")) {
  output <- match.arg(output)
  exacts <- c("female", "male", "fem", "man", "woman", "men", "women", "girl", "boy", "feminine", "masculine")
  partials <- c("female", "woman", "women", "feminine", "masculine")
  cols1 <- 
    dplyr::select(df, dplyr::matches("gender|sex|female|male|gndr|gendr|male|femini|woman|women|masculi")) %>% names()
  cols2 <- 
    dplyr::select_if(df, function(x) {any(x %in% exacts) | any(grepl(paste0(partials, collapse="|"), x, ignore.case = T))}) %>% names()
  cols <- c(cols1, cols2) %>% unique()
  df <- 
    dplyr::select(df, dplyr::matches(paste0(cols, collapse="|"))) #%>% dplyr::distinct()
  
  if(length(df) == 0 | ncol(df) == 0){
    df <- data.frame(name = c("jenny", "bob"), gender = c("female", "male"), race = c("asian", "white"), stringsAsFactors = F)
  }
  
  if(output == "names")
    return(names(df))
  if(output == "df")
    return(df)
  if(output == "dfnewnames") {
    names(df) <- paste0("gender.", 1:ncol(df))
    df
  }
  df
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_gender_cols_nontrad_df <- function(df, output = c("df", "names", "dfnewnames")) {
  output <- match.arg(output)
  exacts <- c("female", "male", "fem", "man", "woman", "men", "women", "girl", "boy", "feminine", "masculine")
  partials <- c("female", "woman", "women", "feminine", "masculine")
  cols1 <-
    dplyr::select(df, dplyr::matches("gender|sex|female|male|gndr|gendr|male|femini|woman|women|masculi")) %>% names()
  cols2 <-
    dplyr::select_if(df, function(x) {any(x %in% exacts) | any(grepl(paste0(partials, collapse="|"), x, ignore.case = T))}) %>% names()
  cols <- c(cols1, cols2, "PLACEFILLER") %>% unique()
  # df$PLACEFILLER <- NA
  df <-
    dplyr::select(df, dplyr::matches(paste0(cols, collapse="|"))) %>% dplyr::select(- dplyr::matches("gender"))
  
  if(length(df) == 0 | ncol(df) == 0){
    df <- data.frame(name = c("jenny", "bob"), gender = c("female", "male"), race = c("asian", "white"), stringsAsFactors = F)
  }
  
  if(output == "names")
    return(names(df))
  if(output == "df")
    return(df)
  if(output == "dfnewnames") {
    names(df) <- paste0("gender.", 1:ncol(df))
    df
  }
  df
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' gender_seq_names()
gender_seq_names <- function(x){
  names(x) <- gsub("[^[:alpha:]]", "", names(x), perl = T)
  names(x) <- gsub("^", "gender", names(x), perl = T)
  if (is.list(x) & ! is.data.frame(x)) 
    x <- lapply(X = x, FUN = gender_seq_names)
  x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_gender_cols_list <- function(mylist, output = c("list", "names")) {
  output <- match.arg(output)
  exacts <- c("female", "male", "fem", "man", "woman", "men", "women", "girl", "boy", "feminine", "masculine")
  partials <- c("female", "woman", "women", "feminine", "masculine")
  mylist <- tryCatch(dplyr::combine(mylist), 
                     error = function(e) mylist)
  cols1 <- 
    purrr::map(mylist, ~select(.x, dplyr::matches("gender|sex|female|male|gndr|gendr|male|femini|woman|women|masculi"))) %>%
    purrr::map(., ~names(.x)) %>% unlist() %>% unique()
  cols2 <- 
    purrr::map(mylist, 
               ~dplyr::select_if(.x, function(xx) {any(xx %in% exacts) | any(grepl(paste0(partials, collapse="|"), xx, ignore.case = T))})) %>% 
    purrr::map(., ~names(.x)) %>% unlist() %>% unique()
  cols <- c(cols1, cols2, "PLACEFILLER") %>% unique()
  mylist <- purrr::map(mylist, ~select(.x, dplyr::matches(paste0(cols, collapse="|"))))
  if(output == "names")
    return(purrr::map(mylist, ~names(.x)) %>% unlist() %>% unique())
  if(output == "list")
    return(mylist)
  mylist
} 


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_race_cols_df <- function(df, output = c("df", "names", "dfnewnames")) {
  output <- match.arg(output)
  exacts <- c("white", "black", "black or african american", "blackorafricanamerican", "hispanic or latino", "hispanicorlatino", "asian",
              "americanindianoralaskanative", "american indian or alaska native", "nativehawaiianorotherpacificislander", 
              "native hawaiian or other pacific islander", "twoormoreraces", "two or more races")
  partials <- c("african", "hispanic", "americanindian", "american indian", "hawaiian", "pacificislander", "hispanic",
                "pacific islander", "indian")
  cols1 <- 
    dplyr::select(df, dplyr::matches("race|ethnicity|ethnicit|ethnici|ethnic|ethni|ethno|ethn|rce|racial")) %>% names()
  cols2 <- 
    dplyr::select_if(df, function(x) {any(x %in% exacts) | any(grepl(paste0(partials, collapse="|"), x, ignore.case = T))}) %>% names()
  cols <- c(cols1, cols2) %>% unique()
  df <- 
    dplyr::select(df, dplyr::matches(paste0(cols, collapse="|")), -dplyr::matches("name")) #%>% dplyr::distinct()
  
  if(length(df) == 0 | ncol(df) == 0)df <- data.frame(name = c("jenny", "bob"), gender = c("female", "male"), race = c("asian", "white"), stringsAsFactors = F)
  
  if(output == "names")
    return(names(df))
  if(output == "df")
    return(df)
  if(output == "dfnewnames") {
    names(df) <- paste0("race.", 1:ncol(df))
    df
  }
  df
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_race_cols_nontrad_df <- function(df, output = c("df", "names", "dfnewnames")) {
  output <- match.arg(output)
  exacts <- c("white", "black", "black or african american", "blackorafricanamerican", "hispanic or latino", "hispanicorlatino", "asian",
              "americanindianoralaskanative", "american indian or alaska native", "nativehawaiianorotherpacificislander",
              "native hawaiian or other pacific islander", "twoormoreraces", "two or more races")
  partials <- c("african", "hispanic", "americanindian", "american indian", "hawaiian", "pacificislander", "hispanic",
                "pacific islander", "indian")
  cols1 <-
    dplyr::select(df, dplyr::matches("race|ethnicity|ethnicit|ethnici|ethnic|ethni|ethno|ethn|rce|racial")) %>% names()
  cols2 <-
    dplyr::select_if(df, function(x) {any(x %in% exacts) | any(grepl(paste0(partials, collapse="|"), x, ignore.case = T))}) %>% names()
  cols <- c(cols1, cols2, "PLACEFILLER") %>% unique()
  # df$PLACEFILLER <- NA
  df <-
    dplyr::select(df, dplyr::matches(paste0(cols, collapse="|")),-dplyr::matches("name")) %>% dplyr::select(- dplyr::matches("race"))
  
  if(length(df) == 0 | ncol(df) == 0){
    df <- data.frame(name = c("jenny", "bob"), gender = c("female", "male"), race = c("asian", "white"), stringsAsFactors = F)
  }
  
  if(output == "names")
    return(names(df))
  if(output == "df")
    return(df)
  if(output == "dfnewnames") {
    names(df) <- paste0("race.", 1:ncol(df))
    df
  }
  df
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_nrg_cols_list <- function(mylist, output = c("list", "names")) {
  output <- match.arg(output)
  mylist <- purrr::map(mylist, ~dplyr::select(.x, 
                                              dplyr::matches('name|gender|gendr|gndr|gnder|sex|race|ethnic'),
                                              -dplyr::matches(minuscontainsregex)))
  if(output == "names")
    return(purrr::map(mylist, ~names(.x)) %>% unlist() %>% unique())
  if(output == "list")
    return(mylist)
  mylist
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
race_seq_names <- function(x){
  names(x) <- stringr::str_replace_all(names(x), "[^[:alpha:]]", "")
  names(x) <- stringr::str_replace_all(names(x), "^", "race")
  if (is.list(x) & ! is.data.frame(x)) 
    x <- lapply(X = x, FUN = race_seq_names)
  x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_race_cols_list <- function(mylist, output = c("list", "names")) {
  output <- match.arg(output)
  exacts <- c("white", "black", "black or african american", "blackorafricanamerican", "hispanic or latino", "hispanicorlatino", "asian",
              "americanindianoralaskanative", "american indian or alaska native", "nativehawaiianorotherpacificislander", 
              "native hawaiian or other pacific islander", "twoormoreraces", "two or more races")
  partials <- c("african", "hispanic", "americanindian", "american indian", "hawaiian", "pacificislander", "hispanic",
                "pacific islander", "indian")
  mylist <- tryCatch(dplyr::combine(mylist), 
                     error = function(e) mylist)
  cols1 <- 
    purrr::map(mylist, ~select(.x, dplyr::matches("gender|sex|female|male|gndr|gendr|male|femini|woman|women|masculi"))) %>%
    purrr::map(., ~names(.x)) %>% unlist() %>% unique()
  cols2 <- 
    purrr::map(mylist, 
               ~dplyr::select_if(.x, function(xx) {any(xx %in% exacts) | any(grepl(paste0(partials, collapse="|"), xx, ignore.case = T))})) %>% 
    purrr::map(., ~names(.x)) %>% unlist() %>% unique()
  cols <- c(cols1, cols2) %>% unique()
  mylist <- purrr::map(mylist, ~select(.x, dplyr::matches(paste0(cols, collapse="|")),
                                       -dplyr::matches("name")))
  if(output == "names")
    return(purrr::map(mylist, ~names(.x)) %>% unlist() %>% unique())
  if(output == "list")
    return(mylist)
  mylist
} 

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_name_race_gender_cols_df <- function(df, output = c("df", "names")){
  output <- match.arg(output)
  cnrg <- select_nrg_cols_df(df, output = "names")
  cr <- select_race_cols_df(df, output = "names")
  cg <- select_gender_cols_df(df, output = "names")
  cols <- c(cnrg, cr, cg) %>% unique()
  df <- dplyr::select(df, dplyr::matches(paste0(cols, collapse="|"))) %>% dplyr::distinct()
  if(output == "names")
    return(names(df))
  if(output == "df")
    return(df)
  df
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_name_race_gender_cols_list <- function(mylist, output = c("list", "names")){
  output <- match.arg(output)
  cnrg <- select_nrg_cols_list(mylist, output = "names")
  cr <- select_race_cols_list(mylist, output = "names")
  cg <- select_gender_cols_list(mylist, output = "names")
  cols <- c(cnrg, cr, cg) %>% unique()
  mylist <- tryCatch(dplyr::combine(mylist), 
                     error = function(e) mylist)
  mylist <- purrr::map(mylist, ~select(.x, dplyr::matches(paste0(cols, collapse="|"))))
  if(output == "names")
    return(purrr::map(mylist, ~names(.x)) %>% unlist() %>% unique())
  if(output == "list")
    return(mylist)
  mylist
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_name_race_gender_cols <- function(dat, type = c("list", "df"), output = NULL){
  type <- match.arg(output)
  if(type == "list")
    return(select_name_race_gender_cols_list(dat, output = output))
  if(type == "df")
    return(select_name_race_gender_cols_df(dat, output = output))
  select_name_race_gender_cols_list(dat, output = output)
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_name_race_gender_cols <- function(dat, type = c("list", "df"), output = NULL){
  type <- match.arg(type)
  if(type == "list")
    return(select_name_race_gender_cols_list(dat, output = output))
  if(type == "df")
    return(select_name_race_gender_cols_df(dat, output = output))
  select_name_race_gender_cols_list(dat, output = output)
}


# ---------------------------------------------------------------------------------


#' A read files into list of list function
#'
#' This function allows you to read a list of files into a list of lists of data if the data is in excel (xlsx or xls format)
#' @export
#' @examples
#' data_lol()
data_lol <- function(pattern='data'){
  dirs <- list.dirs(pattern, recursive=T) %>% .[-grep(paste0(pattern, "$"), .)]
  data <- lapply(dirs, function(l){
    files <- list.files(l, recursive=T, full.names=T)
    l %<>% 
      list.files(., recursive=T, full.names=T) %>% 
      read_excels(., bindsheets = T)
    names(l) <- basename(files)
    l
  }) %>% setNames(., basename(dirs))
}



#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
try_bind <- function(x){
  x <- tryCatch(dplyr::bind_rows(x), 
                error = function(e) plyr::ldply(x, dplyr::bind_rows))
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
list_to_df <- function(mylist) {
  mylist <- try_combine_compact(mylist)
  nrgdf <- try_bind(mylist)
  nrgdf <- data.frame(
    lapply(nrgdf, function (x){
      iconv(x)  %>% 
        stringi::stri_enc_toutf8() %>%
        as.character() %>%
        tolower() %>%
        gsub("[^_|,| |-|\\-|'|\\.|[:space:]|[:alnum:]]|_$|^_", "", ., perl = T) %>%
        gsub("  |\\.|\\_", " ", ., perl = T) %>%
        gsub(" _|_ ", "_", ., perl = T) %>%
        trimws(., which = "both")
    }), stringsAsFactors = F)
  dplyr::distinct(nrgdf)
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' list_to_df_anomalies()
list_to_df_anomalies <- function(mylist) {
  mylist <- try_combine_compact(mylist)
  # nrgdf <- plyr::ldply(mylist)
  nrgdf <- try_bind(mylist)
  nrgdf <- data.frame(lapply(nrgdf, function (x) {
    x <- stringi::stri_enc_toutf8(x)
    x <- as.character(x)
    x <- iconv(x)
    x <- tolower(x)
    x %<>%
      gsub("[^_|,| |-|\\-|'|\\.|[:space:]|[:alnum:]]", "", ., perl = T) %>%
      gsub("\\.|\\_", " ", ., perl = T) %>%
      gsub("_$|$_", "", ., perl = T) %>%
      gsub("  ", " ", ., perl = T)
    x <- trimws(x, which = "both")
    x <- dplyr::na_if(x, "NA")
    x <- dplyr::na_if(x, "")
    x
  }), stringsAsFactors = F)
  dplyr::distinct(nrgdf)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_names_other()
preprocess_names_other <- function(x) {
  names(x) %<>% 
    gsub("coursenameid|vacancyname|nameoftraining|divname|fltname|sexperience|sexclude|sexternal|force", "REMOVE",. , perl = T) %>%
    gsub(preprocess_names_other_getridofstrregex, "", ., perl = T)
  names(x)
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
preprocess_names_race <- function(x) {
  names(x) %<>% 
    gsub(turntoracestrregex, "race", ., perl = T) %>% 
    gsub("raceracerace|racerace|racename|namerace", "race", ., perl = T)
  names(x)
}
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
preprocess_names_name <- function(x) {
  names(x) %<>% 
    gsub("names$|names$|namelfmi|firstnamelastmi|lastnamefirstmi|lastnamesuffixfirstmi|firstlastname|lastnamefirstmi|lastfirstname", "name", ., perl = T) %>%
    
    gsub("firstfirst|middlemiddle|middle|^mid|mid$|frist|frst|1st|andfirst|firstand|firest", "first", ., perl = T) %>%
    gsub("lastmi$|lastmid$|^milast|lastlast|lastmi|blast", "last", ., perl = T) %>%
    
    gsub("person$|nameort|nameorlastname|firstlast|lastfirst|firstnamelast|lastnamefirst|namefull|^urname|namedle|^yoname|initials", "name", ., perl = T) %>%
    gsub("namemiddleinitial|namemi$|namedle|lastnameorfirstname|nameof$|lastnameorfirstname|^eename|^sname|^alname|namescreen", "name", ., perl = T) %>%
    gsub("lfmname|^urname|^urname|nameofor$", "name", ., perl = T) %>%
    gsub("firstmi$|frstname|nickname|candfirstname|^miname|^mname|firstnamea$|firstnamemi$|firstname|firstnam|midname|^mname|^mname", "firstname", ., perl = T) %>%  
    gsub("namelast|candlastname|lastnameb$|lastmi$|^alastname|^slastname|^slastname", "lastname", ., perl = T) %>%  
    gsub("lastname|fulllastname|^slastname|surname|latename|^astname|^llastname|lastnameort|^hlastname|lname", "lastname", ., perl = T) %>%
    gsub("firstnamelastmi|firstinitial|lastinitial|lastnamefirstmi|lastnamesuffixfirstmi|firstlastname|lastnamefirstmi|lastfirstname|namemiddleinitial|firstmid|mifirst|firstmi|midfirst", "name", ., perl = T) %>%
    gsub("namemiddle|nameort$|nameorlastname|firstlast|lastfirst|firstnamelast|lastnamefirst|namemi$|lastnamefirst|firstmiddlelast|firstandlast|lastorfirst|firstorlast|lastandfirst", "name", ., perl = T) %>%
    gsub("firstname|namefirst|fistname|fullfirstname|lfirstname|fristname|namemid$|firstname|irstname|^ffirstname|nickname|middleinitial|^miname|fname", "firstname", ., perl = T) %>%  
    gsub("middleinitial|firstnamemi|^miname|^miname|^fname|^mname|forename|firstnameafirstname|^sfirstname|namemi$", "firstname", ., perl = T) %>%  
    gsub("lastname|namelast|^lname|^lname|fulllastname|^slastname|surname|latename|^astname|^llastname|lastinitial|lastname|lastnam|^llastname", "lastname", ., perl = T) %>%
    
    gsub("firstnamefirstnamefirstname|firstnamefirstname", "firstname", ., perl = T) %>%
    gsub("lastnamelastnamelastname|lastnamelastname", "lastname", ., perl = T) %>%
    gsub("namenamename|namename", "name", ., perl = T)
  names(x)
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_names_gender()
preprocess_names_gender <- function(x) {
  names(x) %<>% 
    gsub(turntogenderstrregex, "gender", ., perl = T) %>%
    gsub("racesex|ethgender|ethsex|genderrace|sexrace|genderethnicity|gendereth|sexeth|racgender|raceender|genderace", "racegender", ., perl = T) %>%
    gsub("gendergendergender|gendergender|gendername|namegender", "gender", ., perl = T)
  names(x)
}
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_names_thorough()
preprocess_names_thorough <- function(x) {
  x <- plyr::compact(x)
  
  names(x) <- iconv(names(x)) %>% 
    stringi::stri_enc_toutf8() %>%
    as.character() %>%
    trimws(., which = "both") %>%
    tolower() %>% 
    gsub("[^[:alpha:]]", "", ., perl = T) %>% 
    gsub("\\.", "", ., perl = T) 
  # %>% gsub("coursenameid|vacancyname|nameoftraining|divname|fltname|sexperience|sexclude|sexternal|force", "REMOVE", ., perl = T)
  
  names(x) <- preprocess_names_race(x)
  names(x) <- preprocess_names_gender(x)
  names(x) <- preprocess_names_other(x)
  names(x) <- preprocess_names_name(x)
  
  names(x) %<>% 
    gsub("gendergendergender|gendergender", "gender", ., perl = T) %>%
    gsub("raceracerace|racerace", "race", ., perl = T) %>%
    gsub("namenamename|namename", "name", ., perl = T)
  
  
  names(x) <- make.names(names = names(x), unique = TRUE)
  names(x) <- as.character(names(x))
  if (is.list(x) & ! is.data.frame(x)) 
    x <- lapply(X = x, FUN = preprocess_names_thorough)
  x
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_names_minimal()
preprocess_names_minimal <- function(x) {
  x <- plyr::compact(x)
  names(x) <- iconv(names(x)) %>% 
    stringi::stri_enc_toutf8() %>%
    as.character() %>%
    trimws(., which = "both") %>%
    tolower() %>% 
    gsub("[^[:alpha:]]", "", ., perl = T) %>% 
    gsub("\\.", "", ., perl = T) 
  # %>% gsub("coursenameid|vacancyname|nameoftraining|divname|fltname|sexperience|sexclude|sexternal", "REMOVE", ., perl = T)
  names(x) <- preprocess_names_race(x)
  names(x) <- preprocess_names_gender(x)
  names(x) <- preprocess_names_name(x)
  names(x) %<>% gsub("gendergendergender|gendergender", "gender", ., perl = T) %>%
    gsub("raceracerace|racerace", "race", ., perl = T) %>%
    gsub("namenamename|namename", "name", ., perl = T)
  names(x) <- make.names(names = names(x), unique = TRUE)
  names(x) <- as.character(names(x))
  if (is.list(x) & ! is.data.frame(x)) x <- lapply(X = x, FUN = preprocess_names_minimal)
  x
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' combine_compact()
combine_compact <- function(x){
  x <- plyr::compact(x)
  x <- dplyr::combine(x)
  x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_combine()
try_combine <- function(x){
  x <- tryCatch(dplyr::combine(dplyr::combine(x)), 
                error = function(e) {
                  tryCatch(dplyr::combine(x), 
                           error = function(e) x)
                })
  
  x <- tryCatch(dplyr::combine(x), 
                error = function(e) x)
  x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_compact()
try_compact <- function(x){
  x <- tryCatch(plyr::compact(plyr::compact(x)), 
                error = function(e) {
                  tryCatch(plyr::compact(x), 
                           error = function(e) x)
                })
  
  x <- tryCatch(plyr::compact(x), 
                error = function(e) x)
  x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' try_combine_compact()
try_combine_compact <- function(x){
  x <- try_combine(x)
  x <- try_compact(x)
  x <- try_combine(x)
  x <- try_compact(x)
  x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_names()
preprocess_names <- function(x, extent = c("minimal", "thorough")) {
  extent = match.arg(extent)
  if(extent == "minimal")
    return(preprocess_names_minimal(x))
  if(extent == "thorough")
    return(preprocess_names_thorough(x))
  preprocess_names_thorough(x)
}


# ---------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_all_cols(x, extent = "thorough")
preprocess_all_cols <- function(x, extent = "thorough") {
  if(is.list(x)){
    x <- tryCatch(dplyr::combine(x),
                  error = function(e) x)
  }
  preprocess_names(x, extent = "thorough") %>%
    select_not_race_gender_cols() %>%
    list_to_df_anomalies() %>%
    dplyr::distinct()
}

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
preprocess_data_minimal <- function(x, type = c("lod")) {
  type = match.arg(type)
  x <- tryCatch(dplyr::combine(x), 
                error = function(e) x)
  if(type == "lod")
    return(preprocess_names(x, extent = "minimal") %>% 
             select_nrg_cols(.) %>% 
             list_to_df(.) %>% dplyr::distinct())
  preprocess_names_minimal(x, extent = "minimal") %>% 
    select_nrg_cols(.) %>% 
    list_to_df(.) %>% dplyr::distinct()
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
preprocess_data_thorough <- function(x, type = c("lod")) {
  type = match.arg(type)
  x <- tryCatch(dplyr::combine(x), 
                error = function(e) x)
  if(type == "lod")
    return(preprocess_names_thorough(x) %>% 
             select_nrg_cols(.) %>% 
             list_to_df(.) %>% dplyr::distinct())
  preprocess_names_thorough(x) %>% 
    select_nrg_cols(.) %>% 
    list_to_df(.) %>% dplyr::distinct()
}
# ---------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_data()
preprocess_data <- function(x, type = c("lod"), extent = NULL) {
  type = match.arg(type)
  x <- tryCatch(dplyr::combine(x), 
                error = function(e) x)
  if(type == "lod")
    return(preprocess_names(x, extent = extent) %>% 
             select_nrg_cols(.) %>% 
             list_to_df(.) %>% dplyr::distinct())
  
  preprocess_names(x, extent = extent) %>% 
    select_nrg_cols(.) %>% 
    list_to_df(.)# %>% dplyr::distinct()
}
# ---------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' gather_firstname()
gather_firstname <- function(df) {
  tidyr::gather(df, "twastwas", "firstname", dplyr::contains("firstname")) %>% 
    dplyr::select(-dplyr::contains("twastwas")) %>% dplyr::distinct() 
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' gather_lastname()
gather_lastname <- function(df) {
  tidyr::gather(df, "twastwas", "lastname", dplyr::contains("lastname")) %>% 
    dplyr::select(-dplyr::contains("twastwas")) %>% dplyr::distinct() 
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' gather_first_last_name()
gather_first_last_name <- function(df) {
  df <- gather_lastname(df)
  df <- gather_firstname(df) 
  # dplyr::distinct(df)
  df
}

# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' join_firstlastname()
join_firstlastname <- function(df, firstname, lastname, seq = c("first last", "last,first")) {
  seq <- match.arg(seq)
  fldf <- data.frame(firstname="samantha", lastname="rhoads", name="samantha rhoads", stringsAsFactors = F)
  if(is.null(df$firstname) | is.null(df$lastname)) df %<>% bind_rows(., fldf)
  # df$firstname = firstname
  # df$lastname = lastname
  # if(is.null(df$firstname)) df$firstname <- NA
  # if(is.null(df$lastname)) df$lastname <- NA
  if(seq == "last,first")
    return(dplyr::distinct(df) %>% 
             dplyr::mutate(name_firstlast = ifelse(!is.na(lastname) & !is.na(firstname), paste0(lastname, ",", firstname), 
                                                   ifelse(!is.na(lastname) & is.na(firstname), paste0(lastname), 
                                                          ifelse(is.na(lastname) & !is.na(firstname), paste0(firstname), 
                                                                 NA)))) %>% dplyr::distinct())
  return(if(seq == "first last")
    dplyr::distinct(df) %>% 
      dplyr::mutate(name_firstlast = ifelse(!is.na(lastname) & !is.na(firstname), paste0(firstname, " ", lastname), 
                                            ifelse(!is.na(lastname) & is.na(firstname), paste0(lastname), 
                                                   ifelse(is.na(lastname) & !is.na(firstname), paste0(firstname), 
                                                          NA)))) %>% dplyr::distinct())
  # if not specified, then do natural form: "firstname lastname"
  df %>% dplyr::mutate(name_firstlast = ifelse(!is.na(lastname) & !is.na(firstname), paste0(firstname, " ", lastname), 
                                               ifelse(!is.na(lastname) & is.na(firstname), paste0(lastname), 
                                                      ifelse(is.na(lastname) & !is.na(firstname), paste0(firstname), 
                                                             NA)))) %>% dplyr::distinct()
}
# ---------------------------------------------------------------------------------

# gather_nrg is usually unusable on HUGE datasets. Use the respective lil fxns below it in that case :)
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' gather_nrg()
gather_nrg <- function(df) {
  df %>%
    tidyr::gather("twastwas", "name", -dplyr::contains("race"), -dplyr::contains("gender")) %>%
    tidyr::gather("twastwas", "gender", -dplyr::contains("name"), -dplyr::contains("race")) %>%
    tidyr::gather("twastwas", "race", -dplyr::contains("name"), -dplyr::contains("gender")) %>%
    dplyr::select(-dplyr::contains("twastwas")) %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>%
    lapply(stringi::stri_enc_toutf8) %>%
    data.frame(., stringsAsFactors = F) %>%
    dplyr::distinct() 
}

# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
dealwith_racegender_variable <- function(x){
  if(!is.null(x$racegender)){
    x <- tidyr::gather(x, "twastwas", "racegender", dplyr::matches("racegender|racgender|raceender|genderrac")) %>% 
      dplyr::select(-dplyr::contains("twastwas"))
    x <- dplyr::mutate(x, race_frm_rg = racegender,
                       gender_frm_rg = racegender)
    x <- dplyr::select(x, -dplyr::contains("racegender"))
  }
  x
}
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_name <- function(df) {
  df %>%
    tidyr::gather("twastwas", "name", dplyr::contains("name")) %>% 
    dplyr::select(-dplyr::contains("twastwas")) %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>%
    lapply(stringi::stri_enc_toutf8) %>% 
    data.frame(., stringsAsFactors = F) %>%  
    dplyr::distinct() 
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_name_namesplit <- function(df) {
  names(df) %<>% gsub("lastname", "ln_orig", .) %>% gsub("firstname", "fn_orig", .)
  df %<>%
    tidyr::gather("twastwas", "name", dplyr::contains("name")) %>% 
    dplyr::select(-dplyr::contains("twastwas")) %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>%
    lapply(stringi::stri_enc_toutf8) %>% 
    data.frame(., stringsAsFactors = F) %>% 
    # namesplit() %>%
    dplyr::distinct() 
  df
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_gender <- function(df) {
  df %>%
    tidyr::gather("twastwas", "gender", dplyr::contains("gender")) %>% 
    dplyr::select(-dplyr::contains("twastwas"), -dplyr::one_of(".id")) %>%
    lapply(stringi::stri_enc_toutf8) %>% 
    data.frame(., stringsAsFactors = F) %>% 
    dplyr::distinct() 
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_race <- function(df) {
  df %>%
    tidyr::gather("twastwas", "race", dplyr::contains("race")) %>% 
    dplyr::select(-dplyr::contains("twastwas"), -dplyr::one_of(".id")) %>%
    lapply(stringi::stri_enc_toutf8) %>% 
    data.frame(., stringsAsFactors = F) %>% 
    dplyr::distinct() 
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_race_and_gender <- function(df) {
  df <- tidyr::gather(df, "twastwas", "gender", dplyr::contains("gender")) %>% 
    dplyr::select(-dplyr::contains("twastwas"), -dplyr::one_of(".id"))
  df <- tidyr::gather(df, "twastwas", "race", dplyr::contains("race")) %>% 
    dplyr::select(-dplyr::contains("twastwas"), -dplyr::one_of(".id"))
  # dplyr::distinct(df)
}
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_join_first_last_name <- function(df, seq = c("first last", "last,first")) {
  seq <- match.arg(seq)
  df <- gather_first_last_name(df)
  df <- join_firstlastname(df, df$firstname, df$lastname, seq = seq)
  df <- gather_name(df) 
  dplyr::distinct(df)
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_join_first_last_namesplit <- function(df, seq = c("first last", "last,first")) {
  seq <- match.arg(seq)
  df <- gather_first_last_name(df)
  df <- join_firstlastname(df, df$firstname, df$lastname, seq = seq)
  df <- gather_name_namesplit(df) 
  df
  # dplyr::distinct(df)
}

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
recode_na_list <- list(
  "NA" = c("NATOEVERYONE", "malefemalerace", "malefemale", "femalemale")
)

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
recode_na_vec <- function(vec, 
                          recode_list = recode_na_list, 
                          extra = NULL) {
  recode_key <- lapply(
    names(recode_list), 
    function(x) {
      to_recode <- recode_list[[x]]
      setNames(rep(x, length(to_recode)), to_recode)
    } )
  recode_key <- unlist(recode_key)
  vec <- dplyr::recode(vec, !!!recode_key)
  vec <- gsub("^NA$", NA, vec, perl = T)
  vec <- gsub("[[:digit:]]", "", vec, perl = T)
  vec <- gsub(recode_na_getridofstrregex, "", vec, perl = T)
  vec
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
recode_na <- function(x){
  if(is.data.frame(x) | is.list(x)) x %<>% lapply(., recode_na_vec) %>% data.frame(., stringsAsFactors=F)
  else x %<>% recode_na_vec(x)
  x
}

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' recode_race_regex()
recode_race_regex <- function(vec) {
  vec <- tolower(vec)
  vec %<>% 
    gsub("[^[:alnum:]]", "", ., perl = T) %>%
    gsub(recode_race_getridofstrregex, "", ., perl = T) %>%
    
    gsub("islandernis$|islandernis$|isldr|islanderor$", "islander", ., perl = T) %>%
    
    gsub("wwhite|caucasian|caucasin|caucason|caucasion|cauc$|casian|whitewhite|whiteorwhite|whte|wt|wht|wite|wit|whitey|whiteowhite", "white", ., perl = T) %>%
    gsub("whitewhite|white|whit|whi|wh|whites$|whitess$|^rwhite|^ewhite|^swhite|^rwhite|^fwhite|caucaian|^rwhite", "white", ., perl = T) %>%
    
    gsub("bla$|^eblack|blacker|bblack|blackblack|blk|afram$|aframe$|africaname$|balck|alfram|blck|africanam$|^mblack|^fblack|aframerican|afamerican", "black", ., perl = T) %>%
    gsub("blackblackblack|blackblack|blackafrica$|caribbean|blackerican|^orblack|blacker$|blackor$|^rblack|^cblack|^cblack|^rblack|^ablack", "black", ., perl = T) %>%
    gsub("blackoramerican", "black", ., perl = T) %>%
    
    gsub("africian|afrian|afrcn|afrn|afric$|afran|africana$|africano|africanme$|africanmer$|aferican|aficrician|afircan", "african", ., perl = T) %>%
    
    gsub("^easian|asn|ason|asin|asan|asain|^rasian|^aasian|asianasian|^sasian|^rasian|asianamerican|asianamerica|asianameric|asianameri|asianamer|asianame|asianam|^rasian", "asian", ., perl = T) %>%
    gsub("^basian", "asian", ., perl = T) %>%
    
    gsub("alasknat$", "alaskanative", ., perl = T) %>%
    gsub("native|nativ|natve|natv|nat$|ntve|ntv|nativeor$|^snative|^rnative|^anative", "native", ., perl = T) %>%
    gsub("amindian|amindia|amindi|amind|amin|amerindian|amerindia|amerindi|amerind|amerin|indianamerican|indianamerica|indianameric|indianameri|indianamer|indianame|indianam", "nativeamerican", ., perl = T) %>%
    gsub("natam$|nvam$|nativam$|nativeam$|navam$|navamerican|nativeamer$|anericanindian|ameriindn", "nativeamerican", ., perl = T) %>%
    gsub("america$|americna|^ramerican|ameerican|amrican|amrica$|ameer$|amrcn|^samerican", "american", ., perl = T) %>%
    gsub("americans$|american|america|amrican|americ$|amer$|amer$|americian|amercian|ameri$|americanss$", "american", ., perl = T) %>%
    
    gsub("hispanicorlatino|^elatino|lation|latina|latono|latini|latin$|lat$|latno|latna|latn|latinolatino|latinov|latinoall|latinos$|^hislatino|latinoorlatino|hislatino$|^slatino", "latino", ., perl = T) %>%
    gsub("hispanic|hispani|^shispanic|mispanic|^ehispanic|^mhispanic|^fhispanic|^bhispanic|hiispanic|hispanicc$|hispanicc$hispanorhispanic|hispaniclatinohispaniclatino|statehispanic|hispanicstate|hisapanic|hislatino$", "hispanic", ., perl = T) %>%
    gsub("^ehispanic|hispanic|hispani|hispan|hispa|hisp|his$|hispnc|hispni|hispn|hispanichispanic|hispanicic|hipanic|hispanicc$|hispanicorhispanic|hispanicorhispani|hispanicorhispa|hispanicorhisp|hispanicorhis|hispanichispanic", "hispanic", ., perl = T) %>%
    
    gsub("2race|2races|3races|4races|5races|rmixed", "two or more races", ., perl = T) %>%
    gsub("^ftwo|^stwo|^rtwo|2|twotwo", "two", ., perl = T)
  
  vec <- dplyr::recode(vec,
                       "1" = "white", "01" = "white", "001" = "white", "one" = "white",
                       "white" = "white",
                       "2" = "black or african american", "02" = "black or african american", "002" = "black or african american", "two" = "black or african american",
                       "blackorafricanamerican" = "black or african american",
                       "3" = "hispanic or latino", "03" = "hispanic or latino", "003" = "hispanic or latino", "three" = "hispanic or latino",
                       "hispanicorlatino" = "hispanic or latino",
                       "4" = "asian", "04" = "asian", "004" = "asian", "four" = "asian",
                       "asian" = "asian",
                       "5" = "american indian or alaska native", "05" = "american indian or alaska native", "005" = "american indian or alaska native", "five" = "american indian or alaska native",
                       "americanindianoralaskanative" = "american indian or alaska native",
                       "6" = "native hawaiian or other pacific islander", "06" = "native hawaiian or other pacific islander", "006" = "native hawaiian or other pacific islander", "six" = "native hawaiian or other pacific islander",
                       "nativehawaiianorotherpacificislander" = "native hawaiian or other pacific islander",
                       "7" = "two or more races", "07" = "two or more races", "007" = "two or more races", "seven" = "two or more races",
                       "twoormoreraces" = "two or more races")
  
  vec %<>% 
    gsub("[^[:alpha:]|[:space:]]", "", ., perl = T) %>%
    gsub("casian|whitewhitewhite|whitewhite|whitess", "white", ., perl = T) %>%
    gsub("asianasianasian|asianasian", "asian", ., perl = T) %>%
    gsub("blackblackblack|blackblack", "black", ., perl = T) %>%
    gsub("blackorafricanamerican|blackblackblackafricanamerican|blackafricanamerican|^black$|^blackafrican$|blackorafricanamericanblackorafricanamerican", "black or african american", ., perl = T) %>%
    gsub("multis|twoormores|twoormore$|twoormoreraces|twoormorerac|twoormorerace|twoormorestwoormores|whitehispanic|blackhispanic|twoormoreracestwoormoreraces", "two or more races", ., perl = T) %>%
    gsub("hispanicorlatinohispanicorlatino|hispanicorlatino|hlo|hispanic or latinohispanic or latino|^hispanic$|hislat$", "hispanic or latino", ., perl = T) %>%
    gsub("nativehawaiianorpacificislander|nativehawaiianorotherpacificislander|nativehawaiianorotherpacificislandernativehawaiianorotherpacificislander|asianpacificisler", "native hawaiian or other pacific islander", ., perl = T) %>%
    gsub("americanindianoralaskanativeamericanindianoralaskanative|nativeamericannativeamerican", "american indian or alaska native", ., perl = T)
  vec %<>% gsub("^NA$", NA, ., perl = T) # can't do perl = T here; yes you can!
  # as.factor(r4)
  vec
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
recode_races_regex <- function(df) {
  if(length(dplyr::select(df, dplyr::matches("race|ethnicity|ethni|ancestry"))) > 0){
    racedf <- data.frame(lapply(dplyr::select(df, dplyr::matches("race|ethnicity|ethni|ancestry")), recode_race_regex), stringsAsFactors = F)
    df <- data.frame(dplyr::select(df,-dplyr::matches("race|ethnicity|ethni|ancestry")), racedf, stringsAsFactors = F)
  }
  df
}

# ---------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to recode gender!
#' @param #recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param #extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @keywords cats
#' @export
#' @examples
#' recode_gender_regex()
recode_gender_regex <- function(vec) {
  vec %<>% 
    gsub("[^[:alnum:]]", "", ., perl = T) %>%
    gsub(recode_gender_getridofstrregex, "", ., perl = T)
  vec <- dplyr::recode(vec,
                       "1" = "male", "01" = "male","001" = "male", "one" = "male", "m" = "male",
                       "0" = "female", "00" = "female", "000" = "female", "zero" = "female",
                       "2" = "female", "02" = "female", "002" = "female", "two" = "female",
                       "f" = "female")
  vec %<>% 
    gsub("[^[:alpha:]]", "", ., perl = T) %>%
    gsub("femalefemalefemale|femalefemale|ffemale|females", "female", ., perl = T) %>%
    gsub("malemalemale|malemale|mmale|males|maley", "male", ., perl = T)
  vec
}

#'
#'
#' This function allows you to recode gender!
#' @param #recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param #extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @keywords cats
#' @export
#' @examples
#' recode_genders_regex()
recode_genders_regex <- function(df) {
  if(length(dplyr::select(df, dplyr::matches("gender|sex"))) > 0){
    genderdf <- data.frame(lapply(dplyr::select(df, dplyr::matches("gender|sex")), recode_gender_regex), stringsAsFactors = F)
    df <- data.frame(dplyr::select(df, -dplyr::matches("gender|sex")), genderdf, stringsAsFactors = F)
  }
  df
}

#' A Function
#'
#' This function allows you to recode gender!
#' @param #recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param #extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @keywords cats
#' @export
#' @examples
#' recode_races_and_genders_regex()
recode_races_and_genders_regex <- function(df, extragender = NULL, extrarace = NULL) {
  
  genderdf <- dplyr::select(df, dplyr::matches("gender|sex"))
  racedf <- dplyr::select(df, dplyr::matches("race|ethnicity|ethni|ancestry"))
  
  if(length(genderdf) > 0){
    genderdf <- lapply(genderdf, recode_gender_regex) 
  }
  if(length(racedf) > 0){
    racedf <- lapply(racedf, recode_race_regex) 
  }
  otherdf <- dplyr::select(df,-dplyr::matches("gender|sex|race|ethnicity|ethni|ancestry"))
  
  if(length(genderdf) == 0 & length(racedf) == 0 & length(otherdf) > 0){
    totaldf <- data.frame(df, stringsAsFactors = F)
  }
  if(length(genderdf) > 0 & length(racedf) == 0 & length(otherdf) == 0){
    totaldf <- data.frame(genderdf, stringsAsFactors = F)
  }
  if(length(genderdf) == 0 & length(racedf) > 0 & length(otherdf) == 0){
    totaldf <- data.frame(racedf, stringsAsFactors = F)
  } 
  if(length(genderdf) > 0 & length(racedf) > 0 & length(otherdf) > 0){
    totaldf <- data.frame(otherdf, genderdf, racedf, stringsAsFactors = F)
  }
  if(length(genderdf) == 0 & length(racedf) > 0 & length(otherdf) > 0){
    totaldf <- data.frame(otherdf, racedf, stringsAsFactors = F)
  }
  if(length(genderdf) > 0 & length(racedf) == 0 & length(otherdf) > 0){
    totaldf <- data.frame(otherdf, genderdf, stringsAsFactors = F)
  }
  if(length(genderdf) > 0 & length(racedf) > 0 & length(otherdf) == 0){
    totaldf <- data.frame(genderdf, racedf, stringsAsFactors = F)
  } 
  if(length(genderdf) == 0 & length(racedf) == 0 & length(otherdf) == 0){
    totaldf <- dfincase
  } 
  
  totaldf <- tryCatch(totaldf, error = function(e) dfincase)
  totaldf
}

#' A Function
#'
#' This function allows you to 
#' @param #recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param #extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @keywords cats
#' @export
#' @examples
#' recode_races_and_genders()
recode_races_and_genders <- function(df, extragender = NULL, extrarace = NULL, extra = NULL) {
  genderdf <- dplyr::select(df, dplyr::matches("gender|sex"))
  racedf <- dplyr::select(df, dplyr::matches("race|ethnicity|ethni|ancestry"))
  if(!is.null(extra)) extragender <- extrarace <- extra
  if(length(genderdf) > 0){
    genderdf <- lapply(genderdf, recode_gender_regex) 
    genderdf <- lapply(genderdf, function (x) recode_gender_j(x, recode_list = gender_list, extra = extragender))
  }
  if(length(racedf) > 0){
    racedf <- lapply(racedf, recode_race_regex) 
    racedf <- lapply(racedf, function (x) recode_race_j(x, recode_list = race_list, extra = extrarace))
  }
  otherdf <- dplyr::select(df,-dplyr::matches("gender|sex|race|ethnicity|ethni|ancestry"))
  
  if(length(genderdf) > 0 & length(racedf) > 0 & length(otherdf) > 0) totaldf <- data.frame(otherdf, genderdf, racedf, stringsAsFactors = F)
  if(length(genderdf) == 0 & length(racedf) > 0 & length(otherdf) > 0) totaldf <- data.frame(otherdf, racedf, stringsAsFactors = F)
  if(length(genderdf) > 0 & length(racedf) == 0 & length(otherdf) > 0) totaldf <- data.frame(otherdf, genderdf, stringsAsFactors = F)
  if(length(genderdf) > 0 & length(racedf) > 0 & length(otherdf) == 0) totaldf <- data.frame(genderdf, racedf, stringsAsFactors = F)
  if(length(genderdf) == 0 & length(racedf) == 0 & length(otherdf) > 0) totaldf <- data.frame(df, stringsAsFactors = F)
  if(length(genderdf) > 0 & length(racedf) == 0 & length(otherdf) == 0) totaldf <- data.frame(genderdf, stringsAsFactors = F)
  if(length(genderdf) == 0 & length(racedf) > 0 & length(otherdf) == 0) totaldf <- data.frame(racedf, stringsAsFactors = F)
  
  totaldf <- tryCatch(totaldf, error = function(e) dfincase)
  totaldf
}
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
#' A clean_dfs Function
#'
#' This function allows you to 
#' @param #recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param #extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @keywords cats
#' @export
#' @examples
#' clean_dfs()
clean_dfs <- function(df) {
  df <- lapply(df, function(x) {
    x <- gsub("  |\\|", " ", x, perl = T)
    x <- trimws(x, which = "both")
    x
  })
  df <- data.frame(df, stringsAsFactors = F)
  df <- dplyr::distinct(df) #%>% dplyr::select_if(not_all_na)
  df <- recode_na(df)
  df <- lapply(df, function(x) {
    x <- gsub("^NA$", NA, x, perl = T)
    x <- trimws(x, which = "both")
    x <- dplyr::na_if(x, "")
  }) %>% data.frame(., stringsAsFactors = F)
  grepl_names_to_na <- paste0(
    c("professor of", " professor", " lecturer", " educator", "fellowship", "instructor of", "faculty", "open rank", "lecturer of", " scholar ", "assistant","visiting scholar", 
      "curriculum", "postdoc", "teaching", "african american", "^asian$", "hispanic or latino", "native hawaiian", " innovation", " engineer", "comparative lit","nyuad lab","tenuretrack",
      "pacific islander", "native american", "american indian", "alaska native", "black or ", "^hispanic$", "interdisciplinary", "professor in"),
    collapse = "|")
  if(length(dplyr::select(df, dplyr::one_of("name"))) > 0) df %<>% dplyr::filter(!grepl(grepl_names_to_na, name))
  # dplyr::distinct(df) #%>% dplyr::select_if(not_all_na)
}
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
#' A Recode Race or Gender Function
#'
#' This function allows you to recode 
#' @param #recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param #extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @keywords cats
#' @export
#' @examples
#' recode_race_or_gender()
recode_race_or_gender <- function(vec, recode_list = list("NA" = c("undef", "none", "unknown"))) {
  vec <- gsub("[^[:alnum:]]", "", vec, perl = T)
  recode_key <- lapply(
    names(recode_list), 
    function(x) {
      to_recode <- recode_list[[x]]
      setNames(rep(x, length(to_recode)), to_recode)
    } )
  recode_key <- unlist(recode_key)
  vec <- dplyr::recode(vec, !!!recode_key)
  vec <- gsub("^NA$", NA, vec, perl = T)
  # as.factor(vec)
  vec
}
# ---------------------------------------------------------------------------------
#' A Recode Gender Function
#'
#' This function allows you to recode gender!
#' @param recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @keywords cats
#' @export
#' @examples
#' recode_gender_j()
recode_gender_j <- function(vec, 
                            recode_list = gender_list, 
                            extra = c("nothing", "other", "NA")) {
  extra <- match.arg(extra)
  vec <- gsub("[^[:alnum:]]", "", vec, perl = T)
  vec <- tolower(vec)
  recode_key <- lapply(
    names(recode_list), 
    function(x) {
      to_recode <- recode_list[[x]]
      setNames(rep(x, length(to_recode)), to_recode)
    } )
  recode_key <- unlist(recode_key)
  vec <- dplyr::recode(vec, !!!recode_key)
  
  if(extra == "nothing")
    # return(as.factor(gsub("^NA$", NA, vec)))
    return(gsub("^NA$", NA, vec, perl = T))
  if(extra == "other")
    return(ifelse(vec == "female", "female",
                  ifelse(vec == "male", "male",
                         ifelse(is.na(vec), NA,
                                ifelse(vec == "NA", NA, 
                                       ifelse(vec == "na", NA, 
                                              ifelse(vec == "other", "other", "other")))))))
  if(extra == "NA")
    return(ifelse(vec == "female", "female",
                  ifelse(vec == "male", "male",
                         ifelse(is.na(vec), NA,
                                ifelse(vec == "NA", NA, 
                                       ifelse(vec == "na", NA, 
                                              ifelse(vec == "other", "other", NA)))))))
  # as.factor(gsub("^NA$", NA, vec))
  gsub("^NA$", NA, vec, perl = T)
}
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @param recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @export
#' @examples
#' recode_gender2()
recode_gender2 <- function(vec, 
                           recode_list = gender_list, 
                           extra = NULL) {
  vec <- recode_gender_regex(vec)
  vec <- recode_gender_j(vec, recode_list = recode_list, extra = NULL)
  vec <- recode_gender_regex(vec)
  vec <- recode_gender_j(vec, recode_list = recode_list, extra = extra)
  vec
}
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to recode race
#' @param recode_list List of gender categories and codes Defaults to a built-in comprehensive list.
#' @param extra Extra categories that weren't able to be recoded. extra = c("nothing", "other", "NA"). Defaults to "nothing".
#' @export
#' @examples
#' recode_race_j(vec, recode_list = race_list, extra = c("nothing", "multirace", "NA"))
recode_race_j <- function(vec, 
                          recode_list = race_list, 
                          extra = c("nothing", "multirace", "NA")) {
  extra <- match.arg(extra)
  vec <- tolower(vec)
  recode_list <- race_list
  vec <- gsub("[^[:alnum:]]", "", vec, perl = T)
  recode_key <- lapply(
    names(recode_list), 
    function(x) {
      to_recode <- recode_list[[x]]
      setNames(rep(x, length(to_recode)), to_recode)
    } )
  recode_key <- unlist(recode_key)
  vec <- dplyr::recode(vec, !!!recode_key)
  
  if (extra == "nothing")
    return(gsub("^NA$", NA, vec, perl = T))
  if(extra == "multirace")
    return(ifelse(vec == "white", "white", 
                  ifelse(vec == "asian", "asian",
                         ifelse(vec == "hispanic or latino", "hispanic or latino",
                                ifelse(vec == "hispanicorlatino", "hispanic or latino",
                                       ifelse(vec == "black or african american", "black or african american",
                                              ifelse(vec == "blackorafricanamerican", "black or african american",
                                                     ifelse(vec == "american indian or alaska native", "american indian or alaska native",
                                                            ifelse(vec == "americanindianoralaskanative", "american indian or alaska native",
                                                                   ifelse(vec == "native hawaiian or other pacific islander", "native hawaiian or other pacific islander",
                                                                          ifelse(vec == "nativehawaiianorotherpacificislander", "native hawaiian or other pacific islander",
                                                                                 ifelse(is.na(vec), NA,
                                                                                        ifelse(vec == "NA", NA,
                                                                                               ifelse(vec == "two or more races", "two or more races",
                                                                                                      ifelse(vec == "twoormoreraces", "two or more races", "two or more races")))))))))))))))
  if (extra == "NA")
    return(ifelse(vec == "white", "white", 
                  ifelse(vec == "asian", "asian",
                         ifelse(vec == "hispanic or latino", "hispanic or latino",
                                ifelse(vec == "hispanicorlatino", "hispanic or latino",
                                       ifelse(vec == "black or african american", "black or african american",
                                              ifelse(vec == "blackorafricanamerican", "black or african american",
                                                     ifelse(vec == "american indian or alaska native", "american indian or alaska native",
                                                            ifelse(vec == "americanindianoralaskanative", "american indian or alaska native",
                                                                   ifelse(vec == "native hawaiian or other pacific islander", "native hawaiian or other pacific islander",
                                                                          ifelse(vec == "nativehawaiianorotherpacificislander", "native hawaiian or other pacific islander",
                                                                                 ifelse(is.na(vec), NA,
                                                                                        ifelse(vec == "NA", NA,
                                                                                               ifelse(vec == "two or more races", "two or more races",
                                                                                                      ifelse(vec == "twoormoreraces", "two or more races", NA)))))))))))))))
  gsub("^NA$", NA, vec, perl = T)
}
# ---------------------------------------------------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' recode_race2(vec, recode_list = race_list, extra = NULL)
recode_race2 <- function(vec, 
                         recode_list = race_list, 
                         extra = NULL) {
  vec <- recode_race_regex(vec)
  vec <- recode_race_j(vec, recode_list = recode_list, extra = NULL)
  vec <- recode_race_regex(vec)
  vec <- recode_race_j(vec, recode_list = recode_list, extra = extra)
  vec
}


race_specific <- mapply(c, 
                        (race_list_short <- lapply(race_list_short, sort)), 
                        (extra_race_vals_list <- lapply(
                          list("american indian or alaska native" = c("americanindianfemale", "americanindianfemale", "nativeamericanfemale", "nativeamericanmale"), 
                               "asian" = c("asianfemale", "asianmale","femaleasian", "maleasian"), 
                               "black or african american" = c("blackfemale", "blackmale","femaleblack", "maleblack", "blackman"), 
                               "hispanic or latino" = c("hispanicfemale", "hispanicmale","femalehispanic", "malehispanic"), 
                               "native hawaiian or other pacific islander" = c("nativehawaiianorotherpacificislanderfemale","nativehawaiianorotherpacificislandermale", "asianpacificisler"), 
                               "white" = c("WHITE"), 
                               "two or more races" = c("bnw", "wbh", "wbhn", "wbn", "nla", "bhn", "ash", "bh","otwos",
                                                       "aw", "hn", "hw", "anh", "ahw", "aiw", "hb"), 
                               "NA" = c("tals", "VALUE", "female", "male", "man", "woman", "w", "ww", "wm", "f"))
                          , sort)), 
                        SIMPLIFY=FALSE)


gender_specific <- mapply(c, 
                          (gender_list_short <- lapply(gender_list_short, sort)), 
                          (extra_gender_vals_list <- lapply(
                            list("male" = c("MALE", "man", "mannlich", "dude", "guy", "sir", "blackman"), 
                                 "female" = c("FEMALE", "woman", "girl", "feminine", "women", "blackwoman"),
                                 "NA" = c("tals", "VALUE", "black", "white", "asian","ormoreraces",
                                          "hispanicorlatino", "hispanic or latino","twoormoreraces","2ormoreraces",
                                          "black or african american", "blackorafricanamerican","nativehawaiianorpacificisler",
                                          "native hawaiian or other pacific islander", "nativehawaiianorotherpacificislander",
                                          "american indian or alaska native", "americanindianoralaskanative")), sort)), SIMPLIFY=FALSE)

getridofgenderspecific_regex <- paste0(
  c(names(race_list_short[-length(race_list_short)]), 
    names(race_list_short[-length(race_list_short)]) %>% gsub(" ", "", ., perl = T), 
    unique(unlist(strsplit(names(race_list_short[-length(race_list_short)]), " ")))),
  collapse = "|"
)
getridofracespecific_regex <- paste0(c("male", "female", "woman", "^man$", "women", 
                                       "regular"), collapse = "|")

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
recode_gender_specific <- function(vec, extra = NULL) {
  vec <- recode_gender_j(vec, recode_list = gender_specific, extra = NULL)
  # vec <- gsub("[^[:alpha:]]", "", vec, perl = T)
  vec <- gsub(getridofgenderspecific_regex, "", vec, perl = T)
  vec <- gsub(getridofgenderspecific_regex, "", vec, perl = T)
  vec <- gsub("^$", NA, vec, perl = T)
  vec
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
recode_race_specific <- function(vec, extra = NULL) {
  vec <- recode_race_j(vec, recode_list = race_specific, extra = NULL)
  # vec <- gsub("[^[:alpha:]]", "", vec, perl = T)
  vec <- gsub(getridofracespecific_regex, "", vec, perl = T)
  vec <- gsub(getridofracespecific_regex, "", vec, perl = T)
  vec <- gsub("^$", NA, vec, perl = T)
  vec
}




# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
clean_recode <- function(df, scrub = c("once", "double"), extrarace = NULL, extragender = NULL) {
  scrub <- match.arg(scrub)
  df <- clean_dfs(df) 
  df <- recode_races_and_genders(df, extrarace = extrarace, extragender = extragender)
  df <- data.frame(lapply(df, stringi::stri_enc_toutf8), stringsAsFactors = F)
  
  if(scrub == "once")return(dplyr::distinct(df))
  if(scrub == "double"){
    df <- clean_dfs(df) 
    df <- recode_races_and_genders(df, extrarace = extrarace, extragender = extragender) 
    return(df)
  } else {
    return(df)
  }
}

# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' prestep_preprocess_all_cols(mylist, subsets = 2, type = NULL, extent = NULL)
prestep_preprocess_all_cols <- function(mylist, subsets = 2, type = NULL, extent = NULL) {
  by <- round(length(mylist) / subsets)
  if(by < 1){
    by <- 2
  }
  if(by > length(mylist)){
    by <- length(mylist)
  }
  
  lapply(seq(1, (length(mylist)), by), 
         function (x) {
           start <- x
           end <- x + (by - 1)
           diff <- end - length(mylist)
           end <- ifelse(diff <= 0, end, end - diff)
           preprocess_all_cols(mylist[start:end], type = type, extent = extent)
         } 
  ) %>%
    dplyr::bind_rows() %>% 
    # lapply(., as.factor) %>% 
    # data.frame() %>% 
    dplyr::distinct()
}

# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
prestep_preprocess_data <- function(mylist, subsets = 2, 
                                    type = NULL, 
                                    extent = NULL) {
  by <- round(length(mylist) / subsets)
  if(by < 1){
    by <- 2
  }
  if(by > length(mylist)){
    by <- length(mylist)
  }
  
  lapply(seq(1, (length(mylist)), by), 
         function (x) {
           start <- x
           end <- x + (by - 1)
           diff <- end - length(mylist)
           end <- ifelse(diff <= 0, end, end - diff)
           preprocess_data(mylist[start:end], type = type, extent = extent)
         } 
  ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct()
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' multistep_preprocess_all_cols(mylist, type = NULL, subsets = 2, subsubsets = 2, write = FALSE, featherpath = "~/")
multistep_preprocess_all_cols <- function(mylist, type = NULL, subsets = 2, subsubsets = 2, write = FALSE, featherpath = "~/") {
  write = match.arg(write)
  by <- round(length(mylist) / subsets)
  if(by < 1) by <- 2
  if(by > length(mylist)) by <- length(mylist)
  if (write)
    return(lapply(seq(1, (length(mylist)), by), 
                  function (x) {
                    start <- x
                    end <- x + (by - 1)
                    diff <- end - length(mylist)
                    end <- ifelse(diff <= 0, end, end - diff)
                    feather::write_feather(prestep_preprocess_all_cols(mylist[start:end], 
                                                                       subsets = subsets, 
                                                                       subsubsets = subsubsets,
                                                                       type = type), 
                                           paste0(featherpath, "block", round4(start), "to", round4(end), ".f"))
                  }
    ) %>%  
      dplyr::bind_rows() %>%  
      dplyr::distinct())
  lapply(seq(1, (length(mylist)), by), 
         function (x) {
           start <- x
           end <- x + (by - 1)
           diff <- end - length(mylist)
           end <- ifelse(diff <= 0, end, end - diff)
           prestep_preprocess_all_cols(mylist[start:end], 
                                       subsets = subsets, 
                                       subsubsets = subsubsets,
                                       type = type)
         }
  ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct()
}

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
multistep_gather_join_first_last_name <- function(df, subsets = 2) {
  by <- round(nrow(df) / subsets)
  if(by < 1) by <- 2
  if(by > length(mylist)) by <- length(mylist)
  lapply(seq(1, (nrow(df)), by), 
         function (x) {
           start <- x
           end <- x + (by - 1)
           diff <- end - nrow(df)
           end <- ifelse(diff <= 0, end, end - diff)
           gather_join_first_last_name(df[start:end, ])
         } 
  ) %>% 
    dplyr::bind_rows() %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>% 
    dplyr::distinct()
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
multistep_gather_race_and_gender <- function(df, subsets = 2) {
  by <- round(nrow(df) / subsets)
  if(by < 1) by <- 2
  if(by > length(mylist)) by <- length(mylist)
  lapply(seq(1, (nrow(df)), by), 
         function (x) {
           start <- x
           end <- x + (by - 1)
           diff <- end - nrow(df)
           end <- ifelse(diff <= 0, end, end - diff)
           gather_race_and_gender(df[start:end, ])
         } 
  ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::distinct()
}
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
multistep_clean_dfs <- function(df, subsets = 2) {
  mylist <- df
  by <- round(nrow(mylist) / subsets)
  if(by < 1)by <- 2
  if(by > length(mylist)){
    by <- length(mylist)
  }
  lapply(seq(1, (nrow(mylist)), by), 
         function (x) {
           start <- x
           end <- x + (by - 1)
           diff <- end - nrow(mylist)
           end <- ifelse(diff <= 0, end, end - diff)
           clean_dfs(mylist[start:end, ])
         } 
  ) %>% 
    dplyr::bind_rows() %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>% 
    dplyr::distinct()
}
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
multistep_recode_race_and_gender <- function(df, subsets = 2) {
  mylist <- df
  by <- round(nrow(mylist) / subsets)
  if(by < 1) by <- 2
  if(by > length(mylist)) by <- length(mylist)
  lapply(seq(1, (nrow(mylist)), by), 
         function (x) {
           start <- x
           end <- x + (by - 1)
           diff <- end - nrow(mylist)
           end <- ifelse(diff <= 0, end, end - diff)
           recode_race_and_gender(mylist[start:end, ])
         } 
  ) %>% 
    dplyr::bind_rows() %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>% 
    dplyr::distinct()
}
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
multistep_clean_recode <- function(df, subsets = 2, scrub = NULL, extrarace = NULL, extragender = NULL) {
  mylist <- df
  by <- round(nrow(mylist) / subsets)
  if(by < 1) by <- 2
  if(by > length(mylist)) by <- length(mylist)
  
  lapply(seq(1, (nrow(mylist)), by), 
         function (x) {
           start <- x
           end <- x + (by - 1)
           diff <- end - nrow(mylist)
           end <- ifelse(diff <= 0, end, end - diff)
           clean_recode(mylist[start:end, ], scrub = scrub, extrarace = extrarace, extragender = extragender)
         } 
  ) %>% 
    dplyr::bind_rows() %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>% 
    dplyr::distinct()
}
# ---------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
multistep_split_files <- function(filelist = NULL,
                                  inpath = NULL,
                                  pattern = NULL,
                                  newdir = NULL,
                                  subsets = NULL, #subsubsets = 2,
                                  by = NULL,
                                  extra = NULL,
                                  outpath = "~/",
                                  filename_prefix = "DEFAULTNAME") {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>% gsub("__", "_", .)
  }
  if(is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  } 
  if(!is.null(newdir)) {
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  }
  lapply(filelist, function (xx) {
    mylist <- get(load(xx))
    mylist <- tryCatch(get(load(xx)), 
                       error = function(e) feather::read_feather(xx))  
    if(is.list(mylist) & !is.data.frame(mylist)){
      mylist <- try_compact(mylist)    
      mylist <- try_combine(mylist)    
    }
    if(is.null(by)) by <- round(length(mylist) / subsets)
    if(by < 1) by <- 2
    if(by > length(mylist)) by <- length(mylist)
    lapply(seq(1, (length(mylist)), by), function (x) {
      start <- x
      end <- x + (by - 1)
      diff <- end - length(mylist)
      end <- ifelse(diff <= 0, end, end - diff)
      time <- system.time(snippet <- mylist[start:end])
      filename <- paste0(dir_path, "/", filename_prefix, "_", 
                         gsub("[^[:alnum:]]", "", xx), "_", 
                         round4(start), "to", round4(end), ".f") %>% 
        gsub("\\/\\/", "\\/",. ) %>% gsub("__", "_",. ) %>%
        gsub("^\\_|_$|^_|_$|\\<_", "", .) %>% gsub("^\\_|\\_$|^_", "", .)
      feather::write_feather(snippet, filename)
      print(time)
      # data.frame(dir = dir_path, filename = filename)
    }
    )
  }
  )
}

#---------------------------------------------------------------------------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
regulars <- function (x, extra = NULL, extent = "thorough"){
  trycomb <- try_combine(x)
  trycomp <- try_compact(x)
  
  if(is.list(trycomp)) x <- try_compact(x)
  if(is.list(trycomb)) x <- try_combine(x)
  
  x %<>%
    preprocess_data(., extent = "thorough") %>%
    #  dplyr::select_if(not_all_na) %>%
    dplyr::distinct()
  
  x %<>%
    dealwith_racegender_variable() %>%
    dplyr::distinct()
  
  x <- tryCatch(x, error = function(e) dfincase)
  
  if(is.null(x) | length(x) == 0) x <- dfincase
  
  # In case you have no race columns in this subset
  if(is.null(x$gender)|is.null(x$name)|is.null(x$race))x <- dplyr::bind_rows(x, dfincase)
  
  x %<>%
    gather_race_and_gender(.) %>%
    dplyr::distinct()
  
  x %<>%
    recode_races_and_genders(extrarace = extra, extragender = extra)  %>%
    dplyr::distinct()
  
  x %<>%
    gather_join_first_last_name(.) %>%
    dplyr::distinct()
  
  x %<>%
    recode_na() %>%
    dplyr::distinct()
  
  x %<>%
    clean_recode(., scrub = "once") %>%
    dplyr::distinct()
  #-------
  x %<>%
    dplyr::mutate(gender_r = race,
                  race_g = gender) %>%
    dplyr::distinct()
  
  x %<>%
    gather_race_and_gender(.) %>%
    dplyr::distinct()
  
  if(!is.null(x$gender) & !is.null(x$race)){
    x %<>%
      dplyr::mutate(gender_r = race,
                    race_g = gender) %>%
      dplyr::distinct()
    
    x %<>%
      gather_race_and_gender(.) %>%
      dplyr::distinct()
    
    x %<>% dplyr::mutate(gender = recode_gender_specific(gender, extra = extra),
                         race = recode_race_specific(race, extra = extra))
  }
  
  if(!is.null(x$gender) & is.null(x$race)){
    x %<>% dplyr::mutate(race_g = gender) %>% dplyr::distinct()
    x %<>% gather_race_and_gender(.) %>% dplyr::distinct()
    x %<>% dplyr::mutate(race = recode_race_specific(race, extra = extra))
  }
  
  if(is.null(x$gender) & !is.null(x$race)){
    x %<>% dplyr::mutate(gender_r = race) %>% dplyr::distinct()
    x %<>% gather_race_and_gender(.) %>% dplyr::distinct()
    x %<>% dplyr::mutate(gender = recode_gender_specific(gender, extra = extra))
  }
  
  x %<>% dplyr::distinct()
  #-------
  x %<>%
    recode_races_and_genders() %>%
    recode_races_and_genders() %>%
    dplyr::select_if(not_all_na)  %>%
    dplyr::distinct()
  # if(!is.null(x$name)) x <- dplyr::filter(x, !is.na(name))
  if(!is.null(x$gender) & !is.null(x$race)) x <- dplyr::filter(x, !is.na(gender) | !is.na(race))
  x
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
featherdump_regulars_filelist_fun <- function(filelist = NULL,
                                              inpath = NULL,
                                              pattern = NULL,
                                              newdir = NULL,
                                              subsets = 2, subsubsets = 2,
                                              extra = NULL,
                                              outpath = "~/",
                                              filename_prefix = "DEFAULTNAME") {
  
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  
  if(is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  } 
  
  if(!is.null(newdir)) {
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  }
  
  lapply(filelist, function (xx) 
  {
    f <- get(load(xx))
    f <- tryCatch(plyr::compact(f),  error = function(e) f)      
    f <- tryCatch(dplyr::combine(f), error = function(e) f)  
    mylist <- f
    
    by <- round(length(mylist) / subsets)
    
    if(by < 1) by <- 2
    if(by > length(mylist))by <- length(mylist)
    
    lapply(seq(1, (length(mylist)), by), function(x){
      start <- x
      end <- x + (by - 1)
      diff <- end - length(mylist)
      end <- ifelse(diff <= 0, end, end - diff)
      
      time <- system.time(snippet <- regulars(mylist[start:end], 
                                              extra = extra))
      
      filename <- paste0(dir_path, "/", filename_prefix, 
                         "_", 
                         gsub("[^[:alnum:]]", "", xx), 
                         "_", 
                         round4(start), "to", round4(end),
                         ".f") %>% 
        gsub("\\/\\/", "\\/",. ) %>% 
        gsub("__", "_",. ) %>%
        gsub("^\\_|_$|^_|_$|\\<_", "", .) %>%
        gsub("^\\_|\\_$|^_", "", .)
      
      feather::write_feather(snippet, filename)
      print(paste0(dim(snippet), " -- ", filename))
      print(time)
      # data.frame(dir = dir_path, filename = filename)
    }
    )
  }
  )
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
load_save_rdata <- function(filelist = NULL,
                            inpath = NULL,
                            pattern = NULL,
                            newdir = NULL,
                            subsets = 2, subsubsets = 2,
                            extra = NULL,
                            outpath = "~/",
                            filename_prefix = "DEFAULTNAME") {
  
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", ., perl = T) %>%
      gsub("__", "_", ., perl = T)
  }
  
  if(is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                 gsub("\\/\\/", "\\/", ., perl = T) %>%
                 gsub("__", "_", ., perl = T))
  } 
  
  if(!is.null(newdir)) {
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", ., perl = T) %>%
                 gsub("__", "_", ., perl = T))
  }
  
  lapply(filelist, function (xx) 
  {
    f <- get(load(xx))
    f <- tryCatch(plyr::compact(f),  error = function(e) f)      
    f <- tryCatch(dplyr::combine(f), error = function(e) f)  
    mylist <- f
    save(mylist, file = paste0(dir_path, "/", filename_prefix, 
                               gsub("[^[:alnum:]]", "", xx), ".rda") %>% 
           gsub("\\/\\/", "\\/",. , perl = T) %>% gsub("__", "_",. , perl = T) %>%
           gsub("^_|_$|\\<_|_\\>", "", ., perl = T) %>% gsub("\\<\\_|\\_\\>", "", .))
    
  })
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
featherdump_anomalies_fun <- function(filelist = NULL,
                                      inpath = NULL,
                                      pattern = "\\.",
                                      outpath = "~",
                                      newdir = NULL,
                                      subsets = 2, subsubsets = 2,
                                      path = "~/",
                                      filename_prefix = "anomalies") {
  
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  
  if(is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  }
  
  if(!is.null(newdir)) {
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  }
  
  lapply(filelist, function (xx)
  {
    f <- get(load(xx))
    f <- tryCatch(plyr::compact(f),
                  error = function(e) f)
    f <- tryCatch(dplyr::combine(f),
                  error = function(e) f)
    mylist <- f
    
    by <- round(length(mylist) / subsets)
    if(by < 1) by <- 2
    if(by > length(mylist)) by <- length(mylist)
    
    lapply(seq(1, (length(mylist)), by), function (x){
      start <- x
      end <- x + (by - 1)
      diff <- end - length(mylist)
      end <- ifelse(diff <= 0, end, end - diff)
      
      snippet <- anomalies(mylist[start:end])
      
      filename <- paste0(dir_path, "/", filename_prefix,
                         "_",
                         gsub("[^[:alnum:]]", "", xx),
                         "_",
                         round4(start), "to", round4(end),
                         ".f") %>%
        gsub("\\/\\/", "\\/",. ) %>%
        gsub("__", "_",. ) %>%
        gsub("^_|_$", "", .) %>%
        gsub("^\\_|\\_$|^_|\\<_", "", .)
      
      feather::write_feather(snippet, filename)
      
      # data.frame(dir = dir_path, filename = filename)
    }
    )
  }
  )
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
featherdump_anomalies_filelist_fun <- function(filelist = NULL,
                                               inpath = NULL,
                                               pattern = NULL,
                                               newdir = NULL,
                                               subsets = 2, subsubsets = 2,
                                               extra = NULL,
                                               outpath = "~/",
                                               filename_prefix = "DEFAULTNAME") {
  
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  
  if(is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  } 
  
  if(!is.null(newdir)) {
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  }
  
  lapply(filelist, function (xx) 
  {
    f <- get(load(xx))
    f <- tryCatch(plyr::compact(f), 
                  error = function(e) f)      
    f <- tryCatch(dplyr::combine(f), 
                  error = function(e) f)  
    mylist <- f
    
    by <- round(length(mylist) / subsets)
    
    if(by < 1) by <- 2
    if(by > length(mylist)) by <- length(mylist)
    
    lapply(seq(1, (length(mylist)), by), function(x) {
      
      start <- x
      end <- x + (by - 1)
      diff <- end - length(mylist)
      end <- ifelse(diff <= 0, end, end - diff)
      
      time <- system.time(snippet <- anomalies(mylist[start:end]))
      
      filename <- paste0(dir_path, "/", filename_prefix, 
                         "_", 
                         gsub("[^[:alnum:]]", "", xx), 
                         "_", 
                         round4(start), "to", round4(end),
                         ".f") %>% 
        gsub("\\/\\/", "\\/",. ) %>% 
        gsub("__", "_",. ) %>%
        gsub("^\\_|_$|^_|_$|\\<_", "", .) %>%
        gsub("^\\_|\\_$|^_", "", .)
      
      feather::write_feather(snippet, filename)
      print(time)
      print(paste0(dim(snippet), " -- ", filename))
      # data.frame(dir = dir_path, filename = filename)
    }
    )
  }
  )
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
load_rdata_files <- function (files){
  loaded <- lapply(files, function (x) 
  {
    f <- get(load(x))
    f <- tryCatch(plyr::compact(f), 
                  error = function(e) f)      
    f <- tryCatch(dplyr::combine(f), 
                  error = function(e) f)  
  })
  
  loaded <- tryCatch(dplyr::combine(dplyr::combine(loaded)), 
                     error = function(e) {
                       tryCatch(dplyr::combine(loaded), 
                                error = function(e) loaded)
                     })
  loaded <- tryCatch(dplyr::combine(loaded), # not really necessary but just in case
                     error = function(e) loaded)
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
load_feather_files <- function (files){
  loaded <- lapply(files, function (x) 
  {
    f <- tryCatch(feather::read_feather(x), 
                  error = function(e) x ) 
    f <- tryCatch(plyr::compact(f), 
                  error = function(e) f)      
    f <- tryCatch(dplyr::combine(f), 
                  error = function(e) f)  
  })
  
  loaded <- tryCatch(dplyr::combine(dplyr::combine(loaded)), 
                     error = function(e) {
                       tryCatch(dplyr::combine(loaded), 
                                error = function(e) loaded)
                     })
  loaded <- tryCatch(dplyr::combine(loaded), # not really necessary but just in case
                     error = function(e) loaded)
  
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_merge_write_feathers <- function(filelist = NULL, 
                                      inpath = NULL,
                                      pattern = NULL,
                                      newdir = NULL,
                                      outpath = "~/",
                                      filename_prefix = "DEFAULT") {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  
  if(is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  } 
  
  if(!is.null(newdir)) {
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  }
  
  loaded <- lapply(filelist, feather::read_feather) 
  loaded <- dplyr::bind_rows(loaded)
  
  filename <- paste0(dir_path, "/", filename_prefix, "_1to", length(filelist), ".f") %>%
    gsub("\\/\\/", "\\/", .) %>%
    gsub("__", "_", .) %>%
    gsub("^_|_$", "", .) %>%
    gsub("^\\_|\\_$|^_|\\<_", "", .)
  
  feather::write_feather(loaded, filename)
  
  # data.frame(dir = dir_path, filename = filename)
  
}



#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_merge_feathers <- function(filelist = NULL, 
                                inpath = NULL,
                                pattern = NULL, 
                                recode_extra_na = F) {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  
  loaded <- lapply(filelist, feather::read_feather) 
  loaded <- dplyr::bind_rows(loaded)
  loaded <- dplyr::distinct(loaded)
  
  if(recode_extra_na){
    loaded %<>% recode_races_and_genders(., extra = "NA")
  }
  
  if(!is.null(loaded$name) & !is.null(loaded$gender) & !is.null(loaded$race)){
    loaded <- dplyr::filter(loaded,# !is.na(name), 
                            !is.na(gender) | !is.na(race))
  }
  loaded
}



#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_feathers <- function(filelist = NULL, 
                          inpath = NULL,
                          pattern = NULL, bind=T,
                          recode_extra_na = F){
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  loaded <- lapply(filelist, feather::read_feather) 
  if(bind){
    loaded <- dplyr::bind_rows(loaded)
    loaded <- dplyr::distinct(loaded)
    if(recode_extra_na) loaded %<>% recode_races_and_genders(., extra = "NA")
  }
  loaded
}



#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_rdas <- function(filelist = NULL, 
                      inpath = NULL,
                      pattern = NULL, bind=F,
                      recode_extra_na = F){
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  loaded <- lapply(filelist, function(x) get(load(x))) 
  if(bind){
    loaded <- dplyr::bind_rows(loaded)
    loaded <- dplyr::distinct(loaded)
    if(recode_extra_na) loaded %<>% recode_races_and_genders(., extra = "NA")
  }
  loaded
}



#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_files_to_rda <- function(filelist,
                              path = "~/",
                              filename_prefix = "nrg_data"){
  
  x <- filelist
  
  dir.create(dir_path <- paste0(path, "/", filename_prefix, "_dump") %>% 
               stringr::str_replace_all(., "\\/\\/", "\\/") %>% 
               stringr::str_replace_all(., "__", "_"))
  
  lapply(filelist, function (xx) {
    
    data <- tryCatch(feather::read_feather(xx), 
                     error = function(e) NULL) 
    
    if(is.null(data)){
      data <- tryCatch(read_excel_allsheets_files(xx), 
                       error = function(e) NULL) 
    }
    
    if(is.null(data)){
      data <- tryCatch(read_csv_files(xx), 
                       error = function(e) NULL) 
    }
    
    filename <- paste0(dir_path, "/", filename_prefix, 
                       "_", 
                       gsub("[^[:alnum:]]", "", xx), 
                       "_", 
                       "1", "to", nrow(data),
                       ".rda") %>% 
      stringr::str_replace_all(., "\\/\\/", "\\/") %>% 
      stringr::str_replace_all(., "__", "_")
    
    
    save(data, file = filename)
  })
  
}



#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_rda_merge_write_feathers <- function(filelist = NULL, 
                                          inpath = NULL,
                                          pattern = NULL,
                                          newdir = NULL,
                                          outpath = "~/",
                                          filename_prefix = "DEFAULT") {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", ., perl = T) %>%
      gsub("__", "_", ., perl = T)
  }
  
  if(is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                 gsub("\\/\\/", "\\/", ., perl = T) %>%
                 gsub("__", "_", ., perl = T))
  } 
  
  if(!is.null(newdir)) {
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", ., perl = T) %>%
                 gsub("__", "_", ., perl = T))
  }
  
  loaded <- lapply(rdata, function (x){
    x <- load(x)
    x <- get(x)
  })
  
  loaded <- try_combine_compact(loaded)
  
  loaded <- dplyr::bind_rows(loaded)
  
  filename <- paste0(dir_path, "/", filename_prefix, "_1to", length(filelist), ".f") %>%
    gsub("\\/\\/", "\\/", ., perl = T) %>%
    gsub("__", "_", ., perl = T) %>%
    gsub("^_|_$", "", ., perl = T) %>%
    gsub("\\<\\_|\\_\\>", "", .)
  
  feather::write_feather(loaded, filename)
  
  # data.frame(dir = dir_path, filename = filename)
  
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_rdas <- function(filelist = NULL, 
                      inpath = NULL,
                      pattern = NULL) {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", ., perl = T) %>%
      gsub("__", "_", ., perl = T)
  }
  
  loaded <- lapply(filelist, function (x){
    x <- load(x)
    x <- get(x)
  })
  loaded
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_files_to_feather()
read_files_to_feather <- function(filelist,
                                  path = "~/",
                                  filename_prefix = "nrg_data"){
  
  x <- filelist
  
  dir.create(dir_path <- paste0(path, "/", filename_prefix, "_dump") %>% 
               stringr::str_replace_all(., "\\/\\/", "\\/") %>% 
               stringr::str_replace_all(., "__", "_"))
  
  lapply(filelist, function (xx) {
    
    data <- tryCatch(feather::read_feather(xx), 
                     error = function(e) NULL) 
    
    if(is.null(data)){
      data <- tryCatch(read_excel_allsheets_files(xx), 
                       error = function(e) NULL) 
      data <- dplyr::bind_rows(data) #%>% data.frame()
    }
    
    if(is.null(data)){
      data <- tryCatch(read_csv_files(xx), 
                       error = function(e) NULL) 
    }
    
    if(is.null(data)){
      data <- data.frame(data, stringsAsFactors = F)
    }
    
    filename <- paste0(dir_path, "/", filename_prefix, 
                       "_", 
                       gsub("[^[:alnum:]]", "", xx), 
                       "_", 
                       "1", "to", nrow(data),
                       ".f") %>% 
      stringr::str_replace_all(., "\\/\\/", "\\/") %>% 
      stringr::str_replace_all(., "__", "_")
    
    
    feather::write_feather(data, filename)
    
    # data.frame(dir = dir_path, filename = filename)
  })
  
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_merge_write_feathers <- function(filelist = NULL, 
                                      inpath = NULL,
                                      pattern = NULL,
                                      newdir = NULL,
                                      outpath = "~/",
                                      filename_prefix = "DEFAULT") {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  
  if(is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  } 
  
  if(!is.null(newdir)) {
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
  }
  
  loaded <- lapply(filelist, feather::read_feather) 
  loaded <- dplyr::bind_rows(loaded)
  
  filename <- paste0(dir_path, "/", filename_prefix, "_1to", length(filelist), ".f") %>%
    gsub("\\/\\/", "\\/", .) %>%
    gsub("__", "_", .) %>%
    gsub("^_|_$", "", .) %>%
    gsub("^\\_|\\_$|^_|\\<_", "", .)
  
  feather::write_feather(loaded, filename)
  
  # data.frame(dir = dir_path, filename = filename)
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_feathers_recode_write <- function(filelist = NULL, 
                                       inpath = NULL,
                                       pattern = NULL,
                                       outpath = "",
                                       newdir = NULL) {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  if (is.null(newdir)){
    loaded <- lapply(filelist, function (file) {
      print("old:")
      print(dim(x1 <- feather::read_feather(file)))
      x <- clean_dfs(x1)
      x <- recode_races_and_genders(x)
      x <- dplyr::distinct(x)
      
      if(!is.null(x$name) & !is.null(x$gender) & !is.null(x$race)){
        x <- dplyr::filter(x, # !is.na(name), 
                           !is.na(race) | !is.na(gender))
      }
      if(!is.null(x$name) & !is.null(x$gender) & is.null(x$race)){
        x <- dplyr::filter(x,  # !is.na(name), 
                           !is.na(gender))
      }
      if(!is.null(x$name) & is.null(x$gender) & !is.null(x$race)){
        x <- dplyr::filter(x,  # !is.na(name), 
                           !is.na(race))
      }
      x2 <- x
      print(dim(feather::write_feather(x, file)))
    }) 
  } 
  if (!is.null(newdir)){
    dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                 gsub("\\/\\/", "\\/", .) %>%
                 gsub("__", "_", .))
    loaded <- lapply(filelist, function (file) {
      print("old:")
      print(dim(x1 <- feather::read_feather(file)))
      x <- clean_dfs(x1)
      x <- recode_races_and_genders(x)
      x <- dplyr::distinct(x)
      
      if(!is.null(x$name) & !is.null(x$gender) & !is.null(x$race)){
        x <- dplyr::filter(x, #!is.na(name), 
                           !is.na(race) | !is.na(gender))
      }
      if(!is.null(x$name) & !is.null(x$gender) & is.null(x$race)){
        x <- dplyr::filter(x, #!is.na(name), 
                           !is.na(gender))
      }
      if(!is.null(x$name) & is.null(x$gender) & !is.null(x$race)){
        x <- dplyr::filter(x, #!is.na(name), 
                           !is.na(race))
      }
      
      filename <- gsub("[^[:alnum:]]", "", file)
      print(dim(feather::write_feather(x, paste0(dir_path,  "/",filename, ".f") %>%
                                         gsub("\\/\\/", "/", .))))
    })
    # print(data.frame("originalrows" = nrow(x1), 
    #                  "newrows" = nrow(x)))
  }
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
fix_encoding <- function(df, originalEncoding = "latin1") {
  numCols <- ncol(df)
  for (col in 1:numCols)
    if(class(df[, col]) == "character"){
      Encoding(df[, col]) <- originalEncoding
    }
  return(df)
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
regulars_anomalies <- function(x, extra = NULL, extent = "thorough"){
  reg <- data.frame(regulars(x, extra = extra), stringsAsFactors = F)
  anom <- data.frame(anomalies(x), stringsAsFactors = F)
  all <- dplyr::bind_rows(reg, anom) %>% dplyr::distinct()
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
featherdump_regulars_anomalies <- function(filelist = NULL,
                                           inpath = NULL,
                                           pattern = NULL,
                                           newdir = NULL,
                                           subsets = 2, subsubsets = 2,
                                           extra = NULL,
                                           outpath = "~/",
                                           filename_prefix = "DEFAULTNAME") {
  
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  
  if(is.null(newdir)) dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                                   gsub("\\/\\/", "\\/", .) %>%
                                   gsub("__", "_", .))
  
  if(!is.null(newdir)) dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                                    gsub("\\/\\/", "\\/", .) %>%
                                    gsub("__", "_", .))
  lapply(filelist, function (xx) {
    f <- get(load(xx))
    f <- tryCatch(plyr::compact(f), error = function(e) f)      
    f <- tryCatch(dplyr::combine(f), error = function(e) f)  
    mylist <- f
    by <- round(length(mylist) / subsets)
    if(by < 1) by <- 2
    if(by > length(mylist)) by <- length(mylist)
    lapply(seq(1, (length(mylist)), by), function (x) {
      start <- x
      end <- x + (by - 1)
      diff <- end - length(mylist)
      end <- ifelse(diff <= 0, end, end - diff)
      time <- system.time(snippet <- regulars_anomalies(mylist[start:end], extra = extra))
      filename <- paste0(dir_path, "/", filename_prefix,  "_", 
                         gsub("[^[:alnum:]]", "", xx),  "_", 
                         round4(start), "to", round4(end), ".f") %>% 
        gsub("\\/\\/", "\\/",. ) %>% 
        gsub("__", "_",. ) %>%
        gsub("^\\_|_$|^_|_$|\\<_", "", .) %>%
        gsub("^\\_|\\_$|^_", "", .)
      feather::write_feather(snippet, filename)
      print(paste0(dim(snippet), " -- ", filename))
      print(time)
      # data.frame(dir = dir_path, filename = filename)
    }
    )
  }
  )
}












#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
merge_many_write_many <- function(filelist = NULL,
                                  inpath = NULL,
                                  pattern = NULL,
                                  newdir = NULL,
                                  subsets = NULL,
                                  by = 1000000,
                                  extra = NULL,
                                  namegenderonly = F,
                                  Ng = F,
                                  Nr = F,
                                  fullnamegenderonly = F,
                                  nameraceonly = F,
                                  firstnameonly = F,
                                  firstnamegenderonly=F,
                                  firstnameraceonly=F,
                                  recode_extra_na = F,
                                  outpath = "~/",
                                  filename_prefix = "") {
  
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>%
      gsub("__", "_", .)
  }
  
  if(is.null(newdir)) dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%
                                   gsub("\\/\\/", "\\/", .) %>%
                                   gsub("__", "_", .))
  if(!is.null(newdir)) dir.create(dir_path <- paste0(outpath, "/", newdir) %>%
                                    gsub("\\/\\/", "\\/", .) %>%
                                    gsub("__", "_", .))
  df <- read_merge_feathers(filelist = filelist, inpath = inpath, pattern = pattern)
  dfname <- "nrg"
  
  if(Ng) {
    df %<>% 
      dplyr::select(dplyr::matches("name|gender")) %>% 
      na.omit() %>%
      mutate(lastname = toupper(lastname),
             gender = as.factor(gender),
             name = paste0(firstname, " ", lastname)) %>%
      select(name, gender) %>% distinct()
    print(dfname <- "Ng")
  }
  
  if(Nr) {
    df %<>% 
      dplyr::select(dplyr::matches("name|race")) %>% 
      na.omit() %>%
      mutate(lastname = toupper(lastname),
             race = as.factor(race),
             name = paste0(firstname, " ", lastname)) %>%
      select(name, race) %>% distinct()
    print(dfname <- "Nr")
  }
  
  if(firstnamegenderonly) {
    df %<>% 
      dplyr::select(dplyr::matches("firstname|gender")) %>% 
      na.omit() %>%
      dplyr::distinct()
    print(dfname <- "fng")
  }
  if(firstnameraceonly) {
    df %<>% 
      dplyr::select(dplyr::matches("firstname|race")) %>% 
      na.omit() %>%
      dplyr::distinct()
    print(dfname <- "fnr")
  }
  if(namegenderonly) {
    df %<>% 
      dplyr::select(dplyr::matches("name|gender")) %>% 
      dplyr::filter(!is.na(name) | !is.na(firstname) | !is.na(lastname), !is.na(gender)) %>%
      dplyr::distinct()
    print(dfname <- "ng")
  }
  if(fullnamegenderonly) {
    df %<>% 
      dplyr::select(dplyr::matches("^name$|gender")) %>% 
      dplyr::filter(!is.na(name), !is.na(gender)) %>%
      dplyr::distinct()
    print(dfname <- "fullnameg")
  }
  if(nameraceonly) {
    df %<>% 
      dplyr::select(dplyr::matches("name|race")) %>% 
      dplyr::filter(!is.na(name) | !is.na(firstname) | !is.na(lastname), !is.na(race) ) %>%
      dplyr::distinct()
    print(dfname <- "nr")
  }
  if(recode_extra_na) df %<>% recode_races_and_genders(., extra = "NA")
  if(!is.null(subsets)) by <- round(nrow(df) / subsets)
  if(by < 1) by <- 1000000
  if(by > nrow(df)) by <- nrow(df)
  lapply(seq(1, (nrow(df)), by), function (x) {
    start <- x
    end <- x + (by - 1)
    diff <- end - nrow(df)
    end <- ifelse(diff <= 0, end, end - diff)
    time <- system.time(snippet <- df[start:end, ])
    filename <- cleanpath(paste0(dir_path, "/", filename_prefix, 
                                 "_", alnum(dfname), "_", 
                                 round4(start), "-", round4(end), ".f"))
    feather::write_feather(snippet, filename)
    print(paste0(dim(snippet), " -- ", filename))
    print(time)
    # data.frame(dir = dir_path, filename = filename)
  }
  )
}

split_originals <- function(filelist, by=1000, outpath="~/"){
  mylist <- read_rdas(filelist) %>% tryCatch_combine_compact()
  lapply(seq(1, (length(mylist)), by), function(x){
    start <- x
    end <- x + (by - 1)
    diff <- end - length(mylist)
    end <- ifelse(diff <= 0, end, end - diff)
    fname <-paste0(outpath, "csv_", start, "to", end, ".rda")
    data <- mylist[start:end]
    save(data, file=fname)
    paste0("length: ", length(data), " | ", fname)
  })
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
extract_vars <- function(df,
                         namegenderonly = F,
                         Ng = F,
                         Nr = F,
                         Nrg=F,
                         fullnamegenderonly = F,
                         nameraceonly = F,
                         firstnameonly = F,
                         firstnamegenderonly=F,
                         firstnameraceonly=F,
                         recode_extra_na = F) {
  if(Nrg) df %<>% dplyr::select(name=fLname, dplyr::matches("race|gender")) %>% filter(!is.na("name"), !is.na("race")|!is.na("gender")) %>% dplyr::distinct() 
  if(Ng) df %<>% dplyr::select(name=fLname, dplyr::matches("gender")) %>% na.omit() %>% dplyr::distinct() 
  if(Nr) df %<>% dplyr::select(name=fLname, dplyr::matches("race")) %>% na.omit() %>% dplyr::distinct() 
  
  if(firstnamegenderonly) {
    df %<>% 
      dplyr::select(dplyr::matches("firstname|gender")) %>% 
      na.omit() %>%
      dplyr::distinct()
  }
  if(firstnameraceonly) {
    df %<>% 
      dplyr::select(dplyr::matches("firstname|race")) %>% 
      na.omit() %>%
      dplyr::distinct()
  }
  if(namegenderonly) {
    df %<>% 
      dplyr::select(dplyr::matches("name|gender")) %>% 
      dplyr::filter(!is.na(name) | !is.na(firstname) | !is.na(lastname), !is.na(gender)) %>%
      dplyr::distinct()
  }
  if(fullnamegenderonly) {
    df %<>% 
      dplyr::select(dplyr::matches("^name$|gender")) %>% 
      dplyr::filter(!is.na(name), !is.na(gender)) %>%
      dplyr::distinct()
  }
  if(nameraceonly) {
    df %<>% 
      dplyr::select(dplyr::matches("name|race")) %>% 
      dplyr::filter(!is.na(name) | !is.na(firstname) | !is.na(lastname), !is.na(race) ) %>%
      dplyr::distinct()
  }
  if(recode_extra_na) df %<>% recode_races_and_genders(., extra = "NA")
  
  dplyr::distinct(df)
}







#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
alnum_period <- function(x) gsub("[^\\.|[:alnum:]]", "", x)

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
alnum <- function(x) gsub("[^[:alnum:]]", "", x)

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
cleanpath <- function(x){
  x %<>% 
    gsub("\\/\\/", "\\/",. ) %>% 
    gsub("__", "_",. ) %>%
    gsub("^\\_|_$|^_|_$|\\<_", "", .) %>%
    gsub("^\\_|\\_$|^_", "", .)
}


#--------------------------------------

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
regulars_namesplit <- function (x, extra = NULL, extent = "thorough"){
  if(is.data.frame(x) | !is.list(x)) x %<>% list()
  trycomb <- try_combine(x)
  trycomp <- try_compact(x)
  if(is.list(trycomp)) x <- try_compact(x)
  if(is.list(trycomb)) x <- try_combine(x)
  
  x %<>% drop_empty()
  
  fldf <- data.frame(firstname="samantha", lastname="rhoads", name="samantha rhoads", gender="female", stringsAsFactors = F)
  x %<>% lapply(., function(xx) bind_rows(data.frame(xx), fldf))
  
  # x %<>% drop_empty()
  
  x %<>% preprocess_data(., extent = "thorough") %>% dplyr::distinct()
  x %<>% dealwith_racegender_variable() %>% dplyr::distinct()
  
  x %<>% tryCatch(., error = function(e) dfincase)
  if(is.null(x) | length(x) == 0) x <- dfincase
  if(is.null(x$name)|is.null(x$gender)|is.null(x$race)) x %<>% dplyr::bind_rows(., dfincase) %>% filter(!is.na(race)|!is.na(gender))
  
  x %<>% gather_race_and_gender() %>% dplyr::distinct()
  x %<>% recode_races_and_genders() #extrarace = extra, extragender = extra) 
  x %<>% dplyr::distinct()
  
  x %<>% gather_join_first_last_namesplit(.) %>% dplyr::distinct()
  
  x %<>% recode_na() %>% dplyr::distinct() %>% clean_recode(., scrub = "once") %>% dplyr::distinct()
  #-------
  x %<>% dplyr::mutate(gender_r = race,
                       race_g = gender) %>% dplyr::distinct()
  
  x %<>% gather_race_and_gender(.) %>% dplyr::distinct()
  
  x %<>% dplyr::mutate(gender = recode_gender_specific(gender),#, extra = extra),
                       race = recode_race_specific(race))#, extra = extra)) 
  x %<>% dplyr::distinct()
  #-------
  x %<>%
    recode_races_and_genders() %>%
    recode_races_and_genders(., extrarace = extra, extragender = extra) %>%
    dplyr::select_if(not_all_na)  %>%
    dplyr::distinct() %>% 
    data.frame(., stringsAsFactors = F)
  
  x$name %<>% 
    # gsub("   |  ", " ", .) %>%
    # trimws(., which="both") %>%
    gsub("^,|,$|^ ,|, $|'$", "", .)
  
  x %<>% namesplit()
  
  names(x) %<>% 
    gsub("^fn_orig", "firstname_fn_orig", .) %>%
    gsub("^ln_orig", "firstname_ln_orig", .)
  
  x %<>% gather_first_last_name()
  # if(!is.null(x$firstname)) x$firstname <- ifelse(x$firstname == x$lastname, stringr::word(x$name, 1), x$firstname)
  x$firstname <- ifelse(x$firstname == x$lastname & grepl(" ",x$name) & !grepl(",",x$name), stringr::word(x$name, 1), x$firstname)
  x$firstname <- ifelse(x$firstname == x$lastname & grepl(",",x$name), stringr::word(x$name, -1), x$firstname)
  x$firstname <- ifelse(x$firstname == x$name & grepl(" ",x$name) & !grepl(",",x$name), stringr::word(x$name, 1), x$firstname)
  x$firstname <- ifelse(x$firstname==stringr::word(x$name, -1) & grepl(" ",x$name) & !grepl(",",x$name), stringr::word(x$name, 1), x$firstname)
  x$lastname <- ifelse(x$lastname == x$name & grepl(" ",x$name) & !grepl(",",x$name), stringr::word(x$name, -1), x$lastname)
  
  x$firstname <- na_if(x$firstname, "")
  x$lastname <- na_if(x$lastname, "")
  
  x$firstname <- stringr::word(x$firstname, -1, sep=",") %>% trimws()
  
  
  x %<>% mutate(fLname = ifelse(!is.na(firstname) & !is.na(lastname), paste0(firstname, " ", toupper(lastname)), 
                                ifelse(!is.na(firstname) & is.na(lastname), firstname, 
                                       ifelse(is.na(firstname) & !is.na(lastname), toupper(lastname), NA))))
  dplyr::distinct(x) %>% 
    dplyr::filter(!is.na(race)|!is.na(gender), !is.na(name)|!is.na(firstname)|!is.na(lastname))
}



#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
read_extract_merge <- function(filelist = NULL, inpath = NULL, pattern = NULL,
                               Ng=F, Nr=F, Nrg=F) {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>% gsub("__", "_", .)
  }
  lapply(
    filelist, function (xx) feather::read_feather(xx) %>% extract_vars(., Ng=Ng, Nr=Nr, Nrg=Nrg)
  ) %>% bind_rows() %>% dplyr::distinct()
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
write_namesplit <- function(filelist = NULL,
                            inpath = NULL, pattern = NULL,
                            newdir = NULL,
                            extension="feather",
                            subsets = 2, subsubsets = 2,
                            extra = NULL,
                            outpath = "~/", filename_prefix = "") {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>% gsub("__", "_", .)
  }
  if(is.null(newdir)) dir.create(dir_path <- paste0(outpath, "/", filename_prefix, "_dump") %>%gsub("\\/\\/", "\\/", .) %>%gsub("__", "_", .))
  if(!is.null(newdir)) dir.create(dir_path <- paste0(outpath, "/", newdir) %>% gsub("\\/\\/", "\\/", .) %>% gsub("__", "_", .))
  lapply(filelist, function (xx) {
    mylist <- get(load(xx))
    if(!is.list(mylist) | is.data.frame(mylist) | tibble::is_tibble(mylist)) mylist %<>% list()
    mylist <- tryCatch(plyr::compact(mylist), error = function(e) mylist)      
    mylist <- tryCatch(dplyr::combine(mylist), error = function(e) mylist)  
    by <- round(length(mylist) / subsets)
    if(by < 1) by <- 1
    if(by > length(mylist)) by <- length(mylist)
    lapply(seq(1, (length(mylist)), by), function(x){
      start <- x
      end <- x + (by - 1)
      diff <- end - length(mylist)
      end <- ifelse(diff <= 0, end, end - diff)
      time <- system.time(snippet <- regulars_namesplit(mylist[start:end], extra = extra))
      filename <- paste0(dir_path, "/", filename_prefix, "_", 
                         gsub("-", "to", xx) %>% gsub("[^[:alnum:]]", "", .), "_", 
                         round4(start), "to", round4(end)) %>% 
        gsub("\\/\\/", "\\/",. ) %>% gsub("__", "_",. ) %>%
        gsub("^\\_|_$|^_|_$|\\<_", "", .) %>% gsub("^\\_|\\_$|^_", "", .)
      if(extension=="feather") feather::write_feather(snippet, paste0(filename, ".f"))
      if(extension=="csv") write.csv(snippet, paste0(filename, ".csv"))
      if(extension=="both"){ 
        feather::write_feather(snippet, paste0(filename, ".f"))
        write.csv(snippet, paste0(filename, ".csv"))
      }
      print("")
      print(paste0(paste0(dim(snippet), collapse=" row "), " col - ", filename))
      # print(filename)
      # print(paste0("dim: ", dim(snippet)))
      print(time)
      # data.frame(dir = dir_path, filename = filename)
    }
    )
  }
  )
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
round5 <- function(num) formatC(num, width = 5, format = "d", flag = "0")

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
round4 <- function(num) formatC(num, width = 4, format = "d", flag = "0")


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
lastname_wmiddle <- function(v){
  v %<>% gsub( ", ", ",", .) %>% 
    gsub( ",.*$", "", .) %>% 
    gsub( "^\\S* ", "", .)
  v
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
firstname_wmiddle <- function(v){
  v %<>% gsub( ", ", ",", .) %>% 
    gsub( "^.*,", "", .) %>% 
    gsub( " \\S*$", "", .)
  v
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
lastname_after_space_when_nocomma <- function(v) ifelse(grepl(" ",v) & !grepl(",",v), stringr::word(v,-1), NA)

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
firstname_nomiddle <- function(v){
  v %<>% 
    firstname_wmiddle() %>%
    trimws(., which="both") %>% 
    stringr::word(., 1)
  v
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
firstname_postcomma <- function(x){
  x <- strsplit(x, ",") %>% 
    lapply(., function(v){
      v <- v[-1]
      v <- ifelse(is.null(v), NA, v)
    }) %>% 
    unlist() %>%
    trimws(., which="both")
  x
}


#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
lastname_precomma <- function(x){
  x <- ifelse(grepl(",", x), x, NA)
  x <- strsplit(x, ",") %>% 
    lapply(., function(v){
      v <- v[1]
      v <- ifelse(is.null(v), NA, v)
    }) %>% 
    unlist() %>%
    trimws(., which="both")
  x
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
namesplit <- function(df, dfincase){
  df %<>% setNames(tolower(names(.)))
  df <- dplyr::bind_rows(dfincase, df) %>% dplyr::distinct()
  df %<>% dplyr::mutate(firstname_wmiddle=firstname_wmiddle(name),
                        firstname_nomiddle=firstname_nomiddle(name),
                        firstname_postcomma=firstname_postcomma(name),
                        lastname_wmiddle=lastname_wmiddle(name),
                        lastname_precomma=lastname_precomma(name),
                        lastname_after_space_when_nocomma=lastname_after_space_when_nocomma(name)
  )
  df
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_namesplit <- function(df){
  df %<>% 
    namesplit() %>%
    gather_first_last_name() %>%
    dplyr::distinct()
  df
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
gather_namesplit_clean <- function(df){
  df %<>% lapply(., function(x){
    x <- stringi::stri_enc_toutf8(x) %>%
      gsub("_", " ", .) %>%
      gsub("[^,|\\.| |\\-|-|[:alpha:]]", "", .) %>%
      trimws(., which="both") %>% 
      as.character() %>% 
      tolower() %>% 
      gsub(",", ", ", .) %>%
      gsub("  ", " ", .) %>%
      gsub("^jr$|^jr\\.$|jr\\.,| jr\\.| jr$| jr |,jr$|,jr\\.|,jr |^jr\\. ", "junior", .) %>%
      trimws() 
    x
  }) %>% data.frame(., stringsAsFactors = F) %>%
    clean_recode() %>%
    gather_namesplit() %>%
    recode_races_and_genders() %>% 
    dplyr::mutate(gender=as.factor(recode_gender_specific(gender))) %>%
    dplyr::distinct() %>%
    lapply(., function (x) {
      x %<>% 
        gsub("^phd$|^rn$|^md$", "", .) %>%
        gsub("^jr$|^jr\\.$|jr\\.,| jr\\.| jr$| jr |,jr$|,jr\\.|,jr |^jr\\. ", "junior", .) %>%
        na_if(., "") %>%
        as.factor()
      x
    }) %>%
    data.frame() %>% 
    dplyr::distinct()
  df
}

#--------------------------------------------------------------
#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
samtokens <- function(df, nmin=2,nmax=3,
                      xv='firstname',
                      yv='gender',
                      sample=nrow(df)){
  df$x <- df[[xv]]
  df$y <- df[[yv]]
  df %<>% 
    sample_n(., sample) %>% 
    select(x, y) %>%  
    na.omit() %>% 
    dplyr::distinct() %>%
    dplyr::mutate(id=paste0("doc", 1:nrow(.)),
                  y=as.factor(as.numeric(as.factor(y))),
                  x = x %>%
                    trimws(., which="both") %>%
                    paste0(" ", ., " ")
    ) %>% dplyr::distinct() 
  df$x %<>%
    quanteda::tokens(., ngrams=nmin:nmax,
                     what="character",
                     remove_separators=F,
                     concatenator = "") %>%
    lapply(., function(x) {
      x <- gsub(" ", "_", x)
      x <- paste0(x, collapse=" ")
    }) %>% data.frame(., stringsAsFactors = F) %>% t() %>%
    .[,1] %>% as.character()%>% unlist()
  return(df)
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
dtmfunc <- function(df=d, corp=df$x, id=df$id, term_count_min=2){
  it = text2vec::itoken(as.character(corp),  
                        tokenizer = text2vec::word_tokenizer, 
                        ids = id)
  vocab <- text2vec::create_vocabulary(it) %>%
    text2vec::prune_vocabulary(., term_count_min=term_count_min)
  vectorizer = text2vec::vocab_vectorizer(vocab)
  dtm_all = text2vec::create_dtm(it, vectorizer)
  dtm_all
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
tokdtmfunc <- function(df=fgsam, xv='firstname', yv='gender', nmin=2,nmax=3,term_count_min=2){
  d <- samtokens(df=df, xv=xv, yv=yv, nmin=nmin, nmax=nmax)
  dtm_all <- dtmfunc(df=d, corp=d$x, id=d$id, term_count_min=term_count_min)
  list("dtm"=dtm_all, "df"=d)
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' cvglmnet()
cvglmnet <- function(x = dtm_all, y = d$y, 
                     family = 'binomial', alpha = 0,
                     type.measure = "auc", nfolds = 5,
                     thresh = 1e-3, maxit = 1e3){
  m <- glmnet::cv.glmnet(x=x, y=y, 
                         family=family, alpha=alpha, 
                         type.measure=type.measure,
                         nfolds=nfolds, thresh=thresh, 
                         maxit=maxit)
  # print(paste("max AUC =", round(max(m$cvm), 4)))
  print(paste("mean AUC =", round(mean(m$cvm), 4)))
  print("     ")
  return(list(m, plot(m)))
  # print(plot(m))
}

#' A Function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
tok_cvglmnet <- function(df=fgsam, xv='firstname', yv='gender', nmin=2,nmax=3,term_count_min=2,
                         family = 'binomial', alpha = 0,
                         type.measure = "auc", nfolds = 5,
                         thresh = 1e-3, maxit = 1e3){
  xy <- tokdtmfunc(df=df, xv=xv, yv=yv, nmin=nmin,nmax=nmax,term_count_min=term_count_min)
  mod <- cvglmnet(x = xy$dtm, y = xy$df$y, 
                  family = family, alpha = alpha,
                  type.measure = type.measure, nfolds = nfolds,
                  thresh = thresh, maxit = maxit)
  
}








print("yey u loaded sam's fxns!")


# 
