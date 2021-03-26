
recode_na_getridofstrregex <- {paste0(
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
    "choosenotto", "choosenot", "chieffinancialofficer", "centralretail", "notresponse",
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
  collapse = "|")}

recode_na_list <- {list(
  "NA" = c(" ", ",", "active", "admin", "agency", "aliennonresident", 
           "american", "anonymized", "answe", "answer1", "answeredhispaniclatinoquestion", 
           "arbpayroll", "blackafricanamerican", "brazilmonthly", "candidateeeo", 
           "centralislipny", "choosenotrodisclose", "choosenottodisclose", 
           "choosenottoidentify", "chosenottodisclose", "chosenottoselfidentify", 
           "claimingdisabilitystat", "clericalunitclerk", "cocreatepayroll", 
           "coders", "comalepanyjobboard", "dateofinterview", "declin", "notnoresponse",
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
           "unknownpersonwasmanuallyentered", "unknwn", "unkown", "unkwn", "", 
           "unotknow", "unspec", "unspecified", "usbiweekly", "xx")
)}


types <- c('ADDL COMP', 'APPLICANTS', 'NEW HIRES', 'PROMOTIONS', 'TERMINATIONS', 'WORKFORCE')

#============================================================================================================================================================================

##### OLD VERSION:
# _______grep_all_df <- function(pattern, df, colnames=F, exact=F, ignore.case=F, print=F){ 
#   if(colnames) grep_all_df_colnames(pattern, df, exact=exact, ignore.case=ignore.case, print=print) 
#   else grep_all_df_df(pattern, df, exact=exact, ignore.case=ignore.case, print=print)
# }

#' Samantha Rhoads's function to check if all of something is NA or NULL
#'
#' Srhoads wrote this to allow you to check if all of a variable is NA or NULL (old version from 20191210)
#' @export
#' @examples
#' is.nanull_V1()
is.nanull_V1 <- function(x){
  all(is.na(x)) | is.null(x)
}


#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' pkg()
#' ## load and/or install package first!
#' pkg <- function (package1, ...) {
#'   packages <- c(package1, ...)
#'   for (package in packages) {
#'     if (package %in% rownames(installed.packages())) 
#'       do.call(library, list(package))
#'     else {
#'       install.packages(package, 
#'                        repos = c("https://cloud.r-project.org", 
#'                                  "http://owi.usgs.gov/R/"), dependencies = NA, 
#'                        type = getOption("pkgType"))
#'       do.call(library, list(package))
#'     }
#'   }
#' }

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' pkgu()



## load and/or install package first, plus check for an update & do the update if there is one!
pkgu <- function (package1, ...) {
  packages <- c(package1, ...)
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      do.call(library, list(package))
      update.packages(package)
    }
    else {
      install.packages(package, 
                       repos = c("https://cloud.r-project.org", 
                                 "http://owi.usgs.gov/R/"), dependencies = NA, 
                       type = getOption("pkgType"))
      do.call(library, list(package))
      update.packages(package)
    }
  }
}

#' Samantha Rhoads's function to...
#'
#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' extract_dateV1(v)
extract_dateV1 <- function(v) {
  c(
    datepat0=' ?(0|1)?([0-9]{4}|[0-9]{1,2})-([0-9]{1,2})-([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]-([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]/([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})\\.([0-9]{1,2})\\.([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]\\.([0-9]{1,2}|[0-9]{4}) ?',
    datepat14=' ?(0|1)?([0-9]{4}|[0-9]{1,2})([0-9]{1,2})([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})([0-9]{1,2})([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})([0-9]{1,2})([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]([0-9]{1,2}|[0-9]{4}) ?',
    
    datepat1=' ?(0|1)?([1-9]{1,2}|[1-9]{4})/([0-9]{1,2})/([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?[1-9]/([0-9]{1,2}|[0-9]{4}) ?',
    datepat2=' ?(0|1)?([1-9]{1,2}|[1-9]{4})-([0-9]{1,2})-([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?[1-9]-([0-9]{1,2}|[0-9]{4}) ?',
    datepat3=' ?(0|1)?([1-9]{1,2}|[1-9]{4})\\.([0-9]{1,2})\\.([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?[1-9]\\.([0-9]{1,2}|[0-9]{4}) ?',
    datepat4=' ?(0|1)?[1-9]/([0-9]{1}|[0-9]{2})/([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]/([0-9]{2}|[0-9]{4}) ?| ?(0|1)?[1-9]/([0-9]{4}|[0-9]{1,2}) ?',
    datepat5=' ?(0|1)?[1-9]-([0-9]{1}|[0-9]{2})-([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]-([0-9]{2}|[0-9]{4}) ?| ?(0|1)?[1-9]-([0-9]{4}|[0-9]{1,2}) ?',
    datepat6=' ?(0|1)?[1-9]\\.([0-9]{1}|[0-9]{2})\\.([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]\\.([0-9]{2}|[0-9]{4}) ?| ?(0|1)?[1-9]\\.([0-9]{4}|[0-9]{1,2}) ?',
    
    datepat7=' ?(0|1)?([1-9]{4}|[0-9]{1,2})/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]/([0-9]{1,2}|[0-9]{4}) ?',
    datepat8=' ?(0|1)?([1-9]{4}/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]/([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([1-9]{1,2}/([0-9]{1,2})/([0-9]{4}|[0-9]{2,4}) ?',
    datepat9=' ?(0|1)?([1-9]{4}|[0-9]{1,2})/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]/([0-9]{1,2}|[0-9]{4}) ?',
    datepat10=' ?(0|1)?([1-9]{4}|[1-9]{1,2})\\.([0-9]{1,2})\\.([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]\\.([0-9]{1,2}|[0-9]{4}) ?',
    datepat11=' ?(0|1)?[1-9]/([0-9]{4}|[0-9]{1,2})/([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]/([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]/([0-9]{4}|[0-9]{1,2}) ?',
    datepat12=' ?(0|1)?[1-9]-([0-9]{4}|[0-9]{1,2})-([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]-([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]-([0-9]{4}|[0-9]{1,2}) ?',
    datepat13=' ?(0|1)?[1-9]\\.([0-9]{4}|[0-9]{1,2})\\.([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]\\.([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]\\.([0-9]{4}|[0-9]{1,2}) ?'
  )
  # # v %>% stringr::str_extract_all(., paste0('datepath', 1:12))
  # (patternstr <- paste0('datepat', c(0:7, 9:14)) %>% sapply(get) %>% unlist() %>% paste0(., collapse="|"))
  # v %>% stringr::str_extract_all(., patternstr)
  (patternstr <- datepats %>% unlist() %>% paste0(., collapse="|"))
  v %>% stringr::str_extract_all(., patternstr)
}



#' A function
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
    (sublist <- dplyr::bind_rows(sublist) %>% dplyr::distinct())
    feather::write_feather(sublist, filename)
    print("")
    print(paste0("dim: ", paste0(dim(sublist), collapse=" row "), " col - ", filename))
    print(sample_n(filter(sublist, !is.na(fLname)), 3))
  })# %>% print(system.time())
}


# readexcel <- function(file, bindsheets=F){
#   sheets <- readxl::excel_sheets(file)
#   d <- lapply(sheets, function(sheet) readxl::read_excel(file, sheet))
#   names(d) <- sheets
#   d <- try_combine_compact(d)
#   d <- drop_empty(d)
#   if(bindsheets) d <- dplyr::bind_rows(d)
#   d
# }


#' A function
#' @export
#' @examples
#' read_ydrive_clean_write()
read_ydrive_clean_write <- read_excel_allsheets <- function(filenames, csv=F, xlsx=F, xls=F, outpath="data/original/", col_types='text') {
  if(xls|xlsx){
    filenames <- readxl::excel_sheets(filenames)
    lapply(filenames, function(f) {
      print(filename <- paste0(outpath, gsub_NSRHOADS(f), ".rda"))
      d <- tryCatch(readxl::read_excel(filenames, sheet = f, col_types=col_types), error=function(e) NULL)
      if(is.list(d)) d %<>% try_combine_compact() %>% dplyr::bind_rows()
      d <- regulars_namesplit(d)
      feather::write_feather(d, filename)
    })
  }
  if(csv) {
    lapply(filenames, function(f){ 
      print(filename <- paste0(outpath, gsub_NSRHOADS(f), ".rda"))
      d <- tryCatch(read.csv(f), error=function(e) NULL)
      if(is.list(d)) d %<>% try_combine_compact() %>% dplyr::bind_rows()
      d <- regulars_namesplit(d)
      feather::write_feather(d, filename)
    })
  }
}

#' A function
#' @export
#' @examples
#' write_ydrive_originals()
write_ydrive_originals <- function(fl, outpath="AA/data/"){ # input = list of file names
  read_ydrive_write(csvfl <- fl[grep("csv$", fl, ignore.case=T)], csv=T)
  read_ydrive_write(xlsfl <- fl[grep("xls$", fl, ignore.case=T)], xls=T)
  read_ydrive_write(xlsxfl <- fl[grep("xlsx$", fl, ignore.case=T)], xlsx=T)
}

#============================================================================================================================================================================


#' A function
#' @export
#' @examples
#' select_not_race_gender_cols()
select_not_race_gender_cols <- function(mylist) {
  nrg <- purrr::map(mylist,
                    ~dplyr::select(.x, #dplyr::matches('name|x'), dplyr::contains(dplyr::everything()),
                                   -dplyr::matches('gender|race|date|time')
                    ))
  nrg
}

#' A function works on both dataframes and LODs (lists of dataframes)
#'
#' This function allows you to 
#' @export
#' @examples
#' select_nrg_cols()
select_nrg_cols <- select_nrg_cols_list <- select_nrg_cols_df <- function(mylist, output = c("default", "list", "names"), minuscontainsregex="") {
  output <- match.arg(output)
  if(is.data.frame(mylist)){
    if(output=="default"){output <- "df"}
    mylist <- list(df = mylist)
  } else if(output=="default"){
    output <- "list"
  }
  if(is.null(minuscontainsregex)){minuscontainsregex <- ''}
  if(nchar(minuscontainsregex) %in% c(0, NA)){minuscontainsregex <- 'nothingatalllolpleaseleavethisaloneoknowifeellikethisstringoflettersandcharsisntgonnaappear_ew8ruijfkdsmxemfue98fmhjaksf293490rehui'}
  mylist <- purrr::map(mylist, ~dplyr::select(.x, 
                                              dplyr::matches('name|gender|gendr|gndr|gnder|sex|race|ethnic'),
                                              -dplyr::matches(minuscontainsregex)))
  if(output == "names"){
    return(purrr::map(mylist, ~names(.x)) %>% unlist() %>% unique())
  } else if(output == "list"){
    return(mylist)
  } else if(output == "df"){
    return(mylist[[1]])
  } else {
    return(mylist )
  }
}


#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' select_name_cols_df()
select_name_cols_df <- function(df, output = c("df", "names"), minuscontainsregex="") {
  output <- match.arg(output)
  if(is.null(minuscontainsregex)){minuscontainsregex <- ''}
  if(nchar(minuscontainsregex) %in% c(0, NA)){minuscontainsregex <- 'nothingatalllolpleaseleavethisaloneoknowifeellikethisstringoflettersandcharsisntgonnaappear_ew8ruijfkdsmxemfue98fmhjaksf293490rehui'}
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


# select_gender_cols_df <- function(df, output = c("df", "names", "dfnewnames")) {
#   output <- match.arg(output)
#   exacts <- c("female", "male", "fem", "man", "woman", "men", "women", "girl", "boy", "feminine", "masculine")
#   partials <- c("female", "woman", "women", "feminine", "masculine")
#   cols1 <- 
#     dplyr::select(df, dplyr::matches("gender|sex|female|male|gndr|gendr|male|femini|woman|women|masculi")) %>% names()
#   cols2 <- 
#     dplyr::select_if(df, function(x) {any(x %in% exacts) | any(grepl(paste0(partials, collapse="|"), x, ignore.case = T))}) %>% names()
#   cols <- c(cols1, cols2) %>% unique()
#   df <- 
#     dplyr::select(df, dplyr::matches(paste0(cols, collapse="|"))) #%>% dplyr::distinct()
#   
#   if(length(df) == 0 | ncol(df) == 0){
#     df <- data.frame(name = c("jenny", "bob"), gender = c("female", "male"), race = c("asian", "white"), stringsAsFactors = F)
#   }
#   
#   if(output == "names")
#     return(names(df))
#   if(output == "df")
#     return(df)
#   if(output == "dfnewnames") {
#     names(df) <- paste0("gender.", 1:ncol(df))
#     df
#   }
#   df
# }


# select_gender_cols_nontrad_df <- function(df, output = c("df", "names", "dfnewnames")) {
#   output <- match.arg(output)
#   exacts <- c("female", "male", "fem", "man", "woman", "men", "women", "girl", "boy", "feminine", "masculine")
#   partials <- c("female", "woman", "women", "feminine", "masculine")
#   cols1 <-
#     dplyr::select(df, dplyr::matches("gender|sex|female|male|gndr|gendr|male|femini|woman|women|masculi")) %>% names()
#   cols2 <-
#     dplyr::select_if(df, function(x) {any(x %in% exacts) | any(grepl(paste0(partials, collapse="|"), x, ignore.case = T))}) %>% names()
#   cols <- c(cols1, cols2, "PLACEFILLER") %>% unique()
#   # df$PLACEFILLER <- NA
#   df <-
#     dplyr::select(df, dplyr::matches(paste0(cols, collapse="|"))) %>% dplyr::select(- dplyr::matches("gender"))
#   
#   if(length(df) == 0 | ncol(df) == 0){
#     df <- data.frame(name = c("jenny", "bob"), gender = c("female", "male"), race = c("asian", "white"), stringsAsFactors = F)
#   }
#   
#   if(output == "names")
#     return(names(df))
#   if(output == "df")
#     return(df)
#   if(output == "dfnewnames") {
#     names(df) <- paste0("gender.", 1:ncol(df))
#     df
#   }
#   df
# }



#' A function
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
    purrr::map(mylist, ~dplyr::select(.x, dplyr::matches("gender|sex|female|male|gndr|gendr|male|femini|woman|women|masculi"))) %>%
    purrr::map(., ~names(.x)) %>% unlist() %>% unique()
  cols2 <- 
    purrr::map(mylist, 
               ~dplyr::select_if(.x, function(xx) {any(xx %in% exacts) | any(grepl(paste0(partials, collapse="|"), xx, ignore.case = T))})) %>% 
    purrr::map(., ~names(.x)) %>% unlist() %>% unique()
  cols <- c(cols1, cols2, "PLACEFILLER") %>% unique()
  mylist <- purrr::map(mylist, ~dplyr::select(.x, dplyr::matches(paste0(cols, collapse="|"))))
  if(output == "names")
    return(purrr::map(mylist, ~names(.x)) %>% unlist() %>% unique())
  if(output == "list")
    return(mylist)
  mylist
} 


#' A function
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


# 
# select_race_cols_nontrad_df <- function(df, output = c("df", "names", "dfnewnames")) {
#   output <- match.arg(output)
#   exacts <- c("white", "black", "black or african american", "blackorafricanamerican", "hispanic or latino", "hispanicorlatino", "asian",
#               "americanindianoralaskanative", "american indian or alaska native", "nativehawaiianorotherpacificislander",
#               "native hawaiian or other pacific islander", "twoormoreraces", "two or more races")
#   partials <- c("african", "hispanic", "americanindian", "american indian", "hawaiian", "pacificislander", "hispanic",
#                 "pacific islander", "indian")
#   cols1 <-
#     dplyr::select(df, dplyr::matches("race|ethnicity|ethnicit|ethnici|ethnic|ethni|ethno|ethn|rce|racial")) %>% names()
#   cols2 <-
#     dplyr::select_if(df, function(x) {any(x %in% exacts) | any(grepl(paste0(partials, collapse="|"), x, ignore.case = T))}) %>% names()
#   cols <- c(cols1, cols2, "PLACEFILLER") %>% unique()
#   # df$PLACEFILLER <- NA
#   df <-
#     dplyr::select(df, dplyr::matches(paste0(cols, collapse="|")),-dplyr::matches("name")) %>% dplyr::select(- dplyr::matches("race"))
#   
#   if(length(df) == 0 | ncol(df) == 0){
#     df <- data.frame(name = c("jenny", "bob"), gender = c("female", "male"), race = c("asian", "white"), stringsAsFactors = F)
#   }
#   
#   if(output == "names")
#     return(names(df))
#   if(output == "df")
#     return(df)
#   if(output == "dfnewnames") {
#     names(df) <- paste0("race.", 1:ncol(df))
#     df
#   }
#   df
# }




#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' select_race_cols_list()
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

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_name_race_gender_cols_df <- function(df, output = c("df", "names")){
  output <- match.arg(output)
  cnrg <- select_nrg_cols(df, output = "names")
  cr <- select_nrg_cols(df, output = "names")
  cg <- select_nrg_cols(df, output = "names")
  cols <- c(cnrg, cr, cg) %>% unique()
  df <- dplyr::select(df, dplyr::matches(paste0(cols, collapse="|"))) %>% dplyr::distinct()
  if(output == "names")
    return(names(df))
  if(output == "df")
    return(df)
  df
}

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' ()
select_name_race_gender_cols_list <- function(mylist, output = c("list", "names")){
  output <- match.arg(output)
  cnrg <- select_nrg_cols(mylist, output = "names")
  cr <- select_nrg_cols(mylist, output = "names")
  cg <- select_nrg_cols(mylist, output = "names")
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

#' A function
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

#' A function
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

#' A function
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

#' A function
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

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_names_race()
preprocess_names_race <- function(x) {
  names(x) %<>% 
    gsub(turntoracestrregex, "race", ., perl = T) %>% 
    gsub("raceracerace|racerace|racename|namerace", "race", ., perl = T)
  names(x)
}
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_names_name()
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

#' A function
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

#' A function
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
  
  
  names(x) <- make.names(names = names(x), unique=T)
  names(x) <- as.character(names(x))
  if (is.list(x) & ! is.data.frame(x)) 
    x <- lapply(X = x, FUN = preprocess_names_thorough)
  x
}

#' A function
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
  names(x) <- make.names(names = names(x), unique=T)
  names(x) <- as.character(names(x))
  if (is.list(x) & ! is.data.frame(x)) x <- lapply(X = x, FUN = preprocess_names_minimal)
  x
}
#' A function
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

#' A function
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

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_data_minimal()
preprocess_data_minimal <- function(x, type = c("lod")) {
  type = match.arg(type)
  x <- tryCatch(dplyr::combine(x), 
                error = function(e) x)
  if(type == "lod")
    return(preprocess_names(x, extent = "minimal") %>% 
             select_nrg_cols(.) %>% 
             list_to_df(.) %>% dplyr::distinct())
  preprocess_names_minimal(x#, extent = "minimal"
  ) %>% 
    select_nrg_cols(.) %>% 
    list_to_df(.) %>% dplyr::distinct()
}

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' preprocess_data_thorough()
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

#' A function
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


#' A read files into list of list function
#'
#' This function allows you to read a list of files into a list of lists of data if the data is in excel (xlsx or xls format)
#' @export
#' @examples
#' data_lol(path='data', skip1='APPLICANTS_FAC-STAFF')
data_lol <- function(path='data', skip1='APPLICANTS_FAC-STAFF', col_types='text'){
  dirs <- list.dirs(path, recursive=T) %>% .[-grep(paste0(path, "$"), .)]
  data <- 
    lapply(dirs, function(l){
      files <- list.files(l, recursive=T, full.names=T)
      l %<>%
        list.files(., recursive=T, full.names=T) %>%
        read_excels(., bindsheets = T, col_types=col_types)
      names(l) <- basename(files)
      l
    }) %>% setNames(., basename(dirs))
  if(!is.null(skip1)){
    s1dfname <- select_list(data$data_files, skip1) %>% names()
    s1df <- list.files(path=path, pattern=skip1, recursive=T, full.names = T) %>% readexcel(., bindsheets=T, skip=1, col_types=col_types)
    data$data_files[[s1dfname]] <- s1df
  }
  data
}
# data_lol <- function(pattern='data'){
#   dirs <- list.dirs(pattern, recursive=T) %>% .[-grep(paste0(pattern, "$"), .)]
#   data <- lapply(dirs, function(l){
#     files <- list.files(l, recursive=T, full.names=T)
#     l %<>% 
#       list.files(., recursive=T, full.names=T) %>% 
#       read_excels(., bindsheets = T)
#     names(l) <- basename(files)
#     l
#   }) %>% setNames(., basename(dirs))
# }










#============================================================================================================================================================================



#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' read_rdas()
#' read_rdas <- function(filelist = NULL, 
#'                       inpath = NULL,
#'                       pattern = NULL) {
#'   if(is.null(filelist)){
#'     files <- list.files(inpath, pattern)
#'     filelist <- paste0(inpath, "/", files) %>%
#'       gsub("\\/\\/", "\\/", ., perl = T) %>%
#'       gsub("__", "_", ., perl = T)
#'   }
#'   
#'   loaded <- lapply(filelist, function (x){
#'     x <- load(x)
#'     x <- get(x)
#'   })
#'   loaded
#' }
#' 
#' 
#' 
#' A function
#' @export
#' @examples
#' read_merge_write_feathers()
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




#' A function
#' @export
#' @examples
#' read_feathers_recode_write()
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



#' A function
#' @export
#' @examples
#' ()
regulars_anomalies <- function(x, extra = NULL, extent = "thorough"){
  reg <- data.frame(regulars(x, extra = extra), stringsAsFactors = F)
  anom <- data.frame(anomalies(x), stringsAsFactors = F)
  all <- dplyr::bind_rows(reg, anom) %>% dplyr::distinct()
}

#' A function
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








#' A function
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
      dplyr::mutate(lastname = toupper(lastname),
                    gender = as.factor(gender),
                    name = paste0(firstname, " ", lastname)) %>%
      select(name, gender) %>% dplyr::distinct()
    print(dfname <- "Ng")
  }
  
  if(Nr) {
    df %<>% 
      dplyr::select(dplyr::matches("name|race")) %>% 
      na.omit() %>%
      dplyr::mutate(lastname = toupper(lastname),
                    race = as.factor(race),
                    name = paste0(firstname, " ", lastname)) %>%
      select(name, race) %>% dplyr::distinct()
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

#' A function
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



#' A function
#' @export
#' @examples
#' recode_list()
recode_list <- function(gender=T, race=T, na=F){
  recodelist <- list("NA" = c("N/A"))
  if(gender) recodelist <- combine.lists(gender_list_short, recodelist)
  if(race) recodelist <- c(recodelist, race_list_short)
  recodelist
}



#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' regulars_namesplit()
regulars_namesplit <- function (x, extra = NULL, extent = "thorough"){
  if(is.data.frame(x) | !is.list(x)) x %<>% list()
  trycomb <- try_combine(x)
  trycomp <- try_compact(x)
  if(is.list(trycomp)) x <- try_compact(x)
  if(is.list(trycomb)) x <- try_combine(x)
  
  x %<>% drop_empty()
  
  fldf <- data.frame(firstname="samantha", lastname="rhoads", name="samantha rhoads", gender="female", stringsAsFactors = F)
  x %<>% lapply(., function(xx) dplyr::bind_rows(data.frame(xx), fldf))
  
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
  
  
  x %<>% dplyr::mutate(fLname = ifelse(!is.na(firstname) & !is.na(lastname), paste0(firstname, " ", toupper(lastname)), 
                                       ifelse(!is.na(firstname) & is.na(lastname), firstname, 
                                              ifelse(is.na(firstname) & !is.na(lastname), toupper(lastname), NA))))
  dplyr::distinct(x) %>% 
    dplyr::filter(!is.na(race)|!is.na(gender), !is.na(name)|!is.na(firstname)|!is.na(lastname))
}



#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' read_extract_merge()
read_extract_merge <- function(filelist = NULL, inpath = NULL, pattern = NULL,
                               Ng=F, Nr=F, Nrg=F) {
  if(is.null(filelist)){
    files <- list.files(inpath, pattern)
    filelist <- paste0(inpath, "/", files) %>%
      gsub("\\/\\/", "\\/", .) %>% gsub("__", "_", .)
  }
  lapply(
    filelist, function (xx) feather::read_feather(xx) %>% extract_vars(., Ng=Ng, Nr=Nr, Nrg=Nrg)
  ) %>% dplyr::bind_rows() %>% dplyr::distinct()
}


#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' write_namesplit()
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






#' A function
#' @export
#' @examples
#' samtokens()
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
  
  tryCatch(if(!require("quanteda")) install.packages("quanteda"), error=function(e) print("Couldn't install/access `quanteda` package"))
  
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

#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' dtmfunc()
#' dtmfunc <- function(df=d, corp=df$x, id=df$id, term_count_min=2){
#'   tryCatch(if(!require("text2vec")) install.packages("text2vec"), error=function(e) print("Couldn't install/access `text2vec` package"))
#'   tryCatch(if(!require("devtools")) install.packages('devtools'), error=function(e) print("Couldn't install/access `devtools` package"))
#'   tryCatch(if(!require("text2vec")) devtools::install_github('dselivanov/text2vec'), error=function(e) print("Couldn't install/access `text2vec` package"))
#'   it = text2vec::itoken(as.character(corp),  
#'                         tokenizer = text2vec::word_tokenizer, 
#'                         ids = id)
#'   vocab <- text2vec::create_vocabulary(it) %>%
#'     text2vec::prune_vocabulary(., term_count_min=term_count_min)
#'   vectorizer = text2vec::vocab_vectorizer(vocab)
#'   dtm_all = text2vec::create_dtm(it, vectorizer)
#'   dtm_all
#' }

#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' tokdtmfunc()
#' tokdtmfunc <- function(df=fgsam, xv='firstname', yv='gender', nmin=2,nmax=3,term_count_min=2){
#'   d <- samtokens(df=df, xv=xv, yv=yv, nmin=nmin, nmax=nmax)
#'   dtm_all <- dtmfunc(df=d, corp=d$x, id=d$id, term_count_min=term_count_min)
#'   list("dtm"=dtm_all, "df"=d)
#' }

#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' cvglmnet()
#' cvglmnet <- function(x = dtm_all, y = d$y, 
#'                      family = 'binomial', alpha = 0,
#'                      type.measure = "auc", nfolds = 5,
#'                      thresh = 1e-3, maxit = 1e3){
#'   
#'   tryCatch(if(!require("glmnet")) install.packages("glmnet"), error=function(e) print("Couldn't install/access `glmnet` package"))
#'   
#'   m <- glmnet::cv.glmnet(x=x, y=y, 
#'                          family=family, alpha=alpha, 
#'                          type.measure=type.measure,
#'                          nfolds=nfolds, thresh=thresh, 
#'                          maxit=maxit)
#'   # print(paste("max AUC =", round(max(m$cvm), 4)))
#'   print(paste("mean AUC =", round(mean(m$cvm), 4)))
#'   print("     ")
#'   return(list(m, plot(m)))
#'   # print(plot(m))
#' }

#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' tok_cvglmnet()
#' tok_cvglmnet <- function(df=fgsam, xv='firstname', yv='gender', nmin=2,nmax=3,term_count_min=2,
#'                          family = 'binomial', alpha = 0,
#'                          type.measure = "auc", nfolds = 5,
#'                          thresh = 1e-3, maxit = 1e3){
#'   xy <- tokdtmfunc(df=df, xv=xv, yv=yv, nmin=nmin,nmax=nmax,term_count_min=term_count_min)
#'   mod <- cvglmnet(x = xy$dtm, y = xy$df$y, 
#'                   family = family, alpha = alpha,
#'                   type.measure = type.measure, nfolds = nfolds,
#'                   thresh = thresh, maxit = maxit)
#'   
#' }





#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' df_paste_()
#' df_paste_ <- function(df){
#'   vector_paste((substitute(df)))
#'   df_paste(df)
#' }
#' 
#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' df_paste_2()
#' df_paste_2 <- function(df){
#'   vector_paste(c("'____'", substitute(df), "'____'"))
#'   df_paste(df)
#' }
#' 
#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' df_paste_lod()
#' df_paste_lod <- function(lod){
#'   lapply(seq_along(lod), 
#'          function(y, n, i) {vector_paste(n[[i]]); df_paste(y[[i]])}, 
#'          y=lod, n=names(lod))
#' }
#' 


#' #' A function
#' #'
#' #' This function allows you to 
#' #' @export
#' #' @examples
#' #' parse_excel_date()
#' parse_excel_date <- function(v){
#'   # v %>% janitor::excel_numeric_to_date(as.numeric(as.character(v)), date_system = "modern")
#'   v %>% as.character() %>% as.numeric() %>% as.Date(., origin = "1899-12-30")
#' }



# if(EVALME <- F){
#   "([0-9]{4}|[0-9]{1,2})" -> DATEREGEX
#   "([0-9]{4}|[0-9]{1,2})-([0-9]{1,2})-([0-9]{4}|[0-9]{1,2})" -> DATEREGEX -> DATEREGEX_DASH
#   "([0-9]{4}|[0-9]{1,2})/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2})" -> DATEREGEX -> DATEREGEX_SLASH
#   "([0-9]{4}|[0-9]{1,2})\\.([0-9]{1,2})\\.([0-9]{4}|[0-9]{1,2})" -> DATEREGEX -> DATEREGEX_DOT
#   "([0-9]{4}|[0-9]{1,2})([0-9]{1,2})([0-9]{4}|[0-9]{1,2})" -> DATEREGEX -> DATEREGEX_NOSEP
#   
#   
#   fourDigitYr <- fourDigitYr_1900sOr2000s <- "\b?(19|20)([0-9]{2})"
#   twoDigitMonth <- "(\b?(0|1)([0-9]{1}))"
#   twoDigitMonth <- "(\b?(0)([0-9]{1})|\b?(1)([0-2]{1}))"
#   twoDigitDay <- "(\b?(0)([0-9]{1})|\b?(1)([0-9]{1})|\b?(2)([0-9]{1})|30|31)"
#   
#   extract_eightDigitDate <- function(string, sep=c("-", "\\.", "/", "")){
#     fourDigitYr <- fourDigitYr_1900sOr2000s <- "\b?(19|20)([0-9]{2})"
#     twoDigitMonth <- "(\b?(0|1)([0-9]{1}))"
#     twoDigitMonth <- "(\b?(0)([0-9]{1})|\b?(1)([0-2]{1}))"
#     twoDigitDay <- "(\b?(0)([0-9]{1})|\b?(1)([0-9]{1})|\b?(2)([0-9]{1})|30|31)"
#     
#     lapply(sep, function(sepi){
#       REGEXPATS <- paste0(fourDigitYr, sepi, twoDigitMonth, sepi, twoDigitDay)
#       REGEXPATS2 <- paste0(twoDigitMonth, sepi, twoDigitDay, sepi, fourDigitYr)
#       REGEXPATS_1_2 <- paste0(paste0("(", REGEXPATS, ")"), "|", paste0("(", REGEXPATS2, ")"))
#       stringr::str_extract_all(string, REGEXPATS_1_2)
#     })
#     # lapply(sep, function(sepi){
#     #   REGEXPATS <- paste0(twoDigitMonth, sepi, twoDigitDay, sepi, fourDigitYr)
#     #   stringr::str_extract_all(string, REGEXPATS)
#     # })
#   } 
#   
#   
#   # " ?(19|20)([0-9]{2})"
#   
#   STR4 <- "20201130 11302020 012020 202001 2020-11-17 11-30-2020 01-2020 2020-01 2020/11/17 11/17/2020 01/2020 2020/01 2020.11.17 11.17.2020 01.2020 2020.01"
#   string <- STR4
#     
#   # "([0-9]{4}|[0-9]{1,2})-([0-9]{1,2})-([0-9]{4}|[0-9]{1,2})" -> DATEREGEX -> DATEREGEX_DASH
#   stringr::str_extract_all("20190117 01172019 012019 201901 2019-01-17 01-17-2019 01-2019 2019-01 2019/01/17 01/17/2019 01/2019 2019/01 2019.01.17 01.17.2019 01.2019 2019.01", fourDigitYr)
#   STR2 <- "20200117 01172020 012020 202001 2020-01-17 01-17-2020 01-2020 2020-01 2020/01/17 01/17/2020 01/2020 2020/01 2020.01.17 01.17.2020 01.2020 2020.01"
#   STR3 <- "20201117 11172020 012020 202001 2020-11-17 11-17-2020 01-2020 2020-01 2020/11/17 11/17/2020 01/2020 2020/01 2020.11.17 11.17.2020 01.2020 2020.01"
#   stringr::str_extract_all(STR2, fourDigitYr)
#   stringr::str_extract_all(STR2, twoDigitMonth)
#   stringr::str_extract_all(STR3, twoDigitMonth)
#   stringr::str_extract_all(STR3, twoDigitDay)
#   
#   # gsub("", DATEREGEX, )
#   stringr::str_extract_all("20190117 01172019 012019 201901 2019-01-17 01-17-2019 01-2019 2019-01 2019/01/17 01/17/2019 01/2019 2019/01 2019.01.17 01.17.2019 01.2019 2019.01", DATEREGEX)
# }
