
## AUTO INSTALL/IMPORT MAJOR DEPENDENCIES;
# --------------------------------------------
if(installIfNeeded <- F){
  tryCatch({
    PKG="devtools"; if(!PKG %in% installed.packages()){install.packages(PKG)}
    {do.call(library, list(PKG))}; cat(paste0("`", PKG, "` dependency imported\n"))
  }, error=function(e) {cat(paste0("Couldn't install/import `", PKG, "` package!\n"))})
  tryCatch({
    PKG="tidyverse"; if(!PKG %in% installed.packages()){install.packages(PKG)}
    {do.call(library, list(PKG))}; cat(paste0("`", PKG, "` dependency imported\n"))
  }, error=function(e) {cat(paste0("Couldn't install/import `", PKG, "` package!\n"))})
  tryCatch({
    PKG="reticulate"; if(!PKG %in% installed.packages()){cat(paste0("Installing missing package: `", PKG, "`...\n")); install.packages(PKG)}
    {do.call(library, list(PKG))}; cat(paste0("`", PKG, "` dependency imported\n"))
  }, error=function(e) {cat(paste0("Couldn't install/import `", PKG, "` package!\n"))})
}
# --------------------------------------------
# if("magrittr" %in% installed.packages()){
#   `%>%` <- magrittr::`%>%`
#   `%<>%` <- magrittr::`%<>%`
# }

## HOW TO UPDATE LIBRARY:
# --------------------------------------------
### --- R ---
# redocument=F # redocument=T
if(redocument <- F){
  devtools::document() # {roxygen2::roxygenise(clean=T)}
  system('git add -A && git commit -m "added/edited functions"; git push') ### --- SHELL if you remove system()
  devtools::install_github('srhoads/srhoads')
}

docu <- function(fxn=""){
  cat(eval("
           #' Samantha Rhoads's function to...
           #' @export
           #' @examples
           #'"), paste0(fxn, "()"))
}
# docu(fxn="")

# --------------------------------------------

# ---------------------------------------------------------------------------------

#' Samantha Rhoads's function to recode race more quickly than prior versions!!
#' @export
#' @examples
#' recode_race(v)
recode_race <- function(v, full_names=F, abbrev_names=F, as_factor=F){
  v_orig <- v
  # recode_list <- list("1"=c("1", "White", "white"),
  #                     "2"=c("2", "Black", "black", "Black or African American"),
  #                     "3"=c("3", "Hispanic", "hispanic", "Hispanic or Latino"),
  #                     "4"=c("4", "Asian", "asian"),
  #                     "5"=c("5", "AmInd", "amind", "Nat. Am./AK Nat.", "American Indian or Alaska Native"),
  #                     "6"=c("6", "NHOPI", "nhopi", "Native Hawaiian or Other Pacific Islander"),
  #                     "7"=c("7", "TwoPlus", "twoplus", "Two or More Races", "Two or More", "Two Or More Races")
  #                     
  # )
  if(full_names){
    recode_list <- list("White"=c("1", "white", "White"),
                        "Black or African American"=c("2", "black", "blackorafricanamerican", "black or african american", "Black", "Black or African American"),
                        "Hispanic or Latino"=c("3", "hispanic", "hispanicorlatino", "hisp", "hispanic or latino", "Hispanic", "Hispanic or Latino", "Hisp", "hispaniclatino", "hisplat"),
                        "Asian"=c("4", "asian", "Asian"),
                        "American Indian or Alaska Native"=c("5", "amind", "nat. am./ak nat.", "natamaknat", "american indian or alaska native", "americanindianoralaskanative", "American Indian or Alaska Native", "AmInd", "Nat. Am./AK Nat.", "Nat. Am.", "natam"),
                        "Native Hawaiian or Other Pacific Islander"=c("6", "nhopi", "nativehawaiianorotherpacificislander", "native hawaiian or other pacific islander", "NHOPI", "Native Hawaiian or Other Pacific Islander"),
                        "Two or More Races"=c("7", "twoplus", "twoormoreraces", "two or more races", "two or more", "twoormore", "Two or More Races", "Two or More", "Two Or More Races", "TwoPlus", "2+", "2 or More Races", "2 or more races", "2ormoreraces", "Two+", "two+", "multirace", "Multi-Racial", "multiracial", "Multi-Race")
    )
  } else if(abbrev_names){
    recode_list <- list("White"=c("1", "White", "white"),
                        "Black"=c("2", "black", "blackorafricanamerican", "black or african american", "Black", "Black or African American"),
                        "Hispanic"=c("3", "hispanic", "hispanicorlatino", "hisp", "hispanic or latino", "Hispanic", "Hispanic or Latino", "Hisp", "hispaniclatino", "hisplat"),
                        "Asian"=c("4", "asian", "Asian"),
                        "AmInd"=c("5", "amind", "nat. am./ak nat.", "natamaknat", "american indian or alaska native", "americanindianoralaskanative", "American Indian or Alaska Native", "AmInd", "Nat. Am./AK Nat.", "Nat. Am.", "natam"),
                        "NHOPI"=c("6", "nhopi", "nativehawaiianorotherpacificislander", "native hawaiian or other pacific islander", "NHOPI", "Native Hawaiian or Other Pacific Islander"),
                        "TwoPlus"=c("7", "twoplus", "twoormoreraces", "two or more races", "two or more", "twoormore", "Two or More Races", "Two or More", "Two Or More Races", "TwoPlus", "2+", "2 or More Races", "2 or more races", "2ormoreraces", "Two+", "two+", "multirace", "Multi-Racial", "multiracial", "Multi-Race")
    )
    
  } else {
    recode_list <- list("1"=c("1", "white", "White"),
                        "2"=c("2", "black", "blackorafricanamerican", "black or african american", "Black", "Black or African American"),
                        "3"=c("3", "hispanic", "hispanicorlatino", "hisp", "hispanic or latino", "Hispanic", "Hispanic or Latino", "Hisp", "hispaniclatino", "hisplat"),
                        "4"=c("4", "asian", "Asian"),
                        "5"=c("5", "amind", "nat. am./ak nat.", "natamaknat", "american indian or alaska native", "americanindianoralaskanative", "American Indian or Alaska Native", "AmInd", "Nat. Am./AK Nat.", "Nat. Am.", "natam"),
                        "6"=c("6", "nhopi", "nativehawaiianorotherpacificislander", "native hawaiian or other pacific islander", "NHOPI", "Native Hawaiian or Other Pacific Islander"),
                        "7"=c("7", "twoplus", "twoormoreraces", "two or more races", "two or more", "twoormore", "Two or More Races", "Two or More", "Two Or More Races", "TwoPlus", "2+", "2 or More Races", "2 or more races", "2ormoreraces", "Two+", "two+", "multirace", "Multi-Racial", "multiracial", "Multi-Race")
    )
  }
  
  if(any(!as.character(v) %in% c(names(recode_list)))){
    v1 <- recode_from_list(v, recode_list)
  } else {
    return(v)
  }
  
  if(any(!as.character(v1) %in% c(names(recode_list)))){
    recode_list_b <- list("1"=c("1", "white"),
                          "2"=c("2", "black", "blackorafricanamerican", "black or african american"),
                          "3"=c("3", "hispanic", "hispanicorlatino", "hisp", "hispanic or latino"),
                          "4"=c("4", "asian"),
                          "5"=c("5", "amind", "nat. am./ak nat.", "natamaknat", "american indian or alaska native", "americanindianoralaskanative"),
                          "6"=c("6", "nhopi", "nativehawaiianorotherpacificislander", "native hawaiian or other pacific islander"),
                          "7"=c("7", "twoplus", "twoormoreraces", "two or more races", "two or more", "twoormore")
    )
    v1 <- gsub("[[:space:]]| ", "", tolower(v1)) %>% gsub("2(\\+|ormore|plus).*", "twoormoreraces", .) %>% gsub("[[:punct:]]|[[:space:]]|nothispanic(orlatino).*|no(n|t)white.*", "", .) %>% gsub("no(n|t)his.*lat.*", "", .)
    # v1 <- recode(v1, "white"="1", "blackorafricanamerican"="2", "hispanicorlatino"="3", "asian"="4", "americanindianoralaskanative"="5", "nativehawaiianorotherpacificislander"="6", "twoormoreraces"="7")
    v1 <- recode_from_list(v1, recode_list_b)
  } else {
    return(v1)
  }
  
  if(any(!as.character(v1) %in% c(names(recode_list_b)))){
    if(any(grepl("[[:alpha:]]", v1))){
      racetext <- c("white|caucasian|irish", "black|africa", "hispan|latino|latinx", "asia(|n)", "am.*ind|nat.*am", "pac.*isl|nat.*hawai|nhopi", "twoormore")
      v1 <- ifelse(grepl(racetext[7], v1)&!grepl(paste0(setdiff(racetext, racetext[7]), collapse="|"), v1), "7", 
                   ifelse(grepl(racetext[6], v1)&!grepl(paste0(setdiff(racetext, racetext[6]), collapse="|"), v1), "6", 
                          ifelse(grepl(racetext[5], v1)&!grepl(paste0(setdiff(racetext, racetext[5]), collapse="|"), v1), "5", 
                                 ifelse(grepl(racetext[4], v1)&!grepl(paste0(setdiff(racetext, racetext[4]), collapse="|"), v1), "4", 
                                        ifelse(grepl(racetext[3], v1)&!grepl(paste0(setdiff(racetext, racetext[3]), collapse="|"), v1), "3", 
                                               ifelse(grepl(racetext[2], v1)&!grepl(paste0(setdiff(racetext, racetext[2]), collapse="|"), v1), "2", 
                                                      ifelse(grepl(racetext[1], v1)&!grepl(paste0(setdiff(racetext, racetext[1]), collapse="|"), v1), "1", 
                                                             v1)))))))
    }
  }
  
  if(any(!as.character(v1) %in% c(names(recode_list_b)))){
    if(any(grepl("[[:alpha:]]", v1))){
      racetext <- c("wh|nonminority|polish|swed(e|is)|greek|1", "blk|afr|jamaic|egypt|somali|westindi|2", "hisp|lat|mexic|span|venezu|hondur|nicarag|puer.*ric|dominic|cuba|brazil|guatem|colombi|boliv|argenti|argentin|centralamer|salvad|3", "asia(|n)|chin|japa|korea|4", "am.*(ind|nat|alask|ak)|nat.*(am|ind|alask|ak)|ind.*(am|nat|alask|ak)|(alask|ak).*(am|nat)|5", "pac.*isl|nat.*hawai|nhopi|hawai|hopi|nhop|pacif|islander|tagal(a|o)|fil(l|)ipi", "twoormore|twoplus|races|multiple|morethan|twoorm|7")
      v1 <- ifelse(grepl(racetext[7], v1)&!grepl(paste0(setdiff(racetext, racetext[7]), collapse="|"), v1), "7", 
                   ifelse(grepl(racetext[6], v1)&!grepl(paste0(setdiff(racetext, racetext[6]), collapse="|"), v1), "6", 
                          ifelse(grepl(racetext[5], v1)&!grepl(paste0(setdiff(racetext, racetext[5]), collapse="|"), v1), "5", 
                                 ifelse(grepl(racetext[4], v1)&!grepl(paste0(setdiff(racetext, racetext[4]), collapse="|"), v1), "4", 
                                        ifelse(grepl(racetext[3], v1)&!grepl(paste0(setdiff(racetext, racetext[3]), collapse="|"), v1), "3", 
                                               ifelse(grepl(racetext[2], v1)&!grepl(paste0(setdiff(racetext, racetext[2]), collapse="|"), v1), "2", 
                                                      ifelse(grepl(racetext[1], v1)&!grepl(paste0(setdiff(racetext, racetext[1]), collapse="|"), v1), "1", 
                                                             v1)))))))
    }
  }
  
  if(any(!as.character(v1) %in% c(names(recode_list_b)))){
    if(any(grepl("[[:alpha:]]", v1))){
      v1 <- recode(v1, "w"="1", "b"="2", "l"="3", "a"="4", "ai"="5", "hi"="6", "bw"="7", 
                   "his"="3", "baa"="2", "lat"="3", "hila"="3", "native"="5", "five"="5", "amin"="5", "pac"="6", "six"="6")
    }
  }
  if(any(!as.character(v1) %in% c(names(recode_list_b)))){
    if(any(grepl("[[:alpha:]]", v1))){
      v1 <- ifelse(grepl('pac.*(isl|haw|nat)|nhopi|nat.*haw|haw.*pac|hawai|philip|polynes|samoa|pacific|tonga|hawpi|aorpi|micrones|fiji|maori|vanuat|tuval|palau|pacif|nauru|fil(l|)ipin|islander|malay|melanes|nhorpi', v1), '6', 
                   ifelse(grepl('(amer|alask).*(ind|nat)|(nat|ind).*(alask|amer)|(nat|am).*ind|nat.*am|ind.*nat|indigen|alaska|aioan', v1), '5', 
                          ifelse(grepl('africa|black|westindi|jamaica|blk|somali|morocc|egypt|nigeria', v1), '2', 
                                 ifelse(grepl('^((f|m|e|d|s|)hisp|latin|hislat|spanis|hipanic|lathis)|costaric|venezu|southam|salvador|mexic|ecua(d|t)or|puertoric|portugs|nicarag|bolivi|peruvi|hispanic|latin|chican|brazil|argentin', v1), '3', v1)
                          )))
    }
  }
  
# 
#   if(full_names){
#     recode_list <- list("White"=c("1", "White", "white"),
#                         "Black or African American"=c("2", "Black", "black", "Black or African American", "blackorafricanamerican", "black or african american"),
#                         "Hispanic or Latino"=c("3", "Hispanic", "hispanic", "Hispanic or Latino", "hispanicorlatino", "hisp", "hispanic or latino"),
#                         "Asian"=c("4", "Asian", "asian"),
#                         "American Indian or Alaska Native"=c("5", "AmInd", "amind", "Nat. Am./AK Nat.", "American Indian or Alaska Native", "natamaknat", "american indian or alaska native", "americanindianoralaskanative"),
#                         "Native Hawaiian or Other Pacific Islander"=c("6", "NHOPI", "nhopi", "Native Hawaiian or Other Pacific Islander", "nativehawaiianorotherpacificislander", "native hawaiian or other pacific islander"),
#                         "Two or More Races"=c("7", "TwoPlus", "twoplus", "Two or More Races", "Two or More", "Two Or More Races", "twoormoreraces", "two or more races", "two or more", "twoormore")
#     )
#     # v1 <- recode(v1, "1"="White", "2"="Black or African American", "3"="Hispanic or Latino", "4"="Asian", "5"="American Indian or Alaska Native", "6"="Native Hawaiian or Other Pacific Islander", "7"="Two or More Races")
#     v1 <- recode_from_list(v1, recode_list)
#   }
#   if(abbrev_names){
#     recode_list <- list("White"=c("1", "White", "white"),
#                         "Black"=c("2", "Black", "black", "Black or African American", "blackorafricanamerican", "black or african american"),
#                         "Hispanic"=c("3", "Hispanic", "hispanic", "Hispanic or Latino", "hispanicorlatino", "hisp", "hispanic or latino"),
#                         "Asian"=c("4", "Asian", "asian"),
#                         "AmInd"=c("5", "AmInd", "amind", "Nat. Am./AK Nat.", "American Indian or Alaska Native", "natamaknat", "american indian or alaska native", "americanindianoralaskanative"),
#                         "NHOPI"=c("6", "NHOPI", "nhopi", "Native Hawaiian or Other Pacific Islander", "nativehawaiianorotherpacificislander", "native hawaiian or other pacific islander"),
#                         "TwoPlus"=c("7", "TwoPlus", "twoplus", "Two or More Races", "Two or More", "Two Or More Races", "twoormoreraces", "two or more races", "two or more", "twoormore")
#     )
#     # v1 <- recode(v1, "1"="White", "2"="Black", "3"="Hispanic", "4"="Asian", "5"="AmInd", "6"="NHOPI", "7"="TwoPlus")
#     v1 <- recode_from_list(v1, recode_list)
#   }
  v1 <- recode_from_list(v1, recode_list)
  v_new <- dplyr::if_else(v1 %in% c(names(recode_list)), v1, v_orig)
  if(as_factor){
    if(!is.factor(v1)){
      v1 <- factor(v1, levels=unique(c(names(recode_list), as.character(v1))))
    }
  }
  
  return(v_new)
}


recode_minority <- function(v="subrace"){
  recode_list <- list("POC"=c("POC", "Other POC", "Black", "Hispanic", "Asian", "Nat. Am.", "Nat. Haw.", "Two or More", "Two or More Races", "Canada Visible Minority", "Black or African American", "Native Hawaiian or Other Pacific Islander", "American Indian or Alaska Native", "Hispanic or Latino", "2", "3", "4", "5", "6", "7"),
                      "White"=c("White", "Non-POC", "Caucasian", "Canada White", "W", "1"),
                      "Canada Aboriginal"=c("Canada Aboriginal"),
                      "Unknown"=c("Unknown"))
  recode_from_list(v, recode_list=recode_list)
}

#' Samantha Rhoads's function to recode gender more quickly than prior versions!!
#' @export
#' @examples
#' recode_gender(v)
recode_gender <- function(v, full_names=F){
  v0 <- gsub("[[:space:]]| ", "", tolower(v)) %>% gsub("[[:punct:]]|[[:space:]]|nothispanic(orlatino).*|no(n|t)white.*", "", .)
  v1 <- recode(v0, "male"="1", "female"="2", "nonbinary"="3", "unknown"="NA")
  
  if(any(grepl("[[:alpha:]]", v1))){
    gendertext <- c("\\bmale", "female|fmale", "nonbinary|other", "none|donotwishto|unknown|undefined|notprovid|decline|no.*(avail|applic)|\\bna\\b|nothin")
    v1 <- #ifelse(grepl(gendertext[7], v1)&!grepl(paste0(setdiff(gendertext, gendertext[7]), collapse="|"), v1), "7",
      # ifelse(grepl(gendertext[6], v1)&!grepl(paste0(setdiff(gendertext, gendertext[6]), collapse="|"), v1), "6", 
      # ifelse(grepl(gendertext[5], v1)&!grepl(paste0(setdiff(gendertext, gendertext[5]), collapse="|"), v1), "5", 
      # ifelse(grepl(gendertext[4], v1)&!grepl(paste0(setdiff(gendertext, gendertext[4]), collapse="|"), v1), "4", 
      ifelse(grepl(gendertext[3], v1)&!grepl(paste0(setdiff(gendertext, gendertext[3]), collapse="|"), v1), "3", 
             ifelse(grepl(gendertext[2], v1)&!grepl(paste0(setdiff(gendertext, gendertext[2]), collapse="|"), v1), "2", 
                    ifelse(grepl(gendertext[1], v1)&!grepl(paste0(setdiff(gendertext, gendertext[1]), collapse="|"), v1), "1", 
                           na_if_(v1))))#))))
    
    if(any(grepl("[[:alpha:]]", v1))){
      gendertext <- c("\\b(m|mal|man|he|amle)|him|dude|mascul|guy|bro|his|ftm|femaletomale|one|boy|01", "fem|f|woman|girl|lady|femal|she|her|gal|wman|weiblich|mtf|maletofemale|frau|two|wmn|02", "non.*bin|three|3|03", "unk")
      v1 <- #ifelse(grepl(gendertext[7], v1)&!grepl(paste0(setdiff(gendertext, gendertext[7]), collapse="|"), v1), "7",
        #                ifelse(grepl(gendertext[6], v1)&!grepl(paste0(setdiff(gendertext, gendertext[6]), collapse="|"), v1), "6", 
        #                       ifelse(grepl(gendertext[5], v1)&!grepl(paste0(setdiff(gendertext, gendertext[5]), collapse="|"), v1), "5", 
        #                              ifelse(grepl(gendertext[4], v1)&!grepl(paste0(setdiff(gendertext, gendertext[4]), collapse="|"), v1), "4", 
        ifelse(grepl(gendertext[3], v1)&!grepl(paste0(setdiff(gendertext, gendertext[3]), collapse="|"), v1), "3",
               ifelse(grepl(gendertext[2], v1)&!grepl(paste0(setdiff(gendertext, gendertext[2]), collapse="|"), v1), "2",
                      ifelse(grepl(gendertext[1], v1)&!grepl(paste0(setdiff(gendertext, gendertext[1]), collapse="|"), v1), "1",
                             v1)))#))))
      #   
      #   
      if(any(grepl("[[:alpha:]]", v1))){
        v1 <- recode(v1, "m"="1", "f"="2", "n"="3", "her"="2", "g"="2", "his"="1")
        
        if(any(grepl("[[:alpha:]]", v1))){
          gendertext <- c("man|mal", "fe", "3", "unk")
          v1 <- ifelse(grepl(gendertext[3], v1)&!grepl(paste0(setdiff(gendertext, gendertext[3]), collapse="|"), v1), "3",
                       ifelse(grepl(gendertext[2], v1)&!grepl(paste0(setdiff(gendertext, gendertext[2]), collapse="|"), v1), "2",
                              ifelse(grepl(gendertext[1], v1)&!grepl(paste0(setdiff(gendertext, gendertext[1]), collapse="|"), v1), "1",
                                     v1)))#))))
        }
        #     if(any(grepl("[[:alpha:]]", v1))){
        #       v1 <- ifelse(grepl('pac.*(isl|haw|nat)|nhopi|nat.*haw|haw.*pac|hawai|philip|polynes|samoa|pacific|tonga|hawpi|aorpi|micrones|fiji|maori|vanuat|tuval|palau|pacif|nauru|fil(l|)ipin|islander|malay|melanes|nhorpi', v1), '6', 
        #                    ifelse(grepl('(amer|alask).*(ind|nat)|(nat|ind).*(alask|amer)|(nat|am).*ind|nat.*am|ind.*nat|indigen|alaska|aioan', v1), '5', 
        #                           ifelse(grepl('africa|black|westindi|jamaica|blk|somali|morocc|egypt|nigeria', v1), '2', 
        #                                  ifelse(grepl('^((f|m|e|d|s|)hisp|latin|hislat|spanis|hipanic|lathis)|costaric|venezu|southam|salvador|mexic|ecua(d|t)or|puertoric|portugs|nicarag|bolivi|peruvi|hispanic|latin|chican|brazil|argentin', v1), '3', v1)
        #                           )))
        #     }
      }
    }
  }
  if(full_names){
    v1 <- recode(v1, "1"="Male", "2"="Female", "3"="Non-Binary")
  }
  
  v1
}


gender_list_short <- {list(
  "female" = c("femlale", "females", "frau", "weiblich", "she", "fem", "fema","femaleidentif","feman","femino","wmn","femalenotto", "femalke", "femaleiprefernottodisclose", "femal", "felame", "femalechoosenotto","wman",
               "female", "shef", "her", "femaile", "feamle", "femalle", "sfemalev", "femalef", "ffemale", "ffemale", "femaleemaleale", "femnale", "woman", "girl", "femaile", "femail", 
               "femaleemaleail", "idonotwishtoprovidethisinfofemale", "femalefemale", "queerfem", "transfemale", "transwoman", "mtf"),
  "male" = c("males", "mannlich", "mann", "männlich", "male", "him", "hismale", "mail", "christopher", "chris", "michael", "fsbm", "mfale", "mlae", "mike", 
             "dude", "guy", "malenotto", "maleiprefernottodisclose", "malechoosenotto", "femaletomaletrans","comalepanjobboard",
             "maley", "his", "tospecifemaley", "amle", "maile", "smalev", "malem", "malevisual", "maleabogal","oluwafemi","mmale", "maleale","make", "man", "boy", "malemale", "maleundisclosed", "maleidentif","maleau","isazamale","tegamale",
             "malequeer fluid", "malequeerfluid", "gayman", "transmale", "maleiamtransftm", "transedftm", "ftm"),
  "NA" = c("NATOEVERYONE", "malefemalerace", "malefemale", "femalemale")
)}

gender_list <- {mapply(c, 
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
                       SIMPLIFY=FALSE)}

#vim nano emacs
race_list_short <- {list(
  "american indian or alaska native" = c("american indian or alaska native", 
                                         "aamericanindianoralaskanative","aian","aina","aindian","nativeamericanamericanindianalaskanative",
                                         "aioan","aioran","alakan","alaska","alaskanative","alaskannative","nativeamericanalasknative",
                                         "alaskannativeamerican","ameindia","ameralaskaindian","amerianindianalaskanative","american indian or alaska native","americani","americanind","americanindianalaskannativeallpersonshavinginanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhiteomaintainsculturalidentificationthroughtribalaffiliationorcommunityrecognition",
                                         "americanindalasnat","americanindiainalaskanative","americanindian","americanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment",
                                         "wnativeamerican"),
  "asian" = c("asian","viet","asiana","asianorasianeuropean","indian","asianisual","minahasianindonesia","chineseindonesia","eurasiansingapore","indonesia","batakindonesia",
              "asion","twoasian","asiantwo","asianv","chineseunitedkingdom","thai","thaithailandthailand","sundaneseindonesia","sthailandthailand","javaneseindonesia",
              "fasian","masian","india","rasian","asianorasianbritish","asianandasianamericanincludespakistanisindians","bangladeshi","southasian","sindhiindian","koreanamerican","chinesejapanese","asiachinese","lasian", 
              "aisian","asianian","cambodian","vietnamese","korean","japanese","chinese","asianindian","southasiaeastindianincludingindianpakistanisrilankanbangladeshieastindiansfromguyanatrinidadeastafrica","asianinanofthealpeoplesofthefareastsoutheastasiaortheindiansubcontinentincludingforexamplecambodiachinaindiajapankoreamalasiapakistanthephilippineislsthailvietnam",
              "asain","nativeasian","southeastasian","asianasianamerican","southasiaeastindian","asiannothispanicorlatono","asianorasianamericaneuropean"),
  "black or african american" = c("black or african american","blackorafricanamericanblackorafricanamerican","blackorafricanamericaninanyoftheblackracialgroupsofafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericanincludingcentralamericanandwhiteomaintaintribalaffiliationorcommunityattachment","blackorafricanamerican","blackoramerican","morocco","blacknothislatino","blackorafricanamericantwo",
                                  "guyanaian","jamaican","blackcarribamer","fblack","mblack","blackorafricanamericannofhispanic","blackorafricanamericaninanoftheblackracialgroupsofafrica",
                                  "blackorafricanamericaninanyoftheblackracialgroupsofafricaamericanindianoralaskanativeinanyofthealpeoplesofnorthandsouthamericaincludingcentralamericaandwhiteomaintaintribalaffiliationorcommunityattachment","blackaframer",
                                  "bk","afircanamerican","blackoraframnisporlatino","blackorafram","af","somalian","baa","ba","blackforafricanamerican","bl","blackorafricanamerican","twoblackorafricanamerican",
                                  "black","afri","african","blkafram","blackafram","blackorafricanamericaninanyoftheblackracialgroupsofafrica","raceblack"),
  "hispanic or latino" = c("hispanic or latino", 
                           "canales","hispanicpuerrico","hispanicorlatinos","nonwhitelatinomerican","nonwhitelatinomericanincludingindigenopersonsfromcentralandsouthamerican",
                           "dunbrazil","hispanicinic","spaniclatino","hispanicc","latinomericanincludingindigenopersonsfromcentralandsouthamerican","latinotwo",
                           "spanish","hislatino","hispanicall","hispanicnative","latinoapersonofcubanmexicanpuerricansouthorcentralamericanorspanishcultureorregardlessofblackorafricanamericaninanyoftheblackracialgroupsofafrica",
                           "hispanicorlatinoallothers","hisporlat","hispa","latino","asianpacificislander","hispancorlatino","latina","latinx","latin"),
  "native hawaiian or other pacific islander" = c("native hawaiian or other pacific islander","nativehawiianorothrpacificisl","asianorpacisl",
                                                  "polynesian","polynese","polynesia","melanesian","melanese","melanesia","micronesia","micronesian","micronese",
                                                  "tuvalu","vanuatu","samoan","samoa","fijian","fiji","nauru","nauruan","palau","palauan","fillipino"),
  "white" = c("white","thispanicinformationtwoormoreraces","europeannewzealand","europeannewzealand","europeans","unitedkingdom",
              "azeri","whitenon","whitenonsasa","whitenofhispanichispanic","whiteite","whiteinanyofthealpeoplesofeuropethemiddleeasrnorthafricant",
              "whitemiddleeastern","fwhite","mwhite","rwhite","germany","germanscottish","cauwhite","caasian","abalatralia",
              "whiteundisclosed","caucasion","nonminority","whit","whire","uswhite","caucasian","wh","whitenothispanicorigin","nonhispanicwhite",
              
              "meast","meast","meast","meast","meast","meast","arabmiddleeastern","arabian","lebanese","iraqi","middleastern","mideastern","jordanian","mideastrn","middleeastern","westasianarabincludingarabianarmenianiranianisraelilebanesepalestiniansyrianturkishegyptianiraqi",
              "iraq","greekamerican","saudi","turkish","mideastern","jordanian","persianamerican","middleeastren","lebanese","iraqi","iranianamerican","greek","arabmiddleeastern","whitetwotwo",
              "arabian","syrian","armenian","arab","afghani","kurdish","mediterranean","iranian","persian","mideast","arabicmiddleeastern","aribac","pakistani","middleeastern","middleeast","arabic"),
  "two or more races" = c("two or more races","nativeamericanalaskanathispanic","whiteblackorafricanamericanamericanindianoralaskanativetwoormoreraces","whiteblackorafricanamericantwoormoreraces","whitetwo","latinowhitetwoormoreraces",
                          "bhispanicorlatinowhiteballpersonsofmexicanpuerricancubancentralorsouthamericanorspanishcultureorandofthetwoormoreracesorlatino","americanindianoralaskanativeasianblackorafricanamericannativehawaiianorotherpacificislander",
                          "nativehawaiianorotherpacificislanderblackorafricanamericanwhite"),
  "NA" = c("NATOEVERYONE","notspecified","memberofvisibleminoritycanada")
)}

race_list <- {mapply(c, 
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
                            "NA" = c("tals","VALUE","NA", 
                                     "nr","ng","na","na","EVENTUALLYWEWILLDELETEITBUTWENEEDTHEPLACEHOLDER","ho","qal","dti","fax","perfer","ofcolor","isclose","nolatino","oth","preferrednot","icurrentlyworkatforest","nmin", 
                                     "mm","min","ielectnot","frxcom","onlinejobsite","abal","thomas","smith","notdeclaringunitedkingdom","bragray","poc","canvisibleminority","remove", "nfalt","missingor","single","ni","aeorai","visual","sbentivegna","does","dn","sb","nd","bkorkuch","sgps","didindentify","noinformation","declinded","refe","sg","employermt","dnr","leftblank", "iama","osed","uknown","anchoice","unnown","apprentice","gbrnotdeclared","electrician","lack","followstepscompleteeeoinformation","notinformation","cannoneofthecategoriesapply", 
                                     "report","missingblank","br","tmr","undefined","respond","needhrreview","say","prefer","observationneeded","ielect","multiples","thispanicquestion","aiisclose","true","expatriates","iisclose","notsupplied","notselected","choose","william","s","answeredyeshispaniclatinoquestion","unotknow",
                                     "to","do","nonpoc","state","nonhispanic","id","furnish","twas","answer","g","aa","refed","yes","un","ukn","ud","u","c","y","to","sv","r","ndr","or","did","avisual","not","non","entify","not","nanonnon","ola","oim","oca","gyr","s","diversity","nothispanicorlanito","mothispanic","welder","hire","un","oai", 
                                     "eeob","e","empty","none","orm","desc","gender","gmna","minority","no","ns","other","q","o","blank","8","9","10","11","0","u","a","x","z","na","null","unk","answer", "d","o","nralien","tr","ax","dw","dx","i","ad","iw","nspec","ow","bi","biw","unknowen","f","ed","monstercom",
                                     "didnotdisclose","unknownphone","notdelcared","didnotwishtoanswer","iamnothispaniclatinoa","nothispaniclatino","nothispaniclatino"))
                       , sort)), 
                     SIMPLIFY=FALSE)}



recode_race_getridofstrregex <- {paste0(c("yes","wish","us","unitedstatesofamerica","to","sgp","state","self","selected","region","raceethnicity","race","other","othe","oth$","origins","origin","only","ofany","obsolete", 
                                          "notofhispanic$","notlatino$","nothisporlatino","nothispnc$","nothispanicorlatino","nothispanicorl$","nothispanicor$","nothispanicoflatino", 
                                          "nothispaniclatinononhispanic$","nothispaniclatino","nothispanichispanic$","nothispanichis$","nothispaniceorlatino","nothispanic$","nothislatino$", 
                                          "nothislat$","nothis$","norhispanic$","norhislatino$","norhis$","noofhispanic$","nonwhite","nonhispanicorlatino","nonhispanicor$","nonhispaniclatino","nonhispanichispanic$","nonhispanichis$","nonhispanic$","nonhislat$","nonhis$","nonexempttoexempt", 
                                          "nofhispanic$","nispnlatino$","nisplatino","nisplat$","nispanicorlatino","nispanic$","nispanc$","nislatino$","nislatin$","nislat$", 
                                          "lessthan","ï","hours","heritage","half", 
                                          "gender","gb","furnish", # "males","male",  # "females","female", 
                                          "ethnicity","ethnic","eorlatino","dta","donottoself","donotto","cmty","apersonhavingorigins","alls","^nhispanic$","^dstate"), 
                                        collapse="|")}

recode_gender_getridofstrregex <- {paste0(c("gender","sexis","earnings","sex","tospecife","lessthan","over","other", 
                                            "inanyofthealpeoplesofthefareastsoutheastasiathesubcontinentincludingfexamplecambodiachinaindiajapankeamalaysiapakistanthephilippineisls",
                                            "noselfid","assume","senior","junior","applicantselected","other","non","unotknow","y","ukn",
                                            "apersonhavingiginsinanyoftheiginalpeoplesofthefareastsoutheastasiathesubcontinentincludingfexamplecambodiachinaindiajapankeamalaysiapakistanthephilippineislsthailvietnam",
                                            "and","selected","tonot","self","professionals","professional","toidentif$",
                                            # "afircanamerican","africanamerican","hispanic","africanamerican","amerindalas", #"ornotorlatino","notorlatino","ormoreraces",# "white","black","non","hisanic","asian","nativeamerican", # "hisanic",
                                            "ethnicity","ethnicit","ethnici","ethnic","ethni","ethn","eth","donotselectthisoptionifyouyour","apersonhavingiginsinanyoftheiginalpeoplesofeuropemiddleeastnthafrica"
), 
collapse="|")}


dfsample <- {data.frame(gender = c("male", "female", "female", 
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
                                 "momoa, jason"), stringsAsFactors = F)}

dfincase <- {data.frame(name=c('charlene teters', 'sandra sunrising osawa'), 
                        firstname=c('charlene', 'sandra sunrising'), 
                        lastname=c('teters', 'osawa'), 
                        gender=c('female', 'female'), 
                        race=c('american indian or alaska native', 'american indian or alaska native'), 
                        stringsAsFactors = F)}




### functions.R #================================================================================================================================

#' A function to see whether there are any non-NA values; can go inside `select_if()`
#' @export
#' @examples
#' not_all_na(x)
not_all_na <- function(x){any(!is.na(x)) }

#' A function to see whether all values are NA; can go inside `select_if()`
#' @export
#' @examples
#' all_na(x)
all_na <- function(x){all(is.na(x)) }

#' A function that can go inside `select_if()` and checks if all items in a vector are identical or not, returns TRUE if they're not all identical
#' @export
#' @examples
#' not_all_same()
not_all_same <- function(x){length(unique(x))>1}

#' A function to select the unique columns in a dataframe
#' @export
#' @examples
#' select_unique_columns(d)
select_unique_columns <- function(d){
  d[!duplicated(as.list(d))]
}


#' A function to do grep() but pasting a vec instead of just a string
#' @export
#' @examples
#' grep_(pattern, v, exact=F, ignore.case=F, value=F)
grep_ <- function(pattern, v, exact=F, ignore.case=F, value=F) grep(paste_regex(pattern, exact=exact), v, ignore.case=ignore.case, value=value)

#' A function to find matching string anywhere in a dataframe
#' @export
#' @examples
#' grep_all_df_df(pattern, df, exact=F, ignore.case=F, print=F)
grep_all_df_df <- function(pattern, df, exact=F, ignore.case=F, print=F){
  loc <- lapply(df, function(v) grep_(pattern, v, exact=exact, value = F, ignore.case=ignore.case)) %>% unlist()
  if(print) print(loc)
  df[loc, ]
}

#' A function to find matching string anywhere in a dataframe
#' @export
#' @examples
#' grep_all_df_colnames(pattern, df, exact=F, ignore.case=F, print=F)
grep_all_df_colnames <- function(pattern, df, exact=F, ignore.case=F, print=F){
  loc <- lapply(df, function(v) grep_(pattern, v, exact=exact, value = T, ignore.case=ignore.case)) %>% drop_empty()
  if(print) print(loc)
  names(loc)
}

#' A function to find matching string anywhere in a dataframe
#' @export
#' @examples
#' grep_all_df(pattern, df, colnames=F, exact=F, ignore.case=F, print=F, cells_only=F, cells_only_discrete=F, rownums_only=F)
grep_all_df <- function(pattern, df, colnames=F, exact=F, ignore.case=F, print=F, cells_only=F, cells_only_discrete=F, rownums_only=F){
  df$rownum <- 1:nrow(df)
  
  if(colnames) grep_all_df_colnames(pattern, df, exact=exact, ignore.case=ignore.case, print=print)
  
  else if(cells_only) {
    colnamez <- grep_all_df_colnames(pattern, df, exact=exact, ignore.case=ignore.case, print=print)
    grep_all_df_df(pattern, df, exact=exact, ignore.case=ignore.case, print=print) %>% select(dplyr::one_of(c(colnamez, "rownum")))
  }
  else if(cells_only_discrete){
    colnamez <- grep_all_df_colnames(pattern, df, exact=exact, ignore.case=ignore.case, print=print)
    cellsdf <- grep_all_df_df(pattern, df, exact=exact, ignore.case=ignore.case, print=print) %>% select(dplyr::one_of(c(colnamez)))#, "rownum")))
    # rownames(cellsdf) <- make.unique(as.character(cellsdf$rownum))
    lapply(cellsdf, function(v) grep_(pattern, v, exact=exact, ignore.case=ignore.case, value=T) %>% #paste0(., grep_(pattern, v, exact=exact, ignore.case=ignore.case, value=F)))
             tibble(value=., rownum=grep_(pattern, v, exact=exact, ignore.case=ignore.case, value=F)) %>%
             dplyr::group_by(value) %>% dplyr::summarize(rownum = paste0(rownum, collapse=", ")))
  }
  else if(rownums_only){
    colnamez <- grep_all_df_colnames(pattern, df, exact=exact, ignore.case=ignore.case, print=print)
    grep_all_df_df(pattern, df, exact=exact, ignore.case=ignore.case, print=print) %>% .$rownum #select(dplyr::one_of("rownum")) %>% dplyr::distinct() %>% unlist() %>% as.character()
  }
  else grep_all_df_df(pattern, df, exact=exact, ignore.case=ignore.case, print=print)
}


#' A function to do gsub() but pasting a vec instead of just a string
#' @export
#' @examples
#' gsub_(pattern, to, v, exact=F, ignore.case=F)
gsub_ <- function(pattern, to, v, exact=F, ignore.case=F) gsub(paste_regex(pattern, exact=exact), to, v, ignore.case=ignore.case)

#' A function to do gsub() but ignoring case as default & pasting a vec instead of just a string
#' @export
#' @examples
#' gsubic(pattern, to, v, exact=F, ignore.case=T)
gsub_ic <- function (pattern, replacement, x, ignore.case=T, perl=F, 
                     fixed=F, useBytes=F, exact=F) {
  pattern <- paste_regex(pattern, exact=exact)
  if (!is.character(x)){ x <- as.character(x) }
  .Internal(gsub(as.character(pattern), as.character(replacement), x, ignore.case, perl, fixed, useBytes))
}

#' Samantha Rhoads's function to grep() but ignoring case as default & pasting a vec instead of just a string
#' @export
#' @examples
#' grep_ic()
grep_ic <- function (pattern, x, ignore.case = T, perl=F, value=F, fixed=F, useBytes=F, invert=F) {
  if (!is.character(x)){ x <- structure(as.character(x), names = names(x)) }
  .Internal(grep(as.character(pattern), x, ignore.case, value, perl, fixed, useBytes, invert))
}


#' Samantha Rhoads's function to grepl() but ignoring case as default & pasting a vec instead of just a string
#' @export
#' @examples
#' grepl_ic()
grepl_ic <- function (pattern, x, ignore.case=T, perl=FALSE, fixed=FALSE, useBytes=FALSE) {
  if (!is.character(x)){ x <- as.character(x) }
  .Internal(grepl(pattern=as.character(pattern), x=x, ignore.case=ignore.case, perl=perl, fixed=fixed, useBytes=useBytes))
}


#' A function kinda like `dplyr`'s `select()` but works on selecting stuff from lists
#' @export
#' @examples
#' select_list(list, pattern, exact=F, ignore.case=T)
select_list <- function(list, pattern, exact=F, ignore.case=T) list[grep_(pattern, names(list), exact=exact, ignore.case=ignore.case)] 


#' This function brings a desired word to the front of a string (reorders string itself)
#' @export
#' @examples
#' word_to_front(wrd, v)
word_to_front <- function(wrd, v) {
  wrd1 <- paste0(wrd, " ")
  wrd2 <- paste0(wrd, "$")
  wrd3 <- paste0(wrd, "-")
  wrd4 <- paste0(wrd, "_")
  v <- sub(paste0('^(.*) ', wrd1), paste0(wrd, ' \\1 '), v)
  v <- sub(paste0('^(.*) ', wrd2), paste0(wrd, ' \\1 '), v)  
  v <- sub(paste0('^(.*) ', wrd3), paste0(wrd, '-\\1 '), v)
  v <- sub(paste0('^(.*) ', wrd4), paste0(wrd, ' \\1 '), v)
  trimws_(v)
}


#' A function that reminds us what some regular expressions are, like and/or
#' @export
#' @examples
#' regex_info()
regex_info <- function(){
  data.frame(
    type = c(
      'alpha',
      'alnum',
      'digit',
      'uppercase / lowercase',
      'and',
      'or',
      'escape',
      'repl w/ self'
    ),
    regex = c(
      '[[:alpha:]]|[[:a-zA-Z:]]',
      '[[:alnum:]]',
      '[[:digit:]]|\\d|[[:0-9:]]',
      '[[:upper:]] / [[:lower:]]',
      '.*',
      '|',
      '\\',
      'pattern="asdg(text)2a93", replacement="\\1"...returns "text"'
    )
  )
}


#' A function that can filter a dataframe so it includes all duplicates of your variable of choice
#' @export
#' @examples
#' filter_duplicated(d, var="id")
filter_duplicated <- function(d, var="id"){
  vdups <-  d %>% dplyr::filter_at(dplyr::vars(dplyr::one_of(var)), function(v) duplicated(v)) %>% .[[var]]
  d %>% dplyr::filter_at(dplyr::vars(dplyr::one_of(var)), function(v) v %in% vdups)
}



recode_from_list_v1 <- function(v, recode_list = list('NA' = c('na', 'not applicable'), 'TRUE' = c('true', 'tru'))){
  v <- tolower(v) %>% trimws_() %>% gsub('\\&', 'and', .) %>% gsub('\\.|\\(|\\)', '', .) %>% gsub('-', ' ', .) %>% trimws_()
  v <- gsub("[^ |[:alpha:]]", "", v, perl = T)
  v <- tolower(v)
  recode_key <- lapply(names(recode_list), function(x) {
    to_recode <- recode_list[[x]]
    setNames(rep(x, length(to_recode)), to_recode)
  })
  recode_key <- unlist(recode_key)
  v <- dplyr::recode(v, !!!recode_key)
  gsub("^NA$", NA, v, perl = T)
}

#' Samantha Rhoads's function to recode from a list so you don't have to write out every recode to and from statement individually. Utilizes dplyr::recode. A function that acts like `recode()` but works on a list
#' @export
#' @examples
#' recode_from_list(.x, recode_list=list('RECODETO1'=c('RECODEFROM1a', 'RECODEFROM1b'), 'RECODETO2'=c('RECODEFROM2a', 'RECODEFROM2b')), .default=NULL, .missing=NULL)
recode_from_list <- function(.x, recode_list=list('RECODETO1'=c('RECODEFROM1a', 'RECODEFROM1b'), 'RECODETO2'=c('RECODEFROM2a', 'RECODEFROM2b')), .default=NULL, .missing=NULL){
  recode_key <- lapply(names(recode_list), function(x) {
    to_recode <- recode_list[[x]]
    setNames(rep(x, length(to_recode)), to_recode)
  })
  recode_key <- unlist(recode_key)
  v <- dplyr::recode(.x, !!!recode_key, .default=.default, .missing=.missing)
  return(v)
}



#' A function that finds strings in a dataframe anywhere (like `grep()` but many strings)
#' @export
#' @examples
#' findme(d=df00.5, g1="", g2="", g3="", g4="", g5="")
findme <- function(d=df00.5, g1="", g2="", g3="", g4="", g5=""){
  (data.frame(grep_all_df(g1, d, ignore.case = T))) %>%
    grep_all_df(g2, ., ignore.case = T) %>%
    grep_all_df(g3, ., ignore.case = T) %>%
    grep_all_df(g4, ., ignore.case = T) %>%
    grep_all_df(g5, ., ignore.case = T) %>%
    data.frame(., row.names = NULL)
}


#' A function that rounds up to next highest digit base #, ie, to 100 or 1000 or 1000 etc...
#' @export
#' @examples
#' roundupc(x)
roundupc <- function(x) { 10^ceiling(log10(x)) }


#' A function that gets 5 digit zip codes; it extracts relevant first 5 digits from zip code when it's formatted like 11111-1111
#' @export
#' @examples {v <- c('924832sjkhf',' 20439-22912', '93488-9293', '23984_93284', '39842-3949 32432', '328rjd2 nh22 22242 ')}
#' zipcode5(v)
zipcode5 <- function(v){stringr::str_extract_all(v, pattern="(?<!\\d)\\d{5}(?!\\d)")} # OLD:# zipcode5 <- qdapRegex::rm_(pattern="(?<!\\d)\\d{5}(?!\\d)", extract=TRUE)


#' A function to paste a vector in a regex way with '|' partial
#' @export
#' @examples
#' paste_regex_partial(v, collapse='|')
paste_regex_partial <- function(v, collapse='|') {paste0(v, collapse=collapse) }


#' A function to paste a vector in a regex way with '|' exact with '^' in front and '$' in back
#' @export
#' @examples
#' paste_regex_exact(v, collapse='|')
paste_regex_exact <- function(v, collapse='|'){paste0('^', v, '$', collapse=collapse) }


#' A function to paste a vector in a regex way with '|' options for exact or partial. Is it working?
#' @export
#' @examples
#' paste_regex(v, collapse='|', exact=F)
paste_regex <- function(v, collapse='|', exact=F){
  if(exact) { paste_regex_exact(v) }
  else { paste_regex_partial(v) }
}


#' A function to clean dates: recode a date that's originally in a format like `January 17, 1996 11:00 AM`
#' @export
#' @examples
#' newdate()
newdate <- function(v){ lubridate::mdy_hm(v) %>% lubridate::date()}


#' This function allows you to remove characters after a certain character of your choosing
#' @export
#' @examples
#' remove_after_char(v, sep=' ')
remove_after_char <- function(v, sep=' '){ gsub(paste0(sep, ".*"),"", v)}


#' A function to paste a vector in a regex way with '|'
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
#' @export
#' @examples
#' is.factorchar(x)
is.factorchar <- is.charorfact <- function(x){ ifelse(is.character(x) | is.factor(x), T, F) }


#' This function allows you to apply a function to a whole dataframe (inferior to purrr::map_if(is.dataframe, fun) tho!)
#' @export
#' @examples
#' dply_all()
dply_all <- function(x, fun){ data.frame(lapply(x, fun), stringsAsFactors = F)}

#' A function lapply stuff to just non-numeric columns in a df
#' @export
#' @examples
#' lapply_on_nonumeric()
lapply_on_nonumeric <- function(df, fun) {
  lapply(df, function(v){
    if(is.character(v) | is.factor(v)) v <- fun(v); v
  })
}

#' A function to apply stuff to just non-numeric columns in a df
#' @export
#' @examples
#' dply_nonnumeric()
dply_nonnumeric <- function(x, fun) data.frame(lapply_on_nonumeric(x, fun), stringsAsFactors = F)

#' A function to apply stuff to all columns in a df, specific to numeric or non-numeric type?
#' @export
#' @examples
#' dply()
dply <- function(df, fun, num=T){
  if(num) df <- dply_all(df, fun=fun); df
  if(!num) df <- dply_nonnumeric(df, fun=fun); df
}

#' A function to operate dplyr?
#' @export
#' @examples
#' ply()
ply <- function(x, fun, num=T){
  if(is.data.frame(x)) x <- dply(x, fun=fun, num=num); x
  if(is.character(x)|is.factor(x)|is.numeric(x)|is.vector(x)) x <- fun(x); x
}


#' A function
#' @export
#' @examples
#' dply_to_title(x, stringsAsFactors=T)
dply_to_title <- string_to_title <- vecs_to_title <- to_title <- function(x, stringsAsFactors=T) {
  if(is.data.frame(x)|is.list(x)) x <- lapply(x, function(xx){
    if(is.character(xx)|is.factor(xx)) xx <- stringr::str_to_title(xx); xx
  }) %>% data.frame(., stringsAsFactors=stringsAsFactors); x
  if(is.vector(x)|is.array(x)|is.character(x)|is.factor(x)) x <- stringr::str_to_title(x); x
}


#' A function
#' @export
#' @examples
#' gsub_ply(from, to, x, ignore.case=T, num=T)
gsub_ply <- function(from, to, x, ignore.case=T, num=T) ply(x, function(xx) gsub(from, to, xx, ignore.case=ignore.case), num=num)


`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`

## Loads pipes (`%>%` and `%<>%`) into R env
#' @export
#' @examples
#' pipes()
pipes <- function(){
  `%>%` <<- magrittr::`%>%` 
  `%<>%` <<- magrittr::`%<>%`
}

pipe1 <- function() `%>%` <<- magrittr::`%>%` 
pipe2 <- function() `%<>%` <<- magrittr::`%<>%`

#' same as list.files() except default recursive=T and full.names=T
#' @export
#' @examples
#' list_files(p, pattern=NULL, recursive=T, full.names=T)
list_files <- function(p, pattern=NULL, recursive=T, full.names=T) list.files(p, pattern=pattern, recursive=recursive, full.names=full.names)


#' This function allows you to 
#' @export
#' @examples
#' try_data_frame(x)
try_data_frame <- function(x) tryCatch(data.frame(x., stringsAsFactors = F), error=function(e) x)


#' A function just like read_excel but better bc it does all the sheets!
#' @export
#' @examples
#' readexcel(file, bindsheets=F, skip=0, col_types='text', simplify=T)
readexcel <- function(file, bindsheets=F, skip=0, col_types='text', simplify=T){
  sheets <- readxl::excel_sheets(file)
  d <- lapply(sheets, function(sheet) readxl::read_excel(file, sheet, skip=skip, na = c('NA', 'None', 'N/A', '-', ''), col_types=col_types))
  names(d) <- sheets
  if(simplify) d <- drop_empty(try_combine_compact(d))
  if(bindsheets) d <- dplyr::bind_rows(d)
  d
}

#' A function to tryCatch read excel file
#' @export
#' @examples
#' try_read_excel(file, bindsheets=F, col_types='text')
try_read_excel <- function(file, bindsheets=F, col_types='text') tryCatch(readexcel(file, bindsheets=bindsheets, col_types=col_types), error=function(e) NULL)



#' A function to a tryCatch read csv file
#' @export
#' @examples
#' try_read_csv(file)
try_read_csv <- function(file){
  d <- tryCatch(read.csv(file), error=function(e) file)
  if(is.character(d)) d <- tryCatch(readr::read_csv(file), error=function(e) NULL)
  d
}


#' A function to read csv files
#' @export
#' @examples
#' read_csvs(filelist, bindrows=F, simplif=T)
read_csvs <- function(filelist, bindrows=F, simplif=T){
  d <- lapply(filelist, function(x) try_read_csv(x))
  if(simplif) d <- try_combine_compact(d) %>% drop_empty()
  if(bindrows) d <- dplyr::bind_rows(d)
  d
}

#' A function
#' @export
#' @examples
#' try_read_rda(file)
try_read_rda <- function(file){tryCatch(get(load(file)), error=function(e) NULL)}

#' A function
#' @export
#' @examples
#' try_read_feather(file)
try_read_feather <- function(file) {
  pkg('feather')
  tryCatch(read_feather(file), error=function(e) NULL)
}

#' A function
#' @export
#' @examples
#' try_read_table(f)
try_read_table <- function(f) tryCatch(data.frame(readr::read_table(f), stringsAsFactors = F), error=function(e) NULL)

#' A function
#' @export
#' @examples
#' try_read_delim(f, delim=';')
try_read_delim <- function(f, delim=';') tryCatch(data.frame(readr::read_delim(f, trim_ws = T, delim=delim), stringsAsFactors = F), error=function(e) NULL)


#' A function
#' @export
#' @examples
#' read_dat(f)
read_dat <- function(f) {
  ddelim <- try_read_delim(f)
  dtable <- try_read_table(f)
  if(!is.null(dtable) & !is.null(ddelim)) d <- tryCatch(dplyr::bind_rows(dtable, ddelim), error=function(e) NULL)
  if(!is.null(dtable) & is.null(ddelim)) d <- tryCatch(dtable, error=function(e) NULL)
  if(is.null(dtable) & !is.null(ddelim)) d <- tryCatch(ddelim, error=function(e) NULL)
  d
}

#' A function
#' @export
#' @examples
#' try_read_dat(f)
try_read_dat <- function(f) tryCatch(read_dat(f), error=function(e) NULL)


#' A function
#' @export
#' @examples
#' read_dats(flist, bind=F)
read_dats <- function(flist, bind=F){
  d <- lapply(flist, try_read_dat)
  if(bind) d <- dplyr::bind_rows(d)
  d
}


#' A function
#' @export
#' @examples
#' read_file(file, bindsheets=F)
read_file <- function(file, bindsheets=F){
  d <- try_read_rda(file)
  if(is.null(d) | grepl("\\.xls$|\\.xlsx$", file, ignore.case=T)) d <- try_read_excel(file, bindsheets=bindsheets, col_types=col_types)
  if(is.null(d) | grepl("\\.csv$", file, ignore.case=T)) d <- try_read_csv(file)
  if(is.null(d) | grepl("\\.dat$", file, ignore.case=T)) d <- try_read_dat(file)
  if(is.null(d) | grepl("\\.f$|\\.feather$", file, ignore.case=T)) d <- try_read_feather(file)
  d <- try_combine_compact(d)
  try_data_frame(d)
}


#' A function
#' @export
#' @examples
#' try_read_file(file, bindsheets=F)
try_read_file <- function(file, bindsheets=F) tryCatch(read_file(file), error=function(e) NULL)


#' A function
#' @export
#' @examples
#' read_files(filelist, bindsheets=F, bindrows=F, simplif=T)
read_files <- function(filelist, bindsheets=F, bindrows=F, simplif=T){
  d <- lapply(filelist, function(x) try_read_file(x, bindsheets=bindsheets))
  if(simplif) d <- try_combine_compact(d) %>% drop_empty()
  if(bindrows) d <- dplyr::bind_rows(d)
  d
}


#' A function
#' @export
#' @examples
#' read_df_all(filenames, bindrows=F, bindsheets=F, simplif=T)
read_df_all <- function(filenames, bindrows=F, bindsheets=F, simplif=T) {
  x <- read_files(filenames)
  # names(x) <- gsub_NSRHOADS(gsub("[^_|[:alnum:]]", "", filenames))
  x
}


#' A function
#' @export
#' @examples
#' read_dfs_process(filelist, by=10, outpath="AA/data/", prefix="clean_", startdoc=1)
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
    (sublist <- dplyr::bind_rows(sublist) %>% dplyr::distinct()) %>%
      saveRDS(., filename)
    # return(nrow(sublist))
    print("")
    print(paste0("dim: ", paste0(dim(sublist), collapse=" row "), " col - ", filename))
    print(dplyr::sample_n(dplyr::filter(sublist, !is.na(fLname)), 3))
    # print(paste0("dim: ", dim(sublist)))
  }) %>% print(system.time())
}


#' a function gsubing stuff related to srhoads jl desktop comp path
#' @export
#' @examples
#' gsub_NSRHOADS(x)
gsub_NSRHOADS <- function(x) x %>% gsub("[^_|[:alnum:]]", "", .) %>% gsub("NSRHOADSGitHubdatafilesunzip|srhoads", "", ., ignore.case=T, perl=T)


#' A function
#' @export
#' @examples
#' read_ydrive_write(filenames, csv=F, xlsx=F, xls=F, outpath="data/original/", col_types='text')
read_ydrive_write <- read_excel_allsheets <- function(filenames, csv=F, xlsx=F, xls=F, outpath="data/original/", col_types='text') {
  if(xls|xlsx){
    filenames <- readxl::excel_sheets(filenames)
    lapply(filenames, function(f) {
      print(filename <- paste0(outpath, gsub_NSRHOADS(f), ".rda"))
      tryCatch(readxl::read_excel(filenames, sheet = f, col_types=col_types), error=function(e) NULL) %>%
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


#' A function to unzip a folder/directory
#' @export
#' @examples
#' unzip_dir(zipfile, outdir="unzip")
unzip_dir <- function(zipfile, outdir="unzip"){
  output <- gsub("\\.zip$", "", zipfile)
  output <- gsub("original", outdir, output)
  unzip(zipfile, exdir = output)
  file.remove(zipfile)
  output
  print(output)
}

#' A function to tryCatch unzip_doir
#' @export
#' @examples
#' try_unzip(zipfile)
try_unzip <- function(zipfile){
  tryCatch(unzip_dir(zipfile), error = function(e) zipfile)
}
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------

#' A function to drop empty (NULL) items from a list
#' @export
#' @examples
#' drop_empty(x_list)
drop_empty <- function(x_list) { x_list[unlist(lapply(x_list, length) != 0)] }

#' A function
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

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' remove_precomma()
remove_precomma <- function(vec, trim=T){
  vec %>% gsub(".*,", "", .)
  if(trim) vec %<>% trimspace()
}

#' A function
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



#' A function
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
                                filename_prefix, verbose=F) {
  if(verbose){ print(filename_prefix); print(by) }
  
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


#' A function
#' @export
#' @examples
#' reduce_name_bytes()
reduce_name_bytes <- function(x){
  names(x) <- substr(names(x), start = 1, stop = 93)
  if (is.list(x) & ! is.data.frame(x)) 
    x <- lapply(X = x, FUN = reduce_name_bytes)
  x
}


#' A RECURSIVE function!!
#' @export
#' @examples
#' race_seq_names()
race_seq_names <- function(x){
  names(x) <- stringr::str_replace_all(names(x), "[^[:alpha:]]", "")
  names(x) <- stringr::str_replace_all(names(x), "^", "race")
  if (is.list(x) & ! is.data.frame(x)) 
    x <- lapply(X = x, FUN = race_seq_names)
  x
}

#' A RECURSIVE function
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


#' A function
#' @export
#' @examples
#' try_bind()
try_bind <- function(x){
  x <- tryCatch(dplyr::bind_rows(x), 
                error = function(e) plyr::ldply(x, dplyr::bind_rows))
}


#' A function
#' @export
#' @examples
#' list_to_df()
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


#' A function
#' @export
#' @examples
#' combine_compact()
combine_compact <- function(x){
  x <- plyr::compact(x)
  x <- dplyr::combine(x)
  x
}


#' A function
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


#' A function
#' @export
#' @examples
#' try_compact(x)
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


#' A function
#' @export
#' @examples
#' try_combine_compact(x)
try_combine_compact <- function(x){
  x <- try_combine(x)
  x <- try_compact(x)
  x <- try_combine(x)
  x <- try_compact(x)
  x
}

# ---------------------------------------------------------------------------------

#' A function
#' @export
#' @examples
#' gather_firstname(df)
gather_firstname <- function(df) {
  tidyr::gather(df, "twastwas", "firstname", dplyr::matches("firstname")) %>% 
    dplyr::select(-dplyr::one_of("twastwas")) %>% dplyr::distinct() 
}

#' A function
#' @export
#' @examples
#' gather_lastname(df)
gather_lastname <- function(df) {
  tidyr::gather(df, "twastwas", "lastname", dplyr::matches("lastname")) %>% 
    dplyr::select(-dplyr::one_of("twastwas")) %>% dplyr::distinct() 
}

#' A function
#' @export
#' @examples
#' gather_first_last_name(df)
gather_first_last_name <- function(df) {
  df <- gather_lastname(df)
  df <- gather_firstname(df) 
  # dplyr::distinct(df)
  df
}

# ---------------------------------------------------------------------------------
#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' join_firstlastname()
join_firstlastname <- function(df, firstname, lastname, seq = c("first last", "last,first")) {
  seq <- match.arg(seq)
  fldf <- data.frame(firstname="samantha", lastname="rhoads", name="samantha rhoads", stringsAsFactors = F)
  if(is.null(df$firstname) | is.null(df$lastname)) df %<>% dplyr::bind_rows(., fldf)
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
#' A function
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
#' A function
#' @export
#' @examples
#' dealwith_racegender_variable()
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

#' This function allows you to 
#' @export
#' @examples
#' gather_name()
gather_name <- function(df) {
  df %>%
    tidyr::gather("twastwas", "name", dplyr::matches("name")) %>% 
    dplyr::select(-dplyr::one_of("twastwas")) %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>%
    lapply(stringi::stri_enc_toutf8) %>% 
    data.frame(., stringsAsFactors = F) %>%  
    dplyr::distinct() 
}


#' This function allows you to 
#' @export
#' @examples
#' gather_name_namesplit()
gather_name_namesplit <- function(df) {
  names(df) %<>% gsub("lastname", "ln_orig", .) %>% gsub("firstname", "fn_orig", .)
  df %<>%
    tidyr::gather("twastwas", "name", dplyr::matches("name")) %>% 
    dplyr::select(-dplyr::one_of("twastwas")) %>% 
    # dplyr::filter(!is.na(name), name != "", name != "NA", name != "na", name != " ") %>%
    lapply(stringi::stri_enc_toutf8) %>% 
    data.frame(., stringsAsFactors = F) %>% 
    # namesplit() %>%
    dplyr::distinct() 
  df
}


#' This function allows you to 
#' @export
#' @examples
#' gather_gender()
gather_gender <- function(df) {
  df %>%
    tidyr::gather("twastwas", "gender", dplyr::matches("gender")) %>% 
    dplyr::select(-dplyr::matches("twastwas"), -dplyr::one_of(".id")) %>%
    lapply(stringi::stri_enc_toutf8) %>% 
    data.frame(., stringsAsFactors = F) %>% 
    dplyr::distinct() 
}


#' This function allows you to 
#' @export
#' @examples
#' gather_race()
gather_race <- function(df) {
  df %>%
    tidyr::gather("twastwas", "race", dplyr::matches("race")) %>% 
    dplyr::select(-dplyr::one_of("twastwas"), -dplyr::one_of(".id")) %>%
    lapply(stringi::stri_enc_toutf8) %>% 
    data.frame(., stringsAsFactors = F) %>% 
    dplyr::distinct() 
}


#' This function allows you to 
#' @export
#' @examples
#' gather_race_and_gender()
gather_race_and_gender <- function(df) {
  df <- tidyr::gather(df, "twastwas", "gender", dplyr::matches("gender")) %>% 
    dplyr::select(-dplyr::one_of("twastwas"), -dplyr::one_of(".id"))
  df <- tidyr::gather(df, "twastwas", "race", dplyr::matches("race")) %>% 
    dplyr::select(-dplyr::one_of("twastwas"), -dplyr::one_of(".id"))
  # dplyr::distinct(df)
}
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------

#' This function allows you to 
#' @export
#' @examples
#' gather_join_first_last_name()
gather_join_first_last_name <- function(df, seq = c("first last", "last,first")) {
  seq <- match.arg(seq)
  df <- gather_first_last_name(df)
  df <- join_firstlastname(df, df$firstname, df$lastname, seq = seq)
  df <- gather_name(df) 
  dplyr::distinct(df)
}


#' This function allows you to 
#' @export
#' @examples
#' gather_join_first_last_namesplit()
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

#' A function to turn a vector of values into a list to use in recoding values to NAs
#' @export
#' @examples
#' recode_na_vec(vec, recode_list = recode_na_list, extra = NULL, recode_na_getridofstrregex=NULL)
recode_na_vec <- function(vec, recode_list = recode_na_list, extra = NULL, recode_na_getridofstrregex=NULL) {
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
  if(!is.null(recode_na_getridofstrregex)){vec <- gsub(recode_na_getridofstrregex, "", vec, perl = T)}
  vec
}

#' A function to recode specified values into NAs
#' @export
#' @examples
#' recode_na_old(x)
recode_na_old <- function(x){
  if(is.data.frame(x) | is.list(x)) x %<>% lapply(., recode_na_vec) %>% data.frame(., stringsAsFactors=F)
  else x %<>% recode_na_vec(x)
  x
}

# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------

#' A function
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

#' A function
#' @export
#' @examples
#' recode_races_regex()
recode_races_regex <- function(df) {
  if(length(dplyr::select(df, dplyr::matches("race|ethnicity|ethni|ancestry"))) > 0){
    racedf <- data.frame(lapply(dplyr::select(df, dplyr::matches("race|ethnicity|ethni|ancestry")), recode_race_regex), stringsAsFactors = F)
    df <- data.frame(dplyr::select(df,-dplyr::matches("race|ethnicity|ethni|ancestry")), racedf, stringsAsFactors = F)
  }
  df
}

# ---------------------------------------------------------------------------------

#' A function to recode gender!
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



# ---------------------------------------------------------------------------------

#' A clean_dfs Function
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
#' A Recode Gender Function: allows you to recode gender!
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
#' A function
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


race_specific <- function() {
  mapply(c, 
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
}

gender_specific <- function() {
  mapply(c, 
         (gender_list_short <- lapply(gender_list_short, sort)), 
         (extra_gender_vals_list <- lapply(
           list("male" = c("MALE", "man", "mannlich", "dude", "guy", "sir", "blackman"), 
                "female" = c("FEMALE", "woman", "girl", "feminine", "women", "blackwoman"),
                "NA" = c("tals", "VALUE", "black", "white", "asian","ormoreraces",
                         "hispanicorlatino", "hispanic or latino","twoormoreraces","2ormoreraces",
                         "black or african american", "blackorafricanamerican","nativehawaiianorpacificisler",
                         "native hawaiian or other pacific islander", "nativehawaiianorotherpacificislander",
                         "american indian or alaska native", "americanindianoralaskanative")), sort)), SIMPLIFY=FALSE)
}

getridofgenderspecific_regex <- function(){ paste0(
  c(names(race_list_short[-length(race_list_short)]), 
    names(race_list_short[-length(race_list_short)]) %>% gsub(" ", "", ., perl = T), 
    unique(unlist(strsplit(names(race_list_short[-length(race_list_short)]), " ")))),
  collapse = "|"
)}
getridofracespecific_regex <- function() {paste0(c("male", "female", "woman", "^man$", "women", 
                                                   "regular"), collapse = "|")}

#' A function
#' @export
#' @examples
#' recode_gender_specific()
recode_gender_specific <- function(vec, extra = NULL) {
  vec <- recode_gender_j(vec, recode_list = gender_specific(), extra = NULL)
  # vec <- gsub("[^[:alpha:]]", "", vec, perl = T)
  vec <- gsub(getridofgenderspecific_regex(), "", vec, perl = T)
  vec <- gsub(getridofgenderspecific_regex(), "", vec, perl = T)
  vec <- gsub("^$", NA, vec, perl = T)
  vec
}

#' A function
#' @export
#' @examples
#' recode_race_specific()
recode_race_specific <- function(vec, extra = NULL) {
  vec <- recode_race_j(vec, recode_list = race_specific(), extra = NULL)
  # vec <- gsub("[^[:alpha:]]", "", vec, perl = T)
  vec <- gsub(getridofracespecific_regex(), "", vec, perl = T)
  vec <- gsub(getridofracespecific_regex(), "", vec, perl = T)
  vec <- gsub("^$", NA, vec, perl = T)
  vec
}




# ---------------------------------------------------------------------------------
#' A function
#' @export
#' @examples
#' multistep_clean_dfs(df, subsets = 2)
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


#' A function
#' @export
#' @examples
#' fix_encoding()
fix_encoding <- function(df, originalEncoding = "latin1") {
  numCols <- ncol(df)
  for (col in 1:numCols)
    if(class(df[, col]) == "character"){
      Encoding(df[, col]) <- originalEncoding
    }
  return(df)
}




#' A function
#' @export
#' @examples
#' combine.lists()
combine.lists <- function(list1, list2){
  list1.names <- names(list1)
  list2.names <- names(list2)
  new.list <- list1
  tmp <- match(list2.names, list1.names)
  w <- which(!is.na(tmp))
  if (length(w) > 0){
    # take values from list2 in matching dimension names
    tmp <- tmp[!is.na(tmp)]
    new.list[[tmp]] <- list2[[w]]
    # append elements of 'list2' with unmatched names
    new.list <- c(new.list, list2[-w])
  }else{
    new.list <- c(new.list, list2)
  } 
  new.list
} # end of combine.lists



#' This function allows you to 
#' @export
#' @examples
#' alnum_period(x)
alnum_period <- function(x){ gsub("[^\\.|[:alnum:]]", "", x)}


#' This function allows you to extract alphanumeric characters from a string
#' @export
#' @examples
#' alnum(x)
alnum <- function(x){ gsub("[^[:alnum:]]", "", x)}

#' Samantha Rhoads's function to extract numeric characters from a string
#' @export
#' @examples
#' extract_digits(x)
extract_digits <- digits <- function(x){gsub("[^[:digit:]]", "", x)}

#' This function allows you to 
#' @export
#' @examples
#' cleanpath(x)
cleanpath <- function(x){
  x %<>% 
    gsub("\\/\\/", "\\/",. ) %>% 
    gsub("__", "_",. ) %>%
    gsub("^\\_|_$|^_|_$|\\<_", "", .) %>%
    gsub("^\\_|\\_$|^_", "", .)
}


#--------------------------------------

#' A function
#' @export
#' @examples
#' round5(num)
round5 <- function(num) {formatC(num, width = 5, format = "d", flag = "0")}

#' A function
#' @export
#' @examples
#' round4(num)
round4 <- function(num) {formatC(num, width = 4, format = "d", flag = "0")}


#' A function to round intuitively based on how many digits the percentage number is + how much space you want to take up
#' @export
#' @examples
#' round_intuitively(x, roundby=2)
round_intuitively <- function(x, roundby=2){
  x <- #ifelse(x < .01, round(x, roundby+1), 
    ifelse(x < .1, round(x, roundby+1), 
           ifelse(x < 1, round(x, roundby),
                  ifelse(x > 1 & x < 10, round(x, max(roundby-1, 1)),
                         round(x))))
  # x <- ifelse(x < 1, round(x, roundby), round(x, roundby-2))
  # if(!is.null(roundby)) x <- round(x, roundby)
  x
}


#' This function allows you to 
#' @export
#' @examples
#' lastname_wmiddle(v)
lastname_wmiddle <- function(v){
  v <- v %>% gsub( ", ", ",", .) %>% 
    gsub( ",.*$", "", .) %>% 
    gsub( "^\\S* ", "", .)
  v
}

#' A function
#' @export
#' @examples
#' firstname_wmiddle(v)
firstname_wmiddle <- function(v){
  v <- v %>% gsub( ", ", ",", .) %>% 
    gsub( "^.*,", "", .) %>% 
    gsub( " \\S*$", "", .)
  v
}

#' A function
#' @export
#' @examples
#' lastname_after_space_when_nocomma(v)
lastname_after_space_when_nocomma <- function(v) ifelse(grepl(" ",v) & !grepl(",",v), stringr::word(v,-1), NA)

#' A function
#' @export
#' @examples
#' firstname_nomiddle(v)
firstname_nomiddle <- function(v){
  v <- v %>% 
    firstname_wmiddle() %>%
    trimws(., which="both") %>% 
    stringr::word(., 1)
  v
}

#' A function
#' @export
#' @examples
#' firstname_postcomma()
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

#' A function
#' @export
#' @examples
#' lastname_precomma()
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

#' A function
#' @export
#' @examples
#' namesplit()
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



#--------------------------------------------------------------


#' A function that makes sample data frames with name, race, gender type columns. This function allows you to make sample dataframes!
#' @export
#' @examples
#' dfsampler(which=c('long', 'short')[1], tibble=F)
dfsampler <- function(which=c('long', 'short')[1], tibble=F){
  if(which=='short') dfincase <- data.frame(name=c('charlene teters', 'sandra sunrising osawa'), 
                                            firstname=c('charlene', 'sandra sunrising'), 
                                            lastname=c('teters', 'osawa'), 
                                            gender=c('female', 'female'), 
                                            race=c('american indian or alaska native', 'american indian or alaska native'), 
                                            stringsAsFactors = F)
  if(which=='long') dfincase <- data.frame(gender = c("male", "female", "female", 
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
  if(tibble) dfincase <- as.tibble(dfincase)
  dfincase
}







#' This function allows you to trim whitespace but also remove double spaces
#' @export
#' @examples
#' trimws_(v, which='both', doublespace=T, newlineseps=T, newlineasspace=F)
trimws_ <- function(v, which='both', doublespace=T, newlineseps=T, newlineasspace=F){
  vlevels <- levels(v)
  vclass <- class(v)
  if(which=="both"){v <- gsub("([[:space:]]| )$|^([[:space:]]| )", "", v)
  } else if(which=="left"){v <- gsub("^([[:space:]]| ){1,100}", "", v)
  } else if(which=="right"){v <- gsub("([[:space:]]| )${1,100}", "", v)}
  if(doublespace) {
    v <- gsub('(   |  ){1,100}', ' ', v)
    v <- gsub('([[:space:]])[[:space:]]){1,100}', '\\1', v)
    if(newlineseps){
      v <- gsub("(\\\r|\\\n|\\\t){1,100}", "\\1", v)
      if(newlineasspace){
        v <- gsub("\\\n|\\\r|\\\t", " ", v)
        v <- gsub("((\\\n|\\\r|\\\t| |[[:space:]])(\\\n|\\\r|\\\t| |[[:space:]])){1,100}", " ", v)
      }
      # v <- gsub("(\\\n|\\\r|\\\t| |[[:space:]])(\\\n|\\\r|\\\t| |[[:space:]]){1,100}", "\\1", v)
      if(which=="both"){v <- gsub("((\\\r|\\\n|\\\t)$|^(\\\r|\\\n|\\\t)){1,100}", "", v)}
    }
  }
  # if(doublespace) {v <- gsub('(   |  |[[:space:]][[:space:]]){1,100}', ' ', v)}
  v <- trimws(v, which=which)
  
  if(!is.null(vlevels)){
    levels(v) <- vlevels
    if(vclass=="factor"){v <- as.factor(v)}
  }
  v
}

#' This function allows you to 
#' @export
#' @examples
#' trimws_v(v, which='both', doublespace=T)
trimws_v <- trimws_


#' This function allows you to trimws on your whole dataframe (but just character and factor variables)
#' @export
#' @examples
#' trimws_df(x, which='both', doublespace=T, on_factor_vars=T)
trimws_df <- function(x, which='both', doublespace=T) {
  fun <- if(on_factor_vars){is.factorchar} else {is.character}
  dplyr::mutate_if(x, fun, function(v) trimws_(v, doublespace=doublespace, which=which))
}

#' A function
#' @export
#' @examples
#' recode_na_if_(x, na_if_Unknown=T)
recode_na_if_ <- function(x, na_if_Unknown=T){ 
  x <- recode_na(x, "", "NA", "-", ".", " ", "na", "/", ",", ";", "  ", "Not Available", "not available", "Not Applicable", "not applicable", "No Response", "NULL", "null", "unknown", "N/A", "n/a", "<NA>", "<N/A>", "Na", "character(0)")
  if(na_if_Unknown){
    x <- recode_na(x, "Unknown", "UNKNOWN", "unknown")
  }
  x
}

#' A function
#' @export
#' @examples
#' na_if_(x, na_if_Unknown=T)
na_if_OLD <- function(x, na_if_Unknown=T){ 
  x %>% na_if('') %>% na_if('NA') %>% {if(na_if_Unknown) na_if(., 'Unknown') else .} %>% na_if('-') %>% 
    na_if('.') %>% na_if(' ') %>% na_if('na') %>% na_if('/') %>% na_if(',') %>% na_if(';') %>% 
    na_if('  ') %>% na_if('Not Available') %>% na_if('not available') %>% na_if('Not Applicable') %>% na_if('not applicable') %>% na_if('No Response') %>% na_if('NULL') %>% na_if('null') %>% 
    na_if('unknown') %>% na_if('N/A') %>% na_if('n/a') %>% na_if('<NA>') %>% na_if('<N/A>') %>% na_if('Na') %>% na_if('') %>% na_if('') %>% na_if('') %>%
    na_if("character(0)") %>% if(is.vector(.)) tryCatch(gsub("^[[:punct:]]$", NA, .), error=function(e) .) else .
}

#' A function
#' @export
#' @examples
#' newdate(v)
newdate <- function(v) lubridate::mdy_hm(v) %>% lubridate::date()

#' A function
#' @export
#' @examples
#' unique_sep(v, sep='; ')
unique_sep <- function(v, sep='; '){
  splitv <- strsplit(v, sep)
  uniqv <- lapply(splitv, unique)
  lapply(uniqv, function(s) paste0(s, collapse=sep)) %>% dplyr::combine() %>% as.character()
}


#' This function allows you to 
#' @export
#' @examples
#' format_initial(v, upper=T)
format_initial <- function(v, upper=T){
  v <- gsub("\\s([[:alpha:]])\\s"," \\1. ", v)
  v <- gsub("^([[:alpha:]])\\s","\\1. ", v)
  v <- gsub("\\s([[:alpha:]])$"," \\1.", v)
  v <- gsub("^([[:alpha:]])$","\\1.", v)
  v <- trimws_(gsub('\\.', '. ', v))
  if(upper) v <- gsub("\\b(\\w)", "\\U\\1", v, perl=T)
  trimws_(v)
}


#' This function allows you to 
#' @export
#' @examples
#' toupper_l1(v)
toupper_l1 <- function(v) gsub("\\b(\\w)", "\\U\\1", v, perl=T)

#' A function
#' @export
#' @examples
#' name_case(v)
name_case <- function(v) toupper_l1(tolower(v))

#' A function
#' @export
#' @examples
#' clean_str(v)
clean_str <- function(v){
  v %>% trimws_() %>% 
    gsub(',', ', ', .) %>%
    trimws_() %>%
    gsub(' \\.$|\\-$|\\_$|\\,$|\\/$', '', .) %>%
    gsub('^\\.|^\\-|^\\_|^\\,|^\\/', '', .) %>%
    # gsub(' \\. ', ' ', .) %>%
    trimws
}

#' A function
#' @export
#' @examples
#' format_pasted_name(name)
format_pasted_name <- function(name){
  name <- trimws_(gsub(' NA|NA ', '', name))
  name <- clean_str(name)
  name <- format_initial(name)
  name <- gsub(' Na ', ' ', name)
  name <- name_case(name)
  name <- trimws_(name)
  name <- unique_sep(name, ' ')
  name
}


#' This function allows you to 
#' @export
#' @examples
#' remove_after_char(v, sep=' ')
remove_after_char <- function(v, sep=' ') gsub(paste0(sep, ".*"),"", v)


#' This function allows you to 
#' @export
#' @examples
#' extract_words_w_numsv()
extract_words_w_nums <- function(v) sapply(stringr::str_extract_all(v, '[A-Za-z]*[0-9]+[A-Za-z]*'), paste, collapse=' ')


#' This function allows you to 
#' @export
#' @examples
#' na_if_all_digits(v)
na_if_all_digits <- function(v) gsub(paste_regex(c(1:2000), exact=T), NA, v, ignore.case = T)


#' This function allows you to 
#' @export
#' @examples
#' join_full(l, type='full')
join_full <- function(l, type='full') plyr::join_all(l, type=type)


#' This function allows you to make a DT datatable with better defaults in my opinion
#' @export
#' @examples
#' datatable_(d)
datatable_ <- function(d){
  DT::datatable(d, rownames=F,
                options = list(pageLength = 5000, 
                               autoWidth=T,
                               dom = 'Bfrtip',
                               autoWidth=T,
                               columnDefs = list(list(width = '10', targets = 2)),
                               scrollX=T, 
                               selection="multiple"))
  
}


#' This function allows you to 
#' @export
#' @examples
#' df_to_vec(df)
df_to_vec <- function(df) as.character(unlist(df))

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' ifelse_to_na_exact(v, na_vec)
ifelse_to_na_exact <- function(v, na_vec) ifelse(tolower(v) %in% tolower(na_vec), NA, v)

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' ifelse_to_na_partial(v, na_vec)
ifelse_to_na_partial <- function(v, na_vec) ifelse(grepl(paste_regex(na_vec), v), NA, v)

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' ifelse_to_na(v, na_vec, exact=T)
ifelse_to_na <- function(v, na_vec, exact=T) if(exact) ifelse_to_na_exact(v, na_vec) else ifelse_to_na_partial(v, na_vec)

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' unlist_as_char(df)
unlist_as_char <- function(df) as.character(unlist(df))

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' sort_df_columns(df)
sort_df_columns <- function(df) df %>% dplyr::select(sort(names(.)))

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' summary_factor()
summary_factor <- function(x, maxsum=7) if(is.data.frame(x)) summary(dplyr::mutate_all(x, as.factor), maxsum) else summary(as.factor(x), maxsum)

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' paste_regex_partial()
paste_regex_partial <- function(v, collapse='|') paste0(v, collapse=collapse)

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' paste_regex_exact()
paste_regex_exact <- function(v, collapse='|') paste0('^', v, '$', collapse=collapse)

#' A function
#'
#' This function allows you to 
#' @export
#' @examples
#' paste_regex(v, collapse='|', exact=F)
paste_regex <- function(v, collapse='|', exact=F){
  if(exact) paste_regex_exact(v)
  else paste_regex_partial(v)
}


#' This function allows you to 
#' @export
#' @examples
#' strip_num_trimws(v)
strip_num_trimws <- function(v) trimws_(gsub('[[:digit:]]+', '', v))


#' This function allows you to 
#' @export
#' @examples
#' clean_na_sep(v, sep='///')
clean_na_sep <- function(v, sep='///') trimws_(gsub('///NA|NA///|^///|///$|^///|///$|^\\///|\\///$', '', trimws_(v), perl=T)) %>% 
  gsub(' ///|/// |//////', '///', ., perl=T) %>% 
  trimws_() %>% na_if_()


#' This function allows you to 
#' @export
#' @examples
#' clean_na_sep_comma(v, sep=', ')
clean_na_sep_comma <- function(v, sep=', ') trimws_(gsub(', NA|NA, |^, |, $|^, |, $|^\\, |\\, $', '', trimws_(v), perl=T)) %>% 
  gsub(' , |,  |, , ', ', ', ., perl=T) %>% 
  trimws_() %>% na_if_()


#' This function allows you to 
#' @export
#' @examples
#' clean_unique_sep(v, sep='///')
clean_unique_sep <- function(v, sep='///'){ 
  # trimws_(gsub('///NA|NA///|^///|///$|^///|///$|^\\///|\\///$', '', v, perl=T)) %>% gsub(' ///|/// |//////', '///', ., perl=T) %>% 
  remove_str <- paste0('(^|', sep, ')', 'NA($|', sep, ')')
  v_ <- gsub(remove_str, sep, v, perl=T)
  trim_sep_str <- paste0('^', sep, '|', sep, '$', '|', sep, sep)
  v_ <- gsub(trim_sep_str, '', v_, perl=T)
  trimws_(v_) %>% 
    unique_sep(., sep=sep) %>% na_if_()
}


#' This function allows you to 
#' @export
#' @examples
#' unique_sep_strip_num_clean()
unique_sep_strip_num_clean <- function(v, sep='///') clean_unique_sep(unique_sep(strip_num_trimws(v), sep=sep), sep=sep)

#' A function
#' @export
#' @examples
#' drop_rows_all_na()
drop_rows_all_na <- function(x, pct=1) x[!rowSums(is.na(x)) >= ncol(x)*pct,]

#' A function
#' @export
#' @examples
#' nna(x)
nna <- function(x) sum(is.na(x))

#' A function
#' @export
#' @examples
#' dimnna()
dimnna <- nnadim <- function(d) paste0("nNA: ", nna(d), "  |  dim: ", paste0(dim(d), collapse=' x '), collapse=' ')

#' A function
#' @export
#' @examples
#' nnadimsum()
nnadimsum <- dimsumnna <- sumdimnna <- dimnnasum <- sumnnadim <- function(d, alpha=F){ 
  if(alpha) print(summary_factor(d %>% sort_df_columns(), 1)) else print(summary_factor(d, 1))
  cat(paste0("\n\nnNA: ", nna(d), "\n nrow: ", paste0(dim(d), collapse=' \n ncol: '), collapse=' '))
}


# June 6, 2019 (06062019) _ ########################################################################################################################


#' A function to get the name of an object to use for other stuff. Ie: get the names of the object/data and make it a variable in the data. 
#' This function allows you to get the name of an object to use for other stuff. Ie: get the names of the object/data and make it a variable in the data. 
#' Idea derived from: https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function about: In R, how to get an object's name after it is sent to a function?
#' @export
#' @examples
#' Using `returnname()` to get name of dataframe, then making a column in the df the name of the dataframe
#' returnname(mtcars) # returns "mtcars"
#' returnname(z)
returnname <- function(z){
  mean.x <- mean(z$x)
  nm <- deparse(substitute(z))
  return(nm)
}

#' A function
#' @export
#' @examples
#' lsetdiff()
lsetdiff <- function(l1, l2){ l1[!(l1 %in% l2)] } # same as identical() i believe

#' A function
#' @export
#' @examples
#' lapply2(l, fxn)
lapply2 <- function(l, fxn){ lapply(l, function(ll) lapply(ll, fxn))}

#' A function
#' @export
#' @examples
#' filter_list(l, is)
filter_list <- filter_l <- function(l, is){
  l_is <- lapply(l, is)
  l[l_is==T]
}

#' A function
#' @export
#' @examples
#' collapse_obj_tostr(x)
collapse_obj_tostr <- function(x){ 
  x %>% lapply(., function(x){ 
    x %>% unlist() %>% capture.output() %>% paste0(., collapse="    \n    ")
  })
}


#' A function
#' @export
#' @examples
#' read_excels(filelist, bindsheets = F, bindrows = F, simplif = F, col_types = "text")
read_excels <- function(filelist, bindsheets = F, bindrows = F, simplif = F, col_types = "text") {
  d <- lapply(filelist, function(x) try_read_excel(x, bindsheets = bindsheets, col_types = col_types))
  if (simplif) d <- try_combine_compact(d) %>% drop_empty()
  if (bindrows) d <- dplyr::bind_rows(d)
  d %>% setNames(filelist)
}




#' #' A function to parse an excel dates: this function allows you to parse an excel dates (one of those with 5 digits as a string)
#' #' @export
#' #' @examples
#' #' parse_excel_date(v, include_time=F)
parse_excel_date <- function(v, include_time=F){
  if(!lubridate::is.Date(v)){
    tryCatch(v %>% as.character() %>% as.numeric() %>% as.Date(., origin = "1899-12-30"),
             error=function(e){
               tryCatch(janitor::excel_numeric_to_date(as.numeric(v), include_time=include_time),
                        error=function(e){
                          tryCatch(as.date.varioustypes(v, include_time=include_time),
                                   error=function(e){
                                     cat("\nCAN'T PARSE EXCEL DATE... LEAVING AS IS....\n")
                                     v
                                   })
                        })
             })
  }
}



#' A function to read multiple excel files and either all or selective sheets from them.
#' @export
#' @examples
#' read_excel_somesheets(fns=NULL, keepshtvec=NULL, na=c("NA", "None", "N/A", "-", ""), col_types="text", skip=0, col_names=T, range=NULL, trim_ws=T, n_max=Inf, guess_max=min(1000, n_max), progress=readxl::readxl_progress(), .name_repair="unique")
read_excel_somesheets <- function(fns=NULL, keepshtvec=NULL, na=c("NA", "None", "N/A", "-", ""), col_types="text", skip=0, col_names=T, range=NULL, trim_ws=T, n_max=Inf, guess_max=min(1000, n_max), progress=readxl::readxl_progress(), .name_repair="unique") {
  if (is.null(fns)) {(fns <- list.files(pattern = "\\.xlsx", recursive = T, full.names = T))}
  if (is.null(keepshtvec)) {keepshtvec <- lapply(fns, function(v) readxl::excel_sheets(v)) %>% unlist() %>% unique()}
  fnshtlst <- lapply(fns, function(s) readxl::excel_sheets(s)) %>% setNames(fns)
  if(is.numeric(keepshtvec)){
    keepshts <- as.list(fns) %>% setNames(fns) %>% lapply(., function(x) {keepshtvec})
  } else {
    (keepshts <- lapply(fnshtlst, function(v) {v[(v %in% keepshtvec) == T]}))
  }
  lapply(1:length(keepshts), function(i) {
    f <- names(keepshts[i])
    shts <- keepshts[[i]]
    (d <- lapply(shts, function(sht) {readxl::read_excel(f, sheet=sht, skip=skip, na=na, col_types=col_types, col_names=col_names, range=range, trim_ws=trim_ws, n_max=n_max, guess_max=guess_max, .name_repair=.name_repair, progress=progress)}) %>% setNames(shts))
  }) %>% setNames(fns)
}

#' A function
#' @export
#' @examples
#' depth(this,thisdepth=0)
depth <- function(this,thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  }else{
    return(max(unlist(lapply(this,depth,thisdepth=thisdepth+1))))    
  }
}


########################################################################################################################

# JUNE 10, 2019 (06102019)

#' A function
#' @export
#' @examples
#' lookslike_number(v, include_decimal=F, include_comma=F, include_dash=F, include_space=F, include_all_punct=F)
lookslike_number <- function (v, include_decimal=F, include_comma=F, include_dash=F, include_space=F, include_all_punct=F) {#{v=c("-1", "2.4", "sfs 242.1", "4.875e-05")}
  vb <- suppressWarnings(as.numeric(v))
  v <- ifelse(!is.na(vb), 1, "NOT_A_NUMBER")
  
  if (include_decimal) {
    v <- gsub("[\\.|[:digit:]]", "", v) %>% na_if(., "")
  } else {
    v <- gsub("[[:digit:]]", "", v) %>% na_if(., "")
  }
  if(include_comma){v <- gsub(",", "", v) %>% na_if(., "")}
  if(include_dash){v <- gsub("-", "", v) %>% na_if(., "")}
  if(include_space){v <- gsub("[ |[:space:]]", "", v) %>% na_if(., "")}
  if(include_all_punct){
    v <- gsub("[\\(|\\)|[:punct:]]", "", v) %>% na_if(., "")
  }
  ifelse(is.na(v), T, F)
}

#' A function
#' @export
#' @examples
#' str_has_upper(v)
str_has_upper <- function(v) grepl('[[:upper:]]', v)

#' A function
#' @export
#' @examples
#' is.upper(v)
is.upper <- function(v) grepl('[[:upper:]]', v) & !grepl('[[:lower:]]', v)

#' A function
#' @export
#' @examples
#' is.lower()
is.lower <- function(v) grepl('[[:lower:]]', v) & !grepl('[[:upper:]]', v)

#' A function to determine if a string or vector is of date type
#' @export
#' @examples
#' is_datetype1(v)
is_datetype1 <- function(v) {ifelse(is.na(lubridate::parse_date_time(v, orders = c("mdy", "dmy"))), F, T)}

#' A function to manipulate a string or vector into date format
#' @export
#' @examples
#' as.date.varioustypes(v, include_time=F)
as.date.varioustypes <- function(v, include_time=F){
  if(!include_time){
    v <- v %>% unlist() %>%
      gsub("\\..*", " ", .)
  } 
  v %>%
    trimws_() %>%
    ifelse(is_datetype1(.), as.character(lubridate::parse_date_time(., orders = c("mdy", "dmy"))), .) %>%
    ifelse(lookslike_number(.), as.character(janitor::excel_numeric_to_date(as.numeric(.), include_time=include_time)), .)
}

#' A function to change state names to abbreviations if there are already abbreviation in the vector, so mixed types
#' @export
#' @examples
#' state.abb_ifelse(statevec)
state.abb_ifelse <- function(statevec) ifelse(is.na(state.abb[match(tolower(statevec), tolower(state.name))]), statevec, state.abb[match(tolower(statevec), tolower(state.name))])


########################################################################################################################






########################################################################################################################
# June 26, 2019 (06/26/2019) ---------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' summary_factor_2deep()
summary_factor_2deep <- function (x, maxsum = 7){ 
  if (depth(x)==1 & is.data.frame(x)) summary(dplyr::mutate_all(x, as.factor), maxsum) 
  else if (depth(x)==0 & is.vector(x)) summary(as.factor(x), maxsum)
  else tryCatch(lapply(x, function(xx) summary(lapply(xx, as.factor))), error=function(e) summary(x))
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' char_as_factor()
char_as_factor <- function(v) if(is.character(v)) as.factor(v) else v

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' sumry_2deep()
sumry_2deep <- function (x, maxsum = 7){ 
  if (depth(x)==1 & is.data.frame(x)) summary(dplyr::mutate_if(x, is.factorchar, as.factor), maxsum) 
  else if (depth(x)==0 & is.vector(x)) summary(char_as_factor(x), maxsum)
  else tryCatch(lapply(x, function(xx) summary(lapply(xx, char_as_factor))), error=function(e) summary(x))
}


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' recode_diversitygroup()
recode_diversitygroup <- function(v){
  v %>% tolower() %>%
    recode(., 
           '1' = "White",
           '2' = "Black",
           '3' = "Hispanic",
           '4' = "Asian",
           '5' = "American Indian",
           '6' = "Native Hawaiian/Pacific Islander",
           '7' = "Two or More Races",
           'f' = "Female",
           'm' = "Male")
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' summary_factor()
summary_factor <- function(x, maxsum=7){
  tryCatch(summary_factor_2deep(x), error=function(e){
    if(depth(x)==2){lapply(x, summary_factor_2deep)}
  })
}

#' Samantha Rhoads's function to summarize an object in R, but in a more meaningful way than the summary() function
#' @export
#' @examples
#' sumry(x, maxsum=7)
sumry <- function(x, maxsum=7, maxmin=F) {
  if (is.data.frame(x)) {
    # x[["____________"]] <- paste0("Total Columns Summarized:", ncol(x), "; Total # of Rows")
    x[["____________"]] <- paste0("Total # of Rows")
    x[["________________"]] <- ncol(x)
    # x <- dplyr::select(x, one_of("____________"), everything())
    if(all(is.na(maxsum))){
      maxsum <- nrow(x)
    }
    # x_sumry <- summary(dplyr::mutate_if(x, is.character, as.factor), maxsum) 
    x2 <- dplyr::mutate_if(x, is.character, as.factor)
    if(maxmin){
      # x2 <- x2 %>% sapply(., function(v){if(is.factor(v)){c( v[v %in% sort(v)[1:ceiling(maxsum/2)]],   v[v %in% rev(sort(v))[1:floor(maxsum/2)]] )} else {v} })
      # x2 <- x2 %>% mutate_all(., function(v){if(is.factor(v)){c( v[v %in% sort(v)[1:ceiling(maxsum/2)]],   v[v %in% rev(sort(v))[1:floor(maxsum/2)]], v[!v %in% c(sort(v)[1:ceiling(maxsum/2)], rev(sort(v))[1:floor(maxsum/2)])  ] )} else {v} })
      x2 <- x2 %>% mutate_if(is.factor, function(v){
        if(length(unique(v))>=maxsum){
          v0 <- as.character(v)
          v1 <- as.factor(ifelse(v0 %in% c( v[v %in% sort(unique(v))[1:floor((maxsum-1)/2)]],   v[v %in% rev(sort(unique(v)))[1:floor((maxsum-1)/2)]] ), v0, "~~~other values~~~") )
        } else {
          v1 <- v
        }
        v1
      })
    }
    
    x_sumry <- summary(x2, maxsum)
    
    x_sumry[[(length(x_sumry)-6)]] <- gsub("Min\\..*\\:", "Total # of Columns:", x_sumry[[(length(x_sumry)-6)]])
    for(i in c((length(x_sumry)-5):(length(x_sumry)-1))){
      x_sumry[[i]] <- ""
    }

    x[["____________"]] <- NULL
    x[["________________"]] <- NULL
    return(x_sumry)
  } else if(is.vector(x)){
    
    v <- {if(is.character(x)) {as.factor(x)} else {x}}
    if(maxmin){
      v <- x
      if(length(unique(v))>=maxsum){
        v0 <- as.character(v)
        v1 <- as.factor(ifelse(v0 %in% c( v[v %in% sort(unique(v))[1:floor((maxsum-1)/2)]],   v[v %in% rev(sort(unique(v)))[1:floor((maxsum-1)/2)]] ), v0, "~~~other values~~~") )
      } else {
        v1 <- v
      }
      
    } else {
      v1 <- v
    }
    
    x_sumry <- summary(v1, maxsum)
    
  } else {
    x_sumry <- summary(x, maxsum)
  }
  return(x_sumry)
}




#' Samantha Rhoads's function to...
#' @export
#' @examples
#' read_excel_all()
read_excel_all <- function(fns){
  fns <- list.files('original', full.names = T)
  sheets <- lapply(fns, function(f) readxl::excel_sheets(f)) %>% setNames(fns)
  lapply(1:length(sheets), function(d) {
    # dfs <- readxl::read_excel(names(sheets)[d])
    lapply(sheets[[d]], 
           function(sheet) readxl::read_excel(names(sheets)[d], 
                                              sheet, #skip = skip, 
                                              na = c("NA", "None", "N/A", "-", "")#, col_types = col_types
           )) %>%
      setNames(sheets[[d]]) 
  }) %>%
    setNames(names(sheets))
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' clean_money_as_numeric()
clean_money_as_numeric <- function(v) as.numeric(gsub('[^\\.|[:digit:]]', '', v))


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' not_is.character()
not_is.character <- function(v) !is.character(v)


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' not_is.factorchar()
not_is.factorchar <- function(v) !is.character(v) & !is.factor(v)


########################################################################################################################
# 07/02/2019 # July, 2, 2019


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' strip_punct()
strip_punct <- function(v, replacewithspace=T, replacewith=NULL, onlyends=F) {
  if(replacewithspace) replacement <- " " else replacement <- ""
  if(!is.null(replacewith)) replacement <- replacewith
  if(onlyends) v <- gsub('^[[:punct:] ]+|[[:punct:]]+$', replacement, v) else v <- gsub('[[:punct:] ]+', replacement, v)
  trimws_(v)
}
# strip_punct(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct", "this ; .punct", "this . ;punct", "this . .punct", "this . .punct"))

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' remove_duplicate_punct_separated_by_space()
remove_duplicate_punct_separated_by_space <- function(v) gsub("([[:punct:]]) \\1", "\\1", v)
# remove_duplicate_punct_separated_by_space(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct"))

#' Samantha Rhoads's function to...
#'
#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' remove_duplicate_punct_separated_by_space_or_not()
remove_duplicate_punct_separated_by_space_or_not <- function(v) gsub("([[:punct:]]) \\1", "\\1", v) %>% gsub("([[:punct:]])\\1", "\\1", .) %>% trimws_()
# remove_duplicate_punct_separated_by_space_or_not(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct",  "this ; .punct", "this . ;punct", "this . .punct", "this . .punct"))

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' remove_duplicate_punct()
remove_duplicate_punct <- function(v, nrepeat=6){
  sum <- 0
  repeat{
    sum <- sum + 1
    v <- remove_duplicate_punct_separated_by_space_or_not(v)
    if(sum==nrepeat) break
  }
  v
}
# remove_duplicate_punct(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct", "this ; .punct", "this . ;punct", "this . .punct", "this . .punct"))

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' add_space_between_punct()
add_space_between_punct <- function(v) gsub("([[:punct:]])", " \\1 ", v) #"([[:punct:]]+)"
# add_space_between_punct(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct")) #%>% strsplit(., "[[:punct:]]")

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' add_space_after_punct()
add_space_after_punct <- function(v) gsub("([[:punct:]])", "\\1 ", v) %>% trimws_() #"([[:punct:]]+)" : use that if u dont wanna make space btwn puncts themselves (ie: '..' not to '. . ')
# add_space_after_punct(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct", "this;.punct"))

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' remove_duplicate_punct_consec()
remove_duplicate_punct_consec <- function(v) gsub("([[:punct:]])\\1+", "\\1", v)
# remove_duplicate_punct_consec(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct", "this;.punct"))

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' remove_space_btwn_identical_punct()
remove_space_btwn_identical_punct <- function(v) v %>%
  strsplit(., "(?<=[[:punct:]])", perl=T) %>% 
  lapply(.,function(s) s %>% trimws_() %>% 
           paste0(., collapse="")) %>%
  unlist() %>% add_space_after_punct() %>% 
  trimws_() %>%
  gsub("([[:punct:]]) \\1", "\\1\\1", .)
# remove_space_btwn_identical_punct(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct", "this;.punct"))

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' remove_space_btwn_any_punct()
remove_space_btwn_any_punct <- function(v) v %>%
  strsplit(., "(?<=[[:punct:]])", perl=T) %>% 
  lapply(.,function(s) s %>% trimws_() %>% 
           paste0(., collapse="")) %>%
  unlist() %>% add_space_after_punct() %>% 
  trimws_() %>%
  gsub("([[:punct:]]) ([[:punct:]])", "\\1\\2", .)
# remove_space_btwn_any_punct(c("this..punct", "this . . punct", "this. .punct", "this. . punct", "this . .punct", "this;.punct", "this; .punct"))

#' Srhoads wrote this to allow you to clean up the punctuation in a string or vector of strings
#' @export
#' @examples
#' trimpunct(v, removenegativesymbol=F, beginning=T, end=T, removeduplicatepunct=T)
trimpunct <- function(v, removenegativesymbol=F, beginning=T, end=T, removeduplicatepunct=T){
  v <- trimws_(v)
  if(!removenegativesymbol) v <- gsub("^-", "samhasanegativesymbolherenowand", v)
  if(beginning) v <- gsub('^[[:punct:] ]+', "", v) 
  if(end) v <- gsub('[[:punct:]]+$', "", v)
  v <- trimws_(gsub("^samhasanegativesymbolherenowand", "-", v))
  if(removeduplicatepunct){
    v <- remove_duplicate_punct(v)
  }
  return(v)
}
# trimpunct(c("s..p", "s . . p", "s. .p", "s. . p", "s . .p", "s;.p", "s; .p", ",.s..p''", ",.s . . p''", ",.s. .p''", ",.s. . p''", ",.s . .p''", ",.s;.p''", ",.s; .p''"))

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' trimpunct_(v, removenegativesymbol=F, beginning=T, end=T)
trimpunct_ <- function(v, removenegativesymbol=F, beginning=T, end=T){
  v %>% trimpunct(removenegativesymbol=removenegativesymbol, beginning=beginning, end=end) %>%
    remove_duplicate_punct() %>% trimws_()
}


#' Srhoads wrote this to allow you to clean punctuation in a string or vector of strings
#' @export
#' @examples
#' clean_punct()
clean_punct <- function(v, removenegativesymbol=F, beginning=T, end=T){
  v <- trimws_(v)
  if(!removenegativesymbol) v <- gsub("^-", "samhasanegativesymbolherenowand", v)
  v <- gsub("^\\(", "samhasopenparenthesisherenowand", v)
  v <- gsub("\\)$", "samhascloseparenthesisherenowand", v)
  if(beginning) v <- gsub('^[[:punct:] ]+', "", v) 
  if(end) v <- gsub('[[:punct:]]+$', "", v)
  v <- gsub('samhasopenparenthesisherenowand', '(', v)
  v <- gsub('samhascloseparenthesisherenowand', ')', v)
  v <- trimws_(gsub("^samhasanegativesymbolherenowand", "-", v))
  v <- remove_duplicate_punct(v)
  return(v)
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' remove_space_btwn_identical_punct()
remove_space_btwn_identical_punct <- function(v) v %>%
  gsub("([[:punct:]]) \\1", "\\1\\1", .)
# remove_space_btwn_identical_punct(c("S;'P / /", "s..p", "s . . p", "s. .p", "s. . p", "s . .p", "s;.p", "s; .p", ",.s..p''", ",.s . . p''", ",.s. .p''", ",.s. . p''", ",.s . .p''", ",.s;.p''", ",.s; .p''"))


#' Samantha Rhoads's function to...
#'
#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' add_space_btwn_lower_upper_letters()
add_space_btwn_lower_upper_letters <- function(v) gsub("([a-z])([A-Z])", "\\1 \\2", v)

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' separate_lower_upper_letters()
separate_lower_upper_letters <- add_space_btwn_lower_upper_letters
# add_space_btwn_lower_upper_letters(c("AppleBottomJeansAndCats"))

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' sort_str_by_alpha()
sort_str_by_alpha <- function(v, desc=F){
  if(!desc) rev <- function(v) v
  strsplit(v, "; ") %>%
    lapply(., function(s) rev(sort(s)) %>% 
             paste0(., collapse="; ")) %>% unlist()
}

#' Srhoads wrote this to allow you to sort a string, separated by a given separator, by number of characters
#' @export
#' @examples
#' sort_str_by_nchar(v, desc = T, sep="; ")
sort_str_by_nchar <- function (v, desc = T, sep="; ") {
  if (!desc) {rev <- function(v) v}
  strsplit(v, sep) %>% lapply(., function(s) s[rev(order(nchar(s), s))] %>% paste0(., collapse = sep)) %>% unlist()
}

#' Srhoads wrote this to allow you to sort a vector by number of characters
#' @export
#' @examples
#' sort_str_by_nchar(v, desc = T)
sort_vec_by_nchar <- function(v, desc = T){
  if (!desc) {rev <- function(v) v}
  v[rev(order(nchar(v), v))]
}

clean_str_strip_NAs_1 <- function(v, sep=", "){
  if(is.null(sep)) return(v)
  # (sep1 <- gsub(" ", "", sep))
  # (v <- gsub(paste0(sep, " "), sep, v))
  # (getrid <- paste0("^NA", sep1, "|", sep1, "NA$", "|", sep1, "NA", sep1) %>% gsub("\\|\\|", "|", .) %>% gsub("\\| \\|", "|", .))
  # v %>% gsub(sep, sep1, .) %>% gsub(getrid, "", .) %>% trimws_() %>% gsub(paste0(sep1, "$", "|", "^", sep1), "", .)
  (v <- gsub(paste0(sep, " "), sep, v) %>% trimws_())
  (sep1 <- gsub(" ", "", sep))
  (getrid <- paste0("^NA", sep1, "|", sep1, "NA$", "|", sep1, "NA", sep1, "|", sep, "NA", sep, "|", " NA$|^NA "))
  v %>% gsub(sep, sep1, .) %>% gsub(getrid, sep, .) %>% trimws_() %>% gsub(paste0(sep1, "$", "|", "^", sep1), "", .) %>% trimws_() %>%
    gsub(getrid, sep, .) %>% trimws_() %>% gsub(paste0("^", sep, "|^", sep1), "", .) %>% trimws_() %>% na_if_()
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' clean_str_strip_NAs(v, sep=", ", sep2=NULL, sep3=NULL)
clean_str_strip_NAs <- function(v, sep=", ", sep2=NULL, sep3=NULL){
  clean_str_strip_NAs_1(v, sep=sep) %>% clean_str_strip_NAs_1(., sep=sep2) %>% clean_str_strip_NAs_1(., sep=sep3)
}
# clean_str_strip_NAs(c("NA; meow; NA", " NA; NA ", " NA,NA", "NA; NA", "NA; NA; NA; NA", "NA,NA,NA,NOPE,NA"), sep=", ", sep2="; ", sep3=";")


#' Samantha Rhoads's function to...
#' @export
#' @examples
#' base_breaks(n = 10)
base_breaks <- function(n = 10) function(x) {
  grDevices::axisTicks(log10(range(x, na.rm=T)), log=T, n=n)
}

#' Samantha Rhoads's function to...
#' @export
#' @examples
#' grepval(pattern, x, ignore.case=F, perl=F, value = T, fixed=F, useBytes=F, invert=F)
grepval <- function (pattern, x, ignore.case=F, perl=F, value = T, fixed=F, useBytes=F, invert=F) {
  if (!is.character(x)){ 
    x <- structure(as.character(x), names=names(x))
  }
  .Internal(grep(as.character(pattern), x, ignore.case, value, 
                 perl, fixed, useBytes, invert))
}

#' Samantha Rhoads's function to...
#' @export
#' @examples
#' unite_all(d, clean=T, remove=F, newcol="unite_all_column", onlynewcol=F, sep="; ")
unite_all <- function(d, clean=T, remove=F, newcol="unite_all_column", onlynewcol=F, sep="; "){
  d <- d %>% unite(., unite_all_column, 1:ncol(.), sep=sep, remove=remove) #%>% #.[[1]] %>% 
  # clean_str() %>% clean_unique_sep(., "; ") #%>% 
  if(clean) d <- d %>%
      dplyr::mutate(unite_all_column = unite_all_column %>% 
                      clean_str() %>% 
                      clean_unique_sep(., "; ") %>% 
                      clean_unique_sep(., ";") %>% 
                      clean_str_strip_NAs(., ";") %>% 
                      clean_str() %>% 
                      strip_punct(., onlyends=T, replacewithspace=F) %>% 
                      na_if_() %>%
                      gsub(trimws_(sep), paste0(sep, " "), .) %>% 
                      trimws_())
  d <- d %>% setNames(gsub("^unite_all_column$", newcol, names(.)))
  if(onlynewcol) d[[newcol]] else d
}


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' unite_if(d, fun=is.factorchar, clean=T, remove=F, newcol="unite_all_column", onlynewcol=F, sep="; ")
unite_if <- function(d, fun=is.factorchar, clean=T, remove=F, newcol="unite_all_column", onlynewcol=F, sep="; "){
  d %>% select_if(fun) %>%
    unite_all(., clean=clean, remove=remove, newcol=newcol, onlynewcol=onlynewcol) %>% #.[[1]] %>% 
    cbind(dplyr::select_if(d, function(v) !fun(v)), .)
}


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' unite_at(d, fun = newcol, clean = T, remove = F, newcol = "unite_all_column", onlynewcol = F, sep = "; ")
unite_at <- function (d, fun = newcol, clean = T, remove = F, newcol = "unite_all_column", onlynewcol = F, sep = "; ") {
  d %>% dplyr::select(fun) %>% unite_all(., clean = clean, remove = remove, 
                                         newcol = newcol, onlynewcol = onlynewcol, sep=sep) %>% cbind(select(d, -fun), .)
}


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' mutate_listname_to_lod_column(LoD, new_colname_for_listname="LISTNAME")
mutate_listname_to_lod_column <- function(LoD, new_colname_for_listname="LISTNAME"){
  namesLoD <- names(LoD)
  LoD %>%
    {
      L <- .
      L <- purrr::map(1:length(L), function(i){ # i <- 1
        LISTNAME <- names(L)[i]
        L[[i]][[new_colname_for_listname]] <- LISTNAME
        L[[i]]
      })
      L
    } %>%
    setNames(namesLoD)
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' datatable2(x, vars = NULL, opts = NULL, caption=NULL, extensions = list(), ...)
datatable2 <- function(x, vars = NULL, opts = NULL, caption=NULL, extensions = list(), ...) {
  
  names_x <- names(x)
  if (is.null(vars)) stop("'vars' must be specified!")
  pos <- match(vars, names_x)
  if (any(purrr::map_chr(x[, pos], typeof) == "list"))
    stop("list columns are not supported in datatable2()")
  
  pos <- pos[pos <= ncol(x)] + 1
  rownames(x) <- NULL
  if (nrow(x) > 0) x <- cbind(' ' = '&oplus;', x)
  
  # options
  opts <- c(
    opts, 
    list(
      columnDefs = list(
        list(visible=F, targets = c(0, pos)),
        list(orderable=F, className = 'details-control', targets = 1),
        list(className = 'dt-left', targets = 1:3),
        list(className = 'dt-right', targets = 4:ncol(x))
      )
    ))
  
  datatable(
    x, 
    ...,
    # escape = -2,
    escape = F,
    extensions = extensions,
    caption = caption,
    options = opts,
    callback = JS(.callback2(x = x, pos = c(0, pos)))
  )
}

#' Samantha Rhoads's function to...
#' @export
#' @examples
#' .callback2()
.callback2 <- function(x, pos = NULL) {
  
  part1 <- "table.column(1).nodes().to$().css({cursor: 'pointer'});"
  
  part2 <- .child_row_table2(x, pos = pos)
  
  part3 <- 
    "
  table.on('click', 'td.details-control', function() {
  var td = $(this), row = table.row(td.closest('tr'));
  if (row.child.isShown()) {
  row.child.hide();
  td.html('&oplus;');
  } else {
  row.child(format(row.data())).show();
  td.html('&ominus;');
  }
  });"
  
  paste(part1, part2, part3)
} 

.child_row_table2 <- function(x, pos = NULL) {
  
  names_x <- paste0(names(x), ":")
  text <- "
  var format = function(d) {
  text = '<div><table >' + 
  "
  pkg('glue')
  for (i in seq_along(pos)) {
    # text <- paste(text, glue::glue(
    text <- paste(text, glue::glue(
      "'<tr>' +
      '<td>' + '{names_x[pos[i]]}' + '</td>' +
      '<td>' + d[{pos[i]}] + '</td>' +
      '</tr>' + " ))
  }
  
  paste0(text,
         "'</table></div>'
         return text;};"
  )
}


#' Srhoads wrote this to allow you to...
#' @export
#' @examples {geocode_by_cell('1156 Susan Way Sunnyvale, CA 94087')}; {geocode_by_cell('Stanford')}
#' geocode_by_cell()
geocode_by_cell <- function(v, replacewith=NULL) lapply(v, function(s){
  ## ggmap VERSION REQUIRED (2.6.1):
  # {install.packages("https://cran.r-project.org/src/contrib/Archive/ggmap/ggmap_2.6.1.tar.gz", type="source", repos=NULL); .rs.restartR(); library(ggmap)}
  if(is.null(replacewith)) replacewith <- s
  # tryCatch(geocode(s, source="dsk") %>% unlist() %>% paste0(., collapse=", ") %>% clean_str_strip_NAs() %>% gsub(",", ", ", .) %>% trimws_(), error=function(e) s)
  tryCatch(ggmap::geocode(s, source="dsk") %>% unlist() %>% paste0(., collapse=", ") %>% clean_unique_sep(., ", ") %>% na_if_(), error=function(e) {cat('\n#geocode_by_cell() ERROR:\n'); print(e); replacewith})
}) %>% unlist()


#' Samantha Rhoads's function to...
#' @export
#' @examples
#' collapse_tiered_vec(v, collapse = "; ", unique_sep=F)
collapse_tiered_vec <- function (v, collapse = "; ", unique_sep=F){
  lapply(v, function(s){
    if(unique_sep & length(s)>1){
      s <- unique(s)
      # print(s)
    }
    paste0(s, collapse=collapse)
  }) %>% unlist()
}


#' Samantha Rhoads's function to...
#' @export
#' @examples
#' str_extract_zip(v, concat=T, collapse="; ", unique_sep=T)
str_extract_zip <- function (v, concat=T, collapse="; ", unique_sep=T) {
  vzips <- regmatches(v, gregexpr("[0-9]{5}(-[0-9]{4})?(?!.*[0-9]{5}(-[0-9]{4})?)", v, perl = TRUE))
  if (concat) {collapse_tiered_vec(vzips, collapse=collapse, unique_sep=unique_sep)} else {vzips}
}

#' Samantha Rhoads's function to...
#' @export
#' @examples
#' extract_word_startswith_dollarsign(v)
extract_word_startswith_dollarsign <- function(v) {v %>% strsplit(" ") %>% sapply(., function(s) grep("\\$", s, value=T)[1]) %>% dplyr::combine()}



#' Samantha Rhoads's function to return either a HOLISTIC dataframe of state names and their abbreviations or a vector of just states or just abbreviations
#' @export
#' @examples
#' get_states(return_which=c('both', 'name', 'abb')[1])
get_states <- function(return_which=c('both', 'name', 'abb', 'fips')[1]){
  state_names_abbs_df <- data.frame(
    state_names = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                    "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                    "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                    "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", 
                    "District of Columbia", "American Samoa","Federated States of Micronesia","Guam","Marshall Islands","Northern Mariana Islands","Palau","Puerto Rico","US Minor Outlying Islands","US Virgin Islands"),
    state_abbs = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                   "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                   "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
                   "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
                   "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                   "DC", "AS","FM","GU","MH","MP","PW","PR","UM","VI"),
    state_fips = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", "15", "16", 
                   "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", 
                   "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", 
                   "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", 
                   "51", "53", "54", "55", "56", 
                   "11", "60","64","66","68","69","70","72","74","78")
  )
  if(grepl('both|name.*abb|abb.*name|all|total|every', return_which, ignore.case=T)){
    return(state_names_abbs_df)
  } else if(grepl('name', return_which, ignore.case=T)){
    return(state_names_abbs_df$state_names)
  } else if(grepl('abb', return_which, ignore.case=T)){
    return(state_names_abbs_df$state_abbs)
  } else if(grepl('fips', return_which, ignore.case=T)){
    return(state_names_abbs_df$state_fips)
  } else {
    return(state_names_abbs_df)
  }
}

#' Samantha Rhoads's function to...
#' @export
#' @examples
#' statetoabb(v)
statetoabb <- function (v, state_names_abbs_df=get_states(return_which='both')) {
  # st=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
  #      "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", 
  #      "District of Columbia",'Washington DC', "US Virgin Islands",'United States Virgin Islands','Virgin Islands', "Puerto Rico",'American Samoa', "Northern Mariana Islands", "Guam")
  # ab=c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
  #      "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", 
  #      "DC", "DC", "VI", "VI", "VI", "PR", "AS", "AP", "GU")
  state_names_abbs_df <- dplyr::distinct(state_names_abbs_df, state_names, .keep_all=T)
  v_ <- strip_punct(tolower(v), replacewithspace=F)
  from <- st <- tolower(state_names_abbs_df$state_names) %>% strip_punct(., replacewithspace=F)
  to <- ab <- toupper(state_names_abbs_df$state_abbs)
  result <- to[match(v_, from)]
  ifelse(is.na(result), v, result)
}


#' Samantha Rhoads's function to...
#' @export
#' @examples
#' abbtostate(v)
abbtostate <- function (v, state_names_abbs_df=get_states(return_which='both')) {
  v_ <- strip_punct(tolower(v), replacewithspace=F)
  state_names_abbs_df <- dplyr::distinct(state_names_abbs_df, state_abbs, .keep_all=T)
  to <- st <- state_names_abbs_df$state_names
  from <- ab <- tolower(state_names_abbs_df$state_abbs) %>% strip_punct(., replacewithspace=F)
  result <- to[match(v_, from)]
  ifelse(is.na(result), v, result)
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' state2abb_or_abb2state(v, abb=F)
state2abb_or_abb2state <- function(v, abb=F, state_names_abbs_df=get_states(return_which='both')){
  v_ <- tolower(v)
  # state_names_abbs_df <- get_states(return_which='both')
  # # state_names <- c(state.name[order(nchar(state.name), state.name, decreasing=T)], 'District of Columbia', 'Washington DC', 'Puerto Rico', 'US Virgin Islands', 'United States Virgin Islands', 'Virgin Islands', 'American Samoa')
  # addl_states <- c("S Carolina", "N Carolina", "W Virginia", "N Hampshire", "S Dakota", "N Dakota", "N Mexico", "N Jersey", "N York")
  ## state_names <- c(state_names, addl_states)
  ## state_abbs <- c(state.abb, 'DC', 'PR', 'VI', 'AS')
  if(any(!tolower(v) %in% tolower(c(state_names_abbs_df$state_names, state_names_abbs_df$state_abbs)))){v_ <- gsub('united states', 'us', v_)}
  st1 <- statetoabb(v_, state_names_abbs_df=state_names_abbs_df) %>% abbtostate(., state_names_abbs_df=state_names_abbs_df)
  st2 <- abbtostate(v_, state_names_abbs_df=state_names_abbs_df)
  result <- if(!abb){ ifelse(is.na(st1), st2, st1)} else {statetoabb(ifelse(is.na(st1), st2, st1), state_names_abbs_df=state_names_abbs_df)}
  
  # if(any(!(result %in% unlist(state_names_abbs_df) ) )){
  #   state_names_abbs_df2 <- state_names_abbs_df %>% tibble::add_row(state_names=c("Cali", "Mariana Islands"), state_abbs=c("CA", "MP"), state_fips=c("06", "69"))
  # }
  
  return(result)
}

#' Samantha Rhoads's function to extract states from a vector
#' @export
#' @examples
#' extract_state(v, keep_which=c('all', 'first', 'last')[1], return_input_if_no_match=T)
extract_state <- function(v, keep_which=c('all', 'first', 'last')[1], return_input_if_no_match=T){ 
  v_ <- gsub('W\\. V', 'W V', v)    #{v <- c('City, West Virginia', 'Larami, WY', 'Oskaloosa, DC', 'Sunnyvale', 'Baby, W. Virginia', 'Sami, W Virginia', 'Cat, N Carolina', 'Albany, N York', 'Albany, N. York', 'Laramie, Wyoming')}
  state_names_abbs_df <- get_states(return_which='both')
  addl_states <- c('United States Virgin Islands', 'Virgin Islands', "S Carolina", "N Carolina", "W Virginia", "N Hampshire", "S Dakota", "N Dakota", "N Mexico", "N Jersey", "N York")
  state_names <- c(state_names_abbs_df$state_names[order(nchar(state_names_abbs_df$state_names), state_names_abbs_df$state_names, decreasing=T)], addl_states)
  # addl_states <- grep('West|East|South|North|New', state_names, value=T) %>% gsub('West', 'W', .) %>% gsub('East', 'E', .) %>% gsub('South', 'S', .) %>% gsub('North', 'N', .) %>% gsub('New', 'N', .) %>% edit()
  state_names <- c(state_names, addl_states) %>% unique()
  state_abbs <- state_names_abbs_df$state_abbs
  v_ <- stringr::str_extract_all(tools::toTitleCase(v_), paste0("\\b(", paste_regex(state_names), ")\\b")  ) %>% sapply(., function(x) paste0(x, collapse="; "))
  if(any(nchar(v_)==0)){
    v_ <- sapply(1:length(v), function(i){ # {i=2}
      vs <- v[i]; v_s <- v_[i]
      if(nchar(v_s)==0){v_s <- stringr::str_extract_all(vs, paste0("\\b(", paste_regex(state_abbs), ")\\b")  ) %>% sapply(., function(x) paste0(x, collapse="; "))}
      if(nchar(v_s)==0){v_s <- stringr::str_extract_all(vs, paste0(paste_regex(state_names))  ) %>% sapply(., function(x) paste0(x, collapse="; "))}
      if(nchar(v_s)==0){v_s <- stringr::str_extract_all(vs, paste0("\\b(", paste_regex(tolower(state_abbs)), ")\\b")  ) %>% sapply(., function(x) paste0(x, collapse="; "))}
      if(nchar(v_s)==0){v_s <- stringr::str_extract_all(gsub('\\.', '', vs), paste0("\\b(", paste_regex(c(state_abbs, state_names)), ")\\b")  ) %>% sapply(., function(x) paste0(x, collapse="; "))}
      if(nchar(v_s)==0){v_s <- stringr::str_extract_all(vs, paste0(paste_regex(gsub(' ', '', state_names)))  ) %>% sapply(., function(x) paste0(x, collapse="; "))}
      if(nchar(v_s)==0){v_s <- stringr::str_extract_all(vs, paste_regex(state_names)  ) %>% sapply(., function(x) paste0(x, collapse="; "))}
      if(nchar(v_s)==0){v_s <- stringr::str_extract_all(tolower(vs), paste_regex(tolower(state_names))  ) %>% sapply(., function(x) paste0(x, collapse="; "))}
      if(nchar(v_s)==0&return_input_if_no_match){v_s <- vs} else if(nchar(v_s)==0){v_s <- NA}
      v_s
    }) %>% as.character()
  }
  if(keep_which=='first'){
    v_ <- gsub(';.*', '', v_) %>% trimws()
  } else if(keep_which=='last'){
    v_ <- gsub('.*;', '', v_) %>% trimws()
  }
  v_
}


#' Samantha Rhoads's function to recode states (even from state fips codes)
#' @export
#' @examples
#' recode_state(v, abb=T, to_fips=F)
recode_state <- function(v, abb=T, to_fips=F, state_names_abbs_df=get_states(return_which='both')){
  if(all(is.numeric(v))|all(lookslike_number(v))){
    v <- state_names_abbs_df$state_abbs[match(readr::parse_number(as.character(v)), readr::parse_number(as.character(state_names_abbs_df$state_fips)))]
  }
  result <- state2abb_or_abb2state(v, abb=abb, state_names_abbs_df=state_names_abbs_df)
  
  if(any(!(result %in% unlist(state_names_abbs_df) ) )){
    state_names_abbs_df2 <- state_names_abbs_df %>% tibble::add_row(state_names=c("Cali", "Mariana Islands", "Virgin Islands"), state_abbs=c("CA", "MP", "VI"), state_fips=c("06", "69", "78"))
    result <- state2abb_or_abb2state(result, abb=abb, state_names_abbs_df=state_names_abbs_df2)
    
    if(any(!(result %in% unlist(state_names_abbs_df) ) )){
      state_names_abbs_df2 <- dplyr::bind_rows(state_names_abbs_df2, 
                                               mutate(state_names_abbs_df2, state_names=gsub("(W|E|S|N)(est|ast|outh|orth(ern|))", "\\1", state_names)),
      ) %>% 
        bind_rows(.,
                  mutate(state_names_abbs_df2, state_names=gsub(" ", "", state_names))
        ) %>% distinct()
      result <- gsub("(W|E|S|N)(est|ast|outh|orth(ern|))", "\\1", strip_punct(result))
      result <- state2abb_or_abb2state(result, abb=abb, state_names_abbs_df=state_names_abbs_df2)
    }
  }
  
  if(to_fips){
    result <- state2abb_or_abb2state(result, abb=T, state_names_abbs_df=state_names_abbs_df)
    state_fips_df <- dplyr::distinct(state_names_abbs_df, state_abbs, .keep_all=T)
    from <- state_fips_df$state_abbs
    to <- state_fips_df$state_fips
    result <- to[match(result, from)]
  }
  return(result)
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' state2region(v_abbr)
state2region <- function(v_abbr){
  (v_abbr <- state2abb_or_abb2state(v_abbr, abb=T))
  vdf <- tibble(state.abb = tolower(v_abbr))
  (regdf <- tibble::tibble(state.region=as.character(state.region), state.abb= tolower(state.abb)) %>% dplyr::left_join(vdf, .))
  regdf$state.region[grepl("vt|dc", regdf$state.abb)] <- "Northeast"
  regdf$state.region[grepl("pr|gu|nmari", regdf$state.abb)] <- "Other"
  regdf$state.region
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' str_extract_all_concat()
str_extract_all_concat <- function(v, pattern, exact=F, collapse="; ", ignore.case=F){
  pattern <- regex(paste_regex(pattern, exact=exact), ignore.case=ignore.case)
  stringr::str_extract_all(v, pattern) %>%
    lapply(., function(s) paste0(s, collapse=collapse)) %>% unlist()
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' str_extract_money()
str_extract_money <- function(v) v %>% 
  gsub_ic(" K", "K", .) %>% 
  gsub_ic(" thousand|thousand", "K", .) %>% 
  gsub_ic(" M", "M", .) %>% 
  gsub_ic(" million|million|mill|mil", "M", .) %>% 
  gsub("K.*| K", "K", .) %>% 
  gsub("M.*| M", "M", .) %>% 
  extract_word_startswith_dollarsign() %>% 
  gsub("K.*| K", "K", .) %>% 
  gsub("M.*| M", "M", .) %>% 
  gsub("\\.$", "", .) %>% 
  na_if("$") %>%
  # gsub("K", "000", .) %>% 
  # gsub("M", "000000", .) %>% 
  gsub("K", "e3", .) %>% 
  gsub("M", "e6", .) %>% 
  gsub(",|\\$", "", .) %>% 
  na_if_() %>% 
  as.numeric()



# July 9, 2019 (07092019) _ ########################################################################################################################

#' Samantha Rhoads's function to copy something that looks like a dataframe (formatted like a dataframe) from a webpage/website & paste it in this function as one string & get a dataframe object back as output
#'
#' Srhoads wrote this to allow you to copy/paste from clipboard something that looks like a dataframe (formatted like a dataframe) from a webpage/website & paste it in this function as one string & get a dataframe object back as output. Alternative names could have been (but do not actually work) read.table_as... read.table_paste... read.table_from... read_table_as... read_table_paste... read_table_from...
#' @export
#' @examples
#' as_table_paste(pastedstuff, sep="\t", header=T)
as_table_paste <- read_table_paste <- function(pastedstuff, sep="\t", header=T) {read.csv(text=pastedstuff, sep=sep, header=header)}

#' Samantha Rhoads's function to copy something that looks like a dataframe (formatted like a dataframe) from a webpage/website & paste it in this function as one string & get a dataframe object back as output
#'
#' Srhoads wrote this to allow you to copy/paste from clipboard something that looks like a dataframe (formatted like a dataframe) from a webpage/website & paste it in this function as one string & get a dataframe object back as output. Alternative names could have been (but do not actually work) read.table_as... read.table_paste... read.table_from... read_table_as... read_table_paste... read_table_from...
#' @export
#' @examples
#' read_table(pastedstuff, sep="\t", header=T)
read_table <- function(pastedstuff, sep="\t", header=T) {tryCatch(read.csv(text=pastedstuff, sep=sep, header=header), error=function(e) read.table(text=pastedstuff, sep=sep))}


#' Srhoads wrote this to allow you to copy/paste from clipboard something that looks like a dataframe (formatted like a dataframe) from a webpage/website & paste it in this function as one string & get a dataframe object back as output. Alternative names could have been (but do not actually work) read.table_as... read.table_paste... read.table_from... read_table_as... read_table_paste... read_table_from... Same as read_table, similar to as_table_paste: read data table pasted from clipboard
#' @export
#' @examples
#' read.table_fromClipboard(pastedstuff, sep="\t", header=T)
read.table_fromClipboard <- read_table


#' Srhoads wrote this to allow you to copy/paste from clipboard something that looks like a dataframe (formatted like a dataframe) from a webpage/website & paste it in this function as one string & get a dataframe object back as output. Alternative names could have been (but do not actually work) read.table_as... read.table_paste... read.table_from... read_table_as... read_table_paste... read_table_from... Same as read_table, similar to as_table_paste: read data table pasted from clipboard. Same as read.table_fromClipboard too
#' @export
#' @examples
#' read.csv_fromClipboard(pastedstuff, sep="\t", header=T)
read.csv_fromClipboard <- read_table


#' Srhoads wrote this to allow you to copy/paste from clipboard something that looks like a dataframe (formatted like a dataframe) from a webpage/website & paste it in this function as one string & get a dataframe object back as output. Alternative names could have been (but do not actually work) read.table_as... read.table_paste... read.table_from... read_table_as... read_table_paste... read_table_from... Same as read_table, similar to `as_table_paste()`: read data table pasted from clipboard. Same as `read.table_fromClipboard()` and `read.csv_fromClipboard()` too
#' @export
#' @examples
#' paste_table(pastedstuff, sep="\t", header=T)
paste_table <- read_table



#' Samantha Rhoads's function to split a string between a lowercase and capital letter, as long as the letter after the uppercase one is a lowercase letter
#'
#' Srhoads wrote this to allow you to split a string between a lowercase and capital letter, as long as the letter after the uppercase one is a lowercase letter (loosly means you don't need the letter after the capital letter to be lowercase)
#' @export
#' @examples
#' split_before_capital(x, sep=" ", loosly=F)
split_before_capital <- function(x, sep=" ", loosly=F){ if(!loosly) gsub('([[:lower:]])([[:upper:]])([[:lower:]])', paste0('\\1', sep, '\\2\\3'), x) else gsub('([[:lower:]])([[:upper:]])', paste0('\\1', sep, '\\2'), x)}


########################################################################################################################




# August 7, 2019 (08072019) _ ########################################################################################################################

#' This function allows you to load and/or install package first; you can specifify if you want it unloaded (or even loaded at all tbh. Setting import=F is a good way to check if the package is installed on your machine, without actually doing anything with it.)
#' @export
#' @examples
#' pkg(package1, ..., dependencies=NA, import=T, unload=F, url=NULL, version=NULL, repos=NULL)
pkg <- function(package1, ..., dependencies=NA, import=T, unload=F, url=NULL, version=NULL, repos=NULL) {
  packages <- c(package1, ...)
  if((is.null(repos)|all(is.na(repos)))){repos<-c("https://cloud.r-project.org", "http://owi.usgs.gov/R/", "https://cran.rstudio.com/")}
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      if(import){do.call(library, list(package))}
      if(unload){try(unloadNamespace(package))}
    } else {
      if((is.null(url)|all(is.na(url))) & (is.null(version)|all(is.na(version))) ){
        install.packages(package, repos=repos, dependencies=dependencies, type=getOption("pkgType"))
      } else if(!(is.null(url)|all(is.na(url))) & (is.null(version)|all(is.na(version))) ){
        install.packages(url, repos=NULL)
      } else if((is.null(url)|all(is.na(url))) & !(is.null(version)|all(is.na(version))) ){
        devtools::install_version(package, version=version, repos=repos)
      }
      if(import){do.call(library, list(package))}
      if(unload){try(unloadNamespace(package))}
    }
  }
}



#' This function allows you to load and/or install package first (tidyverse included by default)!
#' @export
#' @examples
#' pkg_tidy(package1="tidyverse", ...)
pkg_tidy <- function (package1="tidyverse", ...) {
  packages <- unique(c(c(package1, ...), "magrittr"))
  for (package in packages) {
    if (package %in% rownames(installed.packages())) do.call(library, list(package))
    else {
      install.packages(package, repos = c("https://cloud.r-project.org", 
                                          "http://owi.usgs.gov/R/"), dependencies = NA, 
                       type = getOption("pkgType"))
      do.call(library, list(package))
    }
  }
}


#' This function allows you to load and/or install package first (wrapper with tryCatch)!
#' @export
#' @examples
#' install.packages_wrapper(package, dependencies = NA, githubrepo=NULL, repos = c("https://cloud.r-project.org", "http://owi.usgs.gov/R/"), type = getOption("pkgType"))
install.packages_wrapper <- function(package,  dependencies = NA, githubrepo=NULL,
                                     repos = c("https://cloud.r-project.org", "http://owi.usgs.gov/R/"), 
                                     type = getOption("pkgType")){
  tryCatch({
    install.packages(package, repos=repos, dependencies=dependencies, type=type)
    do.call(library, list(package))
  },
  error=function(e){
    if ('devtools' %in% rownames(installed.packages())) do.call(library, list('devtools')) else install.packages("devtools"); library(devtools)
    if(is.null(githubrepo)|!exists("githubrepo")) githubrepo <- "srhoads"
    tryCatch(devtools::install_github(paste0(githubrepo, "/", package)), 
             error=function(e) {
               tryCatch(devtools::install_github(package),
                        error=function(e) cat(paste0("\n", package, " --can't find\n")))
               
             })
  })
}  


#' This function allows you to load and/or install package first (holistic)!
#' `pipes` arg Refers to loading the `magrittr` package to get its pipes (`%<>%`)
#' @export
#' @examples
#' pkg3(package1=NULL, ..., pipes=F, dependencies=NA, githubrepo=NULL, repos = c("https://cloud.r-project.org", "http://owi.usgs.gov/R/"), type = getOption("pkgType"))
pkg3 <- pkg2 <- function (package1=NULL, ..., pipes=F, dependencies=NA, githubrepo=NULL, repos = c("https://cloud.r-project.org", "http://owi.usgs.gov/R/"), type = getOption("pkgType")) {
  if(is.null(package1)) package1 <- "tidyverse"
  packages <- unique(c(package1, ...))
  if(pipes) packages <- unique(c(packages, "magrittr"))
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {do.call(library, list(package)); cat(paste0(package, " loaded\n"))}
    else {
      tryCatch({
        install.packages_wrapper(package, 
                                 dependencies=dependencies, githubrepo=githubrepo,
                                 repos = repos, type = type)
        package <- gsub(".*/", "", package)
        do.call(library, list(package))
        cat(paste0("\n", package, ": installed & loaded!\n"))
      }, 
      error=function(e) cat(paste0(package, ": can't install\n")))
      tryCatch(do.call(library, list(package)), error=function(e) cat(paste0(package, ": can't load\n\n")))
    }
  }
}


#' This function allows you to get col_types when using `readr` package for functions like `read_csv()` with the argument `col_types=`
#' c = character, i = integer, n = number, d = double, l = logical, f = factor, D = date, T = date time, t = time, ? = guess, or _/- to skip the column
#' @export
#' @examples 
#' readr::read_csv("http://samplecsvs.s3.amazonaws.com/Sacramentorealestatetransactions.csv", col_types=col_types('c'))
#' col_types(type="c") 
col_types <- function(type="c"){
  # c = character, i = integer, n = number, d = double, l = logical, f = factor, D = date, T = date time, t = time, ? = guess, or _/- to skip the column
  if(grepl("text|^c$|character", type, ignore.case=T)) type <- "c"
  readr::cols(.default = type)
}


# 09/11/2019 (SEPTEMBER 11, 2019) 09112019 #######################################################################################################################


#' This function allows you to group by a desired variable and summarize it by `n()` (total count/sum). Compatible with dataframes & vectors. Uses `matches()` logic.
#' @export
#' @examples 
#' gbsum(d, var=NULL) 
gbsum <- group_by_summary <- group_by_summarize <- function(d, var=NULL){
  if(is.null(var)) var <- names(data.frame(var=d))[1]
  if(is.data.frame(d)) d %>% dplyr::group_by_at(vars(dplyr::matches(var))) %>% dplyr::summarize(total=n()) %>% ungroup()
  else data.frame(var = d) %>% dplyr::group_by(var) %>% dplyr::summarize(total = n()) %>% setNames(gsub("var", var, names(.))) %>% ungroup()
}


# gbsum <- group_by_summary <- group_by_summarize <- function(d, var=NULL, math=n){
#   MATH <- function(x) tryCatch(math(x, na.rm=T), error=function(e) math(x))
#   if(is.null(var)) var <- "SUMMARYVAR"
#   if(is.data.frame(d)) d %>% ungroup() %>% dplyr::group_by_at(vars(dplyr::matches(var))) %>% dplyr::summarize(total=n())
#   else data.frame(var = d) %>% dplyr::group_by(var) %>% dplyr::summarize(total = n()) %>% setNames(gsub("var", var, names(.)))
# }

########################################################################################################################

# 09122019 #######################################################################################################################

#' This function allows you to both return and print output at the same time, but not redundantly. You know when you run a function in R but don't assign the output to anything & it returns what you ran in the console? But not when you assign it to something? Well now you can print/cat & assign, but still get the stuff returned how intend. AND it won't return twice in the console--just one cute lil time. You can choose between `print` or `cat` as your desired output to the console.
#' @export
#' @examples 
#' printurn(stuff, how=c("cat", "print")) 
printurn <- caturn <- function(stuff, how=c("cat", "print")){
  how <- match.arg(how)
  if(how=="cat") tryCatch(cat(stuff, "\n"), error=function(e) print(stuff))
  if(how=="print") print(stuff)
  invisible(stuff)
}

#' This function is like `cat` but is wrapped in `\\n` (line breaks). It prints with formatting, and without quotes. Look at `cat`'s documentation for details
#' @export
#' @examples 
#' catn(..., file = "", sep = " ", fill=F, labels = NULL, append=F, collapse=" ") 
catn <- function(..., file = "", sep = " ", fill=F, labels = NULL, append=F, collapse=" "){
  cat("\n", paste0(..., collapse = collapse), "\n", 
      file=file, sep = sep, fill = fill, labels = labels, append = append)
}


# 12122020 #######################################################################################################################
#' A function to literally do nothing... just returns the identical object you fed it
#' @export
#' @examples 
#' donothing(x)
donothing <- function(x){
  x
}

# 10232019 #######################################################################################################################

#' This function is to arrange a dataframe by fewest NAs first (at the top of the dataset/earliest rows).
#' @export
#' @examples 
#' arrange_by_na(d)
arrange_by_na <- function (d) { d %>% dplyr::arrange(rowSums(is.na(.)))}

#' Function to arrange a dataframe by MOST NAs first (at the top of the dataset/earliest rows).
#' @export
#' @examples {D <- data.frame(id=1:5, gender=c("F", "M", NA, "F", NA), race=c(3, 5, NA, 7, NA))}; arrange_by_na_desc(D); 
#' arrange_by_na_desc(d)
arrange_by_na_desc <- function(d){
  d %>% dplyr::arrange(dplyr::desc(rowSums(is.na(.))))
}
########################################################################################################################


# 11012019 #######################################################################################################################

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' is.POSIX(v)
is.POSIX <- function(v){ if(any(grepl("POSIX", class(v)))) T else F}


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' is.Date(v)
is.Date <- function(v) {if(any(class(v)=="Date")|is.POSIX(v)) T else F}

#' Samantha Rhoads's function to...
#'
#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' is.Date.class(v)
is.Date.class <- function(v){ if(all(class(v)=="Date")) T else F}


#' Jason originally wrote this to allow you to find the closest date in your piece of data to a given date you choose, like mathcing them up. Now it's Samantha Rhoads's function to find the closest date
#' @export
#' @examples
#' closest(x, y, type=NA)
closest <- function(x, y, type=NA) {
  if ((length(x) == 0) | (length(y) == 0)) return(rep(F, length(y)))
  
  if (is.na(type)) {
    found_y <- y[which.min(abs(x - y))]
    found_y == y
  } else if (type == 'l') {
    if (sum(y <= x) == 0) return(rep(F, length(y)))
    new_y <- y[(y <= x)]
    found_y <- new_y[which.min(x - new_y)]
    y == found_y
  } else if (type == 'r') {
    if (sum(y >= x) == 0) return(rep(F, length(y)))
    new_y <- y[(y >= x)]
    found_y <- new_y[which.min(abs(x - new_y))]
    y == found_y
  }
}

########################################################################################################################


# 11052019 #######################################################################################################################



#' Samantha Rhoads's function to give length and sumry in one go
#'
#' Srhoads wrote this to allow you to get the length of the object and sumry of the object in one call (see `sumry()` for details on that type of summary)
#' @export
#' @examples
#' sumrys(x, n=7)
sumrys <- function(x, n=7){
  list(dim_or_length = tryCatch(dim(x), error=function(e) length(x)),
       sumry = sumry(x, n)#,
       # lapplysumry = lapply(x, function(xx) sumry(xx, n))
  )
}

#' Samantha Rhoads's function to return today's date in yyyymmdd format (no punctuation)
#' @export
#' @examples
#' sysdate()
sysdate <- sysdateYMD <- function(){format(Sys.Date(), format="%Y%m%d")}

#' Samantha Rhoads's function to return today's date in mmddyyyy format (no punctuation)
#' @export
#' @examples
#' sysdateMDY()
sysdateMDY <- function(){format(Sys.Date(), format="%m%d%Y")}

########################################################################################################################


# 11202019 #######################################################################################################################

#' Samantha Rhoads's function to return the most recent versions of files based on a given pattern of characters of its name
#' @export
#' @examples
#' getMostRecentFiles(path=".", desc=T, verbose=F, pattern=NULL, all.files=F, full.names=T, recursive=T, ignore.case=F, include.dirs=F, no..=F, printfilenames=F)
getMostRecentFiles <- function(path=".", desc=T, verbose=F, pattern=NULL, all.files=F, full.names=T, recursive=T, ignore.case=F, include.dirs=F, no..=F, printfilenames=F){
  fns <- list.files(path=path, pattern=pattern, all.files=all.files, full.names=full.names, recursive=recursive, ignore.case=ignore.case, include.dirs=include.dirs, no..=no..)
  if(printfilenames){print(fns)}
  if(desc) {
    fninfo <- fns %>% file.info() %>% data.frame(name=fns, .) %>% dplyr::arrange(desc(mtime))
  } else {
    fninfo <- fns %>% file.info() %>% data.frame(name=fns, .)  %>% dplyr::arrange(mtime)
  }
  if(verbose){
    return(fninfo)
  } else {
    return(fninfo %>% .$name %>% as.character())
  }
}

########################################################################################################################

# 12102019 #######################################################################################################################

#' Srhoads wrote this to allow you to check if all of a variable is NA or NULL (edited 20200226)
#' @export
#' @examples
#' is.nanull(x)
is.nanull <- function(x){
  tryCatch({
    all(is.na(x)) | is.null(x)
  },
  error=function(e){
    x <- as.character(x)
    all(is.na(x)) | is.null(x)
  })
}

#' Samantha Rhoads's function to take an Excel 5-digit date and turn it into a date format
#' @export
#' @examples
#' excelToDateIf5DigitStr(v)
excelToDateIf5DigitStr <- function(v){
  if(all(unique(nchar(na.omit(gsub("[^[:digit:]]", "", v))))==5)){
    v <- lubridate::date(janitor::excel_numeric_to_date(as.numeric(v)))
  }
  return(v)
}


#' Samantha Rhoads's function to take an Excel 5-digit date (or many digit datetime like '43467 381058125') + turn it into a date format
#' @export
#' @examples
#' excelToDateIf5DigitStrAndManyDigitTime(v)
excelToDateIf5DigitStrAndManyDigitTime <- function(v, force_include_time=T){  #ie: {v="43467 381058125"}...or... {v="43467 402791006942"}...or... {v="43798.999305555597"}
  # if(stringi::stri_count_words(v)==2&!grepl("[[:alpha:]]", v)){
  #   v <- v %>% gsub(" ", ".", .) %>% trimws_()
  # }
  v2 <- ifelse((stringi::stri_count_words(v)==2&!grepl("[[:alpha:]]", v))&!is.Date(v), {v %>% gsub(" ", ".", .) %>% trimws_()}, as.character(v))
  
  if(
    all(
      unique(nchar(na.omit(
        v2 %>% word(1) %>% gsub("[^[:digit:]]", "", .)
      )))==5
    ) & all(
      unique(nchar(na.omit(
        v2 %>% word(2) %>% gsub("[[:digit:]]", "", .)
      )))==0
    )
  ){
        v2 <- word(v2, 1)
    v2 <- lubridate::as_datetime(janitor::excel_numeric_to_date(as.numeric(v2), include_time=force_include_time))
  } else {
    if(any(grepl("\\.", v2))&!is.Date.class(v2)){
      # v2 <- gsub("\\.", " ", v2)
      v2 <- word(v2, 1)
      v2 <- tryCatch({lubridate::as_datetime(janitor::excel_numeric_to_date(as.numeric(v2), include_time=force_include_time))}, error=function(e) v2)
    }
  }
  v2 <- tryCatch({
    if(force_include_time){
      v2 <- lubridate::as_datetime(v2)
    } else if(nchar(v<=5)) {
      v2 <- lubridate::date(v2)
    }
  },
  error=function(e){
    v2
  })
  return(v2)
}


#' Samantha Rhoads's function to extract the date from a string, but doesn't turn it into a date!
#' @export
#' @examples
#' extract_date(v)
extract_date <- function(v) {
  datepats <- c(
    # datepat000=' ?(0|1)?([0-9]{2})\\/(?(0|1|2|3)?([0-9]{2}))\\/',
    # datepat000=' ?(0|1)?([0-9]{2})\\/(\\d{1,2})\\/(\\d{4})',
    # datepat000='(\\d{1,2})\\/(\\d{1,2})\\/(\\d{4})',
    datepat00=' ?(0|1)?([0-9]{4}|[0-9]{1,2})-([0-9]{2,4})?| ?(0|1)?[1-9]-([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]/([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})\\.([0-9]{1,2})\\.([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]\\.([0-9]{1,2}|[0-9]{4}) ?',
    datepat0=' ?(0|1)?([0-9]{4}|[0-9]{1,2})-([0-9]{1,2})-([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]-([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]/([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})\\.([0-9]{1,2})\\.([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]\\.([0-9]{1,2}|[0-9]{4}) ?',
    datepat14=' ?(0|1)?([0-9]{4}|[0-9]{1,2})([0-9]{1,2})([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})([0-9]{1,2})([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([0-9]{4}|[0-9]{1,2})([0-9]{1,2})([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[0-9]([0-9]{1,2}|[0-9]{4}) ?',
    # 
    datepat1=' ?(0|1)?([1-9]{1,2}|[1-9]{4})/([0-9]{1,2})/([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?[1-9]/([0-9]{1,2}|[0-9]{4}) ?',
    datepat2=' ?(0|1)?([1-9]{1,2}|[1-9]{4})-([0-9]{1,2})-([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?[1-9]-([0-9]{1,2}|[0-9]{4}) ?',
    datepat3=' ?(0|1)?([1-9]{1,2}|[1-9]{4})\\.([0-9]{1,2})\\.([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?[1-9]\\.([0-9]{1,2}|[0-9]{4}) ?',
    datepat4=' ?(0|1)?[1-9]/([0-9]{1}|[0-9]{2})/([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]/([0-9]{2}|[0-9]{4}) ?| ?(0|1)?[1-9]/([0-9]{4}|[0-9]{1,2}) ?',
    datepat5=' ?(0|1)?[1-9]-([0-9]{1}|[0-9]{2})-([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]-([0-9]{2}|[0-9]{4}) ?| ?(0|1)?[1-9]-([0-9]{4}|[0-9]{1,2}) ?',
    datepat6=' ?(0|1)?[1-9]\\.([0-9]{1}|[0-9]{2})\\.([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]\\.([0-9]{2}|[0-9]{4}) ?| ?(0|1)?[1-9]\\.([0-9]{4}|[0-9]{1,2}) ?',
    # 
    datepat7=' ?(0|1)?([1-9]{4}|[0-9]{1,2})/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]/([0-9]{1,2}|[0-9]{4}) ?',
    # datepat8=' ?(0|1)?([1-9]{4}/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]/([0-9]{1,2}|[0-9]{4}) ?| ?(0|1)?([1-9]{1,2}/([0-9]{1,2})/([0-9]{4}|[0-9]{2,4}) ?',
    datepat9=' ?(0|1)?([1-9]{4}|[0-9]{1,2})/([0-9]{1,2})/([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]/([0-9]{1,2}|[0-9]{4}) ?',
    datepat10=' ?(0|1)?([1-9]{4}|[1-9]{1,2})\\.([0-9]{1,2})\\.([0-9]{4}|[0-9]{1,2}) ?| ?(0|1)?[1-9]\\.([0-9]{1,2}|[0-9]{4}) ?',
    datepat11=' ?(0|1)?[1-9]/([0-9]{4}|[0-9]{1,2})/([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]/([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]/([0-9]{4}|[0-9]{1,2}) ?',
    datepat12=' ?(0|1)?[1-9]-([0-9]{4}|[0-9]{1,2})-([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]-([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]-([0-9]{4}|[0-9]{1,2}) ?',
    datepat13=' ?(0|1)?[1-9]\\.([0-9]{4}|[0-9]{1,2})\\.([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]\\.([0-9]{4}|[0-9]{2}) ?| ?(0|1)?[1-9]\\.([0-9]{4}|[0-9]{1,2}) ?')
  # v %>% stringr::str_extract_all(., paste0('datepath', 1:12))
  (patternstr <- datepats %>% unlist() %>% paste0(., collapse="|"))
  v %>% stringr::str_extract_all(., patternstr)
}

if(F){v %>% stringr::str_extract_all(., datepats[1])}

#' Samantha Rhoads's function to extract the 4-digit year from a string, and stores it as a character!
#' @export
#' @examples
#' extract_year(v)
extract_year <- function(v){
  v %>% as.character() %>% gsub(".*(\\d{4}).*", "\\1", .) %>% select_matches("\\d{4}")
}

#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' fillr(df, ID='ID')
fillr <- function(df, ID='ID'){
  cat("fillr()\n")
  df <- na_if_(df)
  df[['var']] <- df[[ID]]
  df %<>% dplyr::group_by(var) %>%
    na_if_() %>%
    tidyr::fill(-var) %>%
    na_if_() %>%
    tidyr::fill(-var, .direction='up') %>%
    na_if_() %>%
    dplyr::distinct()
  # df %>% select(-matches('^var$'))
  df <- df[, -ncol(df)]
  print(paste0('fillr: ', ID))
  df
  # dplyr::distinct(df)
}


#' Samantha Rhoads's function fillr2
#' @export
#' @examples
#' fillr2(df, ID='ID')
fillr2 <- function(df, ID='ID'){
  DATE_COLUMN_NAMES <- df %>% select_if(is.Date.class) %>% names()
  df %<>% mutate_at(vars(one_of(DATE_COLUMN_NAMES)), as.character)
  df <- na_if_(df)
  df[['var']] <- df[[ID]]
  df %<>% dplyr::group_by(var) %>%
    tidyr::fill(-var) %>%
    tidyr::fill(-var, .direction='up')
  df <- df[, -ncol(df)]
  cat(paste0('fillr3: ', ID))
  df %<>% mutate_at(vars(one_of(DATE_COLUMN_NAMES)), lubridate::date)
  df
}


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' DT_NAs_red_background(DTdatatable)
DT_NAs_red_background <- function(DTdatatable){
  cat("\n", "DT_NAs_red_background()", "\n")
  # DTdatatable$x$data <- DTdatatable$x$data %>% mutate_all(., function(v) replace_na(v, "<center>-</center>"))
  NAcodestoRED <- c(NA, "", " ", "  ", "-", "<center>-</center>", "<center> </center>", "<center></center>", "<center>  </center>")
  DTdatatable %>%
    DT::formatStyle(names(.$x$data),
                    backgroundColor = styleEqual(
                      NAcodestoRED,
                      rep("rgb(255,198,198)", length(NAcodestoRED)))
    )
}



#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' select_vec(v, pattern, ignore.case=T, everything=F)
select_vec <- function(v, pattern, ignore.case=T, everything=F){
  as_tibble <- function(x, ..., .rows = NULL, .name_repair = c("check_unique", "unique", "universal", "minimal"), rownames = pkgconfig::get_config("tibble::rownames", NULL)){
    suppressWarnings(tibble::as_tibble(x, ..., .rows, .name_repair, rownames))
  }
  tv <- as_tibble(t(as_tibble(v))) %>% setNames(v %>% as.character() %>% make.unique(., sep="~FUNKYDISTINCTIVEPATTERN~~MEOW~~920482033243931~~~BDJKSFH~SAMISRHOADSY"))
  if(everything){
    tv <- tv %>% dplyr::select(dplyr::matches(paste_regex(pattern)), dplyr::everything()) %>% names()
  } else {
    tv <- tv %>% dplyr::select(dplyr::matches(paste_regex(pattern))) %>% names()
  }
  tv0 <- tv %>% gsub("~FUNKYDISTINCTIVEPATTERN~~MEOW~~920482033243931~~~BDJKSFH~SAMISRHOADSY.*", "", .)
  if(is.factor(v)){
    tv0 <- tv0 %>% factor(., levels=levels(v))
  }
  tv0
}



#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' dt_datatables_pre()
dt_datatables_pre <- function(df, pageLength=nrow(df), open_file=F){
  # cat("dt_datatables_pre()\n")
  # Update? (Yes/no/cancel) y
  # DT :
  #   Version 0.20 installed in /Library/Frameworks/R.framework/Versions/4.1/Resources/library 
  #   Version 0.23 available at https://cran.rstudio.com
  x <- df %>%
    DT::datatable(.,
                  # caption=shiny::HTML("<code>formula</code> is encoded as <code>y ~ x</code>, or dependent variable (<code>y</code>) predicted by independent variable (<code>x</code>)"),
                  escape=F,
                  rownames=F,
                  # extensions = "Buttons",
                  # filter = list(position = 'top', clear = F),
                  # height="550%",
                  options = list(#search = list(regex=T, caseInsensitive = T),
                    pageLength = pageLength,
                    dom = 'BfrtipSR',
                    autoWidth=T,
                    # columnDefs = list(list(width = '10%', targets = collapsevars)),
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis'),
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().body()).css({'font-size': '76%'});",
                      # "$(this.api().table().buttons()).css({'font-size': '80%'});",
                      "$(this.api().table().header()).css({'font-size': '76%'});",
                      "}")),
                  class = 'cell-border stripe table-hover table-condensed compact'
    )
  if(open_file){
    path <- tempfile(fileext=".html")
    if(!grepl("xls(x|)$", path, ignore.case=T)){path <- paste0(path, ".html")}
    if(is.data.frame(x)){widget <- DT::datatable(x)} else if("datatables" %in% class(x)){widget <- x}
    htmlwidgets::saveWidget(widget, path, selfcontained=TRUE, libdir=NULL, background="white", title=class(widget)[[1]], knitrOptions=list())
    system(paste0('open "', path, '"'))
  }
  return(x)
}



#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' dt_condensed()
dt_condensed <- function(df, pageLength=nrow(df)){
  DT::datatable(df,
                escape=F,
                rownames=F,
                options = list(#search = list(regex=T, caseInsensitive = T),
                  pageLength = pageLength,
                  dom = 'BfrtiSR',
                  info=F,
                  autoWidth=T,
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis'),
                  initComplete = DT::JS(
                    "function(settings, json) {",
                    "$(this.api().table().body()).css({'font-size': '76%'});",
                    "$(this.api().table().header()).css({'font-size': '76%'});",
                    "}")),
                class = 'cell-border stripe table-hover table-condensed compact'
  )
}


#' Srhoads wrote this to allow you to...
#' @export
#' @examples
#' dt_sumry()
dt_sumry <- function(dftosumry, pageLength=nrow(df)){
  dftosumry %>%
    mutate_if(is.Date.class, function(v) as.character(v))  %>%
    lapply(., function(v) sumry(v, 20)) %>%
    paste0("<code>", names(.),"</code>:::  ", ., collapse="NEWLINENEWLINENEWLINE" #collapse="<hr>"
    ) %>% gsub("c\\(|\\)|\\(|\\`", "", .)  %>% 
    paste0("<small>", ., "</small>") %>% 
    gsub(", ", "<br>", .) %>%
    strsplit(., "NEWLINENEWLINENEWLINE") %>% unlist() %>%
    tibble(value=.) %>% # HTML() %>% # tibble() %>% 
    separate(., value, into=c("Variable", "Summary (Count)"), sep=":::") %>%
    # setNames("Variable Summaries") %>% 
    DT::datatable(.,  escape=F,
                  fillContainer=T,
                  # filter = list(position = 'top', clear = F),
                  # extensions = c("Scroller", "Buttons", "ColReorder"),
                  class = 'cell-bordered stripe table-condensed compact',
                  options = list(#scrollX=T,
                    # autoWidth=T,
                    scrollCollapse = T,
                    search = list(regex=T, caseInsensitive = T),
                    pageLength = pageLength,
                    columnDefs = list(list(className = 'dt-left', targets = "_all")),
                    colReorder=T,
                    dom = 'BfrtiSR', #dom = 'BfrtilSR',
                    scrollY = "200vh",
                    # buttons = I('excel'),
                    initComplete = DT::JS(
                      "function(settings, json) {",
                      "$(this.api().table().body()).css({'font-size': '76%'});",
                      "$(this.api().table().header()).css({'font-size': '76%'});",
                      "}")),
                  # class = 'cell-border stripe table-hover table-condensed compact'
    ) %>%
    DT::formatStyle(0, target= 'row', color = 'black', lineHeight='98%', fontSize="95%")
  # DT::datatable(sumryoutput,
  #               escape=F, rownames=F,
  #               options = list(#search = list(regex=T, caseInsensitive = T),
  #                 pageLength = pageLength,dom = 'BfrtiSR',info=F, autoWidth=T,
  #                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis'),
  #                 initComplete = DT::JS(
  #                   "function(settings, json) {",
  #                   "$(this.api().table().body()).css({'font-size': '76%'});",
  #                   "$(this.api().table().header()).css({'font-size': '76%'});",
  #                   "}")),
  #               class = 'cell-border stripe table-hover table-condensed compact'
  # )
}

#' Samantha Rhoads's function to generate a random string
#' @export
#' @examples
#' rand_str(n=1, length=10, pattern = "[a-zA-Z0-9]")
rand_str <- function(n=1, length=10, pattern = "[a-zA-Z0-9]"){stringi::stri_rand_strings(n = n, length = length, pattern = pattern)}

#' Samantha Rhoads's function removes all special characters and spaces
#' @export
#' @examples
#' kill_special_space(text)
kill_special_space <- function(text) {
  stringr::str_replace_all(stringr::str_replace_all(text, "[^[:alnum:][:space:]]", ""), " ", "")
}


# January 10, 2020 (20200110) ##########################################################################################################################

#' Samantha Rhoads's function returns R package URLs for a given input package. Great if you need to install old package versions by their zipped/compressed tar.gz URL from cran
#' Srhoads wrote this to allow you to get versions of packages. You can return the whole package version URL if desired too, ie: input "devtools" and get "https://cran.r-project.org/src/contrib/Archive/devtools/devtools_2.0.2.tar.gz" and other versions as a vector
#' @export
#' @examples
#' getPackageVersions()
getPackageVersions <- function(pkg = "devtools", sortNewToOld=T, getFullURL=T){
  baseURL <- "https://cran.r-project.org/src/contrib/Archive/"
  url <- paste0(baseURL, pkg, "/")
  urlHTML <- read_html(url)
  
  urlHTML %>%
    rvest::html_nodes("td") %>%
    rvest::html_nodes("a")%>%
    rvest::html_text() %>%
    select_vec(pkg) %>% 
    {if(sortNewToOld) rev(.) else .} %>%
    {if(getFullURL) paste0(url, .) else .}
}


#' Samantha Rhoads's function to extract a date of 8 digits from a string. `sep` is the character that separates parts of a date, ie: dash or slash or nothing.
#' Srhoads wrote this to allow you to extract a date of 8 digits from a string, ie: `extract_eightDigitDate(string="I am Sam and I was born on 01-17-1996. Today is 20201001.", sep=c("-", "\\.", "/", ""))` returns a hierarchical list with: "01-17-1996" and "20201001" in it at various places 
#' @export
#' @examples
#' extract_eightDigitDate(string, sep=c("-", "\\.", "/", ""))
extract_eightDigitDate <- function(string, sep=c("-", "\\.", "/", "")){
  fourDigitYr <- fourDigitYr_1900sOr2000s <- "\b?(19|20)([0-9]{2})"
  # twoDigitMonth <- "(\b?(0|1)([0-9]{1}))"
  twoDigitMonth <- "(\b?(0)([0-9]{1})|\b?(1)([0-2]{1}))"
  twoDigitDay <- "(\b?(0)([0-9]{1})|\b?(1)([0-9]{1})|\b?(2)([0-9]{1})|30|31)"
  
  lapply(sep, function(sepi){
    REGEXPATS <- paste0(fourDigitYr, sepi, twoDigitMonth, sepi, twoDigitDay)
    REGEXPATS2 <- paste0(twoDigitMonth, sepi, twoDigitDay, sepi, fourDigitYr)
    REGEXPATS_1_2 <- paste0(paste0("(", REGEXPATS, ")"), "|", paste0("(", REGEXPATS2, ")"))
    stringr::str_extract_all(string, REGEXPATS_1_2)
  })
  # lapply(sep, function(sepi){
  #   REGEXPATS <- paste0(twoDigitMonth, sepi, twoDigitDay, sepi, fourDigitYr)
  #   stringr::str_extract_all(string, REGEXPATS)
  # })
} 


#' Samantha Rhoads's function to extract a date of 6 digits from a string. `sep` is the character that separates parts of a date, ie: dash or slash or nothing. A 6 digit date is like "01-2019" or "202001" or "01/2020" 
#' Srhoads wrote this to allow you to extract a date of 8 digits from a string, ie: `extract_sixDigitDate(string="I am Sam and I was born on 01-1996. Today is 202001.", sep=c("-", "\\.", "/", ""))` returns a hierarchical list with: "01-1996" and "202001" in it at various places 
#' @export
#' @examples
#' extract_sixDigitDate(string, sep=c("-", "\\.", "/", ""))
extract_sixDigitDate <- function(string, sep=c("-", "\\.", "/", "")){
  fourDigitYr <- fourDigitYr_1900sOr2000s <- "(\b?(19|20)([0-9]{2}))"
  twoDigitMonth <- "(\b?(0)([0-9]{1})|\b?(1)([0-2]{1}))"
  
  lapply(sep, function(sepi){
    REGEXPATS <- paste0(fourDigitYr, sepi, twoDigitMonth)
    REGEXPATS2 <- paste0(twoDigitMonth, sepi, fourDigitYr)
    REGEXPATS_1_2 <- paste0(paste0("(", REGEXPATS, ")"), "|", paste0("(", REGEXPATS2, ")"))
    stringr::str_extract_all(string, REGEXPATS_1_2)
  })
} 

#' Samantha Rhoads's function to extract a year of 4 digits. Can be in the 1900s or 2000s only, or can include centuries before that
#' @export
#' @examples
#' extract_fourDigitYear(string, includePre1900s=T)
extract_fourDigitYear <- function(string, includePre1900s=T){
  fourDigitYr <- fourDigitYr_1900sOr2000s <- "(\b?(19|20)([0-9]{2}))"
  if(includePre1900s) fourDigitYr <- "(\b?(1)([0-9]{3}))|(\b?(19|20)([0-9]{2}))"
  stringr::str_extract_all(string, fourDigitYr)
} 

###################################################################################################################################################



# 03 02, 2020 (20200302) ##########################################################################################################################


#' Samantha Rhoads's function to extract or keep or select elements of a VECTOR. Invert means keep everything other than the pattern. Default pattern is everything
#' @export
#' @examples
#' select_vec2(x, pat=".*", invert=F, ignore.case=F)
select_vec2 <- function(v, pattern=".*", ignore.case=T, everything=F, invert=F){
  `%>%` <- magrittr::`%>%`
  if(invert) {yesornot <- `!`; plusorminus <- `-`} else {yesornot <- function(x) x; plusorminus <- function(x) x}
  tv <- tryCatch(tibble::as_tibble(t(tibble::as_tibble(v))) %>% setNames(v), error=function(e) v)
  tv <- tryCatch({
    if(everything){
      if(invert) {
        tv <- tv %>% dplyr::select(-dplyr::matches(paste_regex(pattern), ignore.case = ignore.case), dplyr::everything()) %>% names()
      } else {
        tv <- tv %>% dplyr::select(-dplyr::matches(paste_regex(pattern), ignore.case = ignore.case), dplyr::everything()) %>% names()
      }
    } else {
      if(invert) {
        tv <- tv %>% dplyr::select(-dplyr::matches(paste_regex(pattern), ignore.case = ignore.case)) %>% names()
      } else {
        tv <- tv %>% dplyr::select(dplyr::matches(paste_regex(pattern)), ignore.case = ignore.case) %>% names()    }
    }
    tv
  }, 
  error=function(e){
    if(!is.null(names(tv))){
      tv <- tv %>% purrr::keep(yesornot(grepl(pattern, names(.), ignore.case = ignore.case))) %>% names()
    } else {
      tv <- tv %>% purrr::keep(yesornot(grepl(pattern, ., ignore.case = ignore.case)))
    }
    tv
  })
  tv
}


#' Samantha Rhoads's function to extract or keep or select elements of a list, dataframe, or vector with a regular expression. Invert means keep everything other than the pattern. Default pattern is everything
#' @export
#' @examples x <- as.list(iris); select_matches(x, pat="Petal"); v <- iris$Species; select_matches(v, pat="versi")
#' select_matches(x, pat=".*", invert=F, ignore.case=F)
select_matches <- select_list_or_other <- function(x, pat=".*", invert=F, ignore.case=F, escape_chars_in_pattern=F){
  pattern <- pat
  `%>%` <- magrittr::`%>%`
  if(invert) {yesornot <- `!`; plusorminus <- `-`} else {yesornot <- function(x) x; plusorminus <- function(x) x}
  # if(invert) {yesornot <- function(y) `!`; plusorminus <- function(y) `-`} else {yesornot <- function(y) ``; plusorminus <- function(y) ``}
  if(is.list(x)&!is.data.frame(x)){
    x %>% purrr::keep(yesornot(grepl(pat, names(.), ignore.case = ignore.case)))
  } else if(is.data.frame(x)){
    x %>% dplyr::select(plusorminus(dplyr::matches(pat, ignore.case = ignore.case)))
  } else if(is.vector(x)|is.factorchar(x)|depth(x)==0){
    if(!is.null(names(x))){
      x %>% purrr::keep(yesornot(grepl(pattern, names(.), ignore.case = ignore.case))) %>% names()
    } else {
      x %>% purrr::keep(yesornot(grepl(pattern, ., ignore.case = ignore.case)))
    } # x %>% select_vec2(., pattern=pat, invert=invert, ignore.case=ignore.case)
  } else {
    print("object doesn't match anything for select_matches() to work on")
    x
  }
}

#' Samantha Rhoads's function to extract or keep or select elements of a list, dataframe, or vector with a regular expression. Invert means keep everything other than the pattern. Default pattern is everything
#' @export
#' @examples x <- as.list(iris); select_matches(x, pat="Petal"); v <- iris$Species; select_matches(v, pat="versi")
#' select_matches_everything(x, pat=".*", invert=F, ignore.case=F)
select_matches_everything <- function (x, pat = ".*", invert = F, ignore.case = F, everything = F) {
  pattern <- pat
  `%>%` <- magrittr::`%>%`
  if(invert) {yesornot <- `!`; plusorminus <- `-`} else {yesornot <- function(x) x; plusorminus <- function(x) x}
  # if(invert) {yesornot <- function(y) `!`; plusorminus <- function(y) `-`} else {yesornot <- function(y) ``; plusorminus <- function(y) ``}
  if(is.list(x)&!is.data.frame(x)){
    x2 <- x %>% purrr::keep(yesornot(grepl(pat, names(.), ignore.case = ignore.case)))
    if(everything){c(x2, x %>% purrr::keep(!yesornot(grepl(pat, names(.), ignore.case = ignore.case))))} else {x2}
  } else if(is.data.frame(x)){
    x %>% dplyr::select(plusorminus(dplyr::matches(pat, ignore.case = ignore.case)))
  } else if(is.vector(x)|is.factorchar(x)|depth(x)==0){
    if(!is.null(names(x))){
      x %>% purrr::keep(yesornot(grepl(pattern, names(.), ignore.case = ignore.case))) %>% names()
    } else {
      x %>% purrr::keep(yesornot(grepl(pattern, ., ignore.case = ignore.case)))
    } # x %>% select_vec2(., pattern=pat, invert=invert, ignore.case=ignore.case)
  } else {
    print("object doesn't match anything for select_matches() to work on")
    x
  }
}

#' Samantha Rhoads's function to do `setdiff()` from both sides and print which input the different strings come from
#' @export
#' @examples
#' setdiff_(x, y, printWhichOnly=F)
setdiff_ <- function(x, y, returnWhichOnly=T, printWhichOnly=F, sortItems=F){
  xonly <- setdiff(x, y)
  yonly <- setdiff(y, x)
  if(sortItems){
    xonly <- sort(xonly)
    yonly <- sort(yonly)
  }
  if(printWhichOnly){
    if(length(xonly>0)) catn("xonly: ", paste0(xonly, collapse="        "))
    if(length(yonly>0)) catn("yonly: ", paste0(yonly, collapse="        "))
  }
  if(returnWhichOnly){
    res <- list(xonly=xonly,
                yonly=yonly)
  } else {
    res <- unique(c(xonly, yonly))
  }
  res
}


###################################################################################################################################################


# 04 05, 2020 (20200405) ##########################################################################################################################

#' Samantha Rhoads's function to do `setdiff()` from both sides and print which input the different strings come from
#' @export
#' @examples
#' reticulate_correctly(x, y, printWhichOnly=F)
reticulate_correctly <- function(){
  reticulate::use_python("/usr/local/bin/python3", required=T)
  reticulate::repl_python()
}
###################################################################################################################################################


# 05 28, 2020 (20200528) ##########################################################################################################################

#' Samantha Rhoads's function to unlist/flatten a list one time, but from the top, instead of the bottom (aka the opposite of `unlist()` or purrr's `flatten()`)
#' @export
#' @examples
#' flatten_list_from_top(L)
flatten_list_from_top <- function(L){
  Reduce(f=c, x=L)
}

#' Samantha Rhoads's function to compare the columns of two lists of dataframes to see their differences based on their `sumry()`s and if the column `sumry()`s are `identical()`
#' @export
#' @examples
#' compare_two_lods(list1 = list(df1 = dfsampler()), list2 =  list(df2 = dfsampler(which="short")), exampleOfColumnName="^name$")
compare_two_lods <- function(list1 = list(df = dfsampler()), list2 =  list(df = dfsampler(which="short")), exampleOfColumnName="^name$"){
  
  list1 %<>% .[sort(names(.))]
  list2 %<>% .[sort(names(.))]
  
  (BOTHLISTNAMES <- union(names(list1), names(list2)))
  
  (MAXLISTLENGTH <- max(length(list1), length(list2)))
  
  RETURN0 <- lapply(1:MAXLISTLENGTH, function(i){ # i <- 1
    dfname_of_interest <- BOTHLISTNAMES[i]
    # BOTHDATASETS <- list(OLD = list1[[i]], NEW = list2[[i]])
    # BOTHDATASETS <- list(OLD = list1[[dfname_of_interest]], NEW = list2[[dfname_of_interest]])
    BOTHDATASETS <- list(OLD = select_matches(list1, pat=paste0("^", dfname_of_interest, "$"), ignore.case = T) %>% flatten_list_from_top(), 
                         NEW = select_matches(list2, pat=paste0("^", dfname_of_interest, "$"), ignore.case = T) %>% flatten_list_from_top())
    
    MAXNROW <- BOTHDATASETS %>% purrr::map_if(is.data.frame, function(d) nrow(d)) %>% unlist() %>% max(., na.rm = T)
    BOTHDFS <- BOTHDATASETS %>%
      purrr::map_if(is.data.frame, function(d){
        headerRow <- grep_all_df(exampleOfColumnName, d[1:min(c(300, nrow(d))), ], rownums_only=T, ignore.case = T) %>% unique()
        if(length(headerRow)>0){
          headerRow %<>% .[[1]]
          headerNames <- d %>% slice(headerRow) %>% unlist() %>% replace_na(., "na") %>% gsub('NA', "na", .) %>% make.unique()
          d %<>% setNames(make.unique(headerNames)) %>% slice(-(1:headerRow))
        }
        d 
      })
    BOTHLISTDFNAMES <- BOTHDFS %>% purrr::map(names) %>% unlist() %>% unique()
    
    lapply(BOTHLISTDFNAMES, function(ii){ # ii <- "lastname"      #    ii <- "gender"
      ugh <- list(
        hcOLD = (
          BOTHDFS$OLD %>% data.frame() %>% dplyr::select(dplyr::one_of(ii)) %>% .[1:MAXNROW, ] %>% data.frame() %>% 
            # {if(ncol(.)==0) {.[[ii]] <- NA; .} else .} #slice(1:MAXNROW)
            {if(ncol(.)>0) setNames(., paste0(ii, " (OLD)")) else .}#slice(1:MAXNROW)
        ),
        hcNEW = (
          BOTHDFS$NEW %>% data.frame() %>% dplyr::select(dplyr::one_of(ii)) %>% .[1:MAXNROW, ] %>% data.frame() %>% #setNames(paste0(ii, " (NEW)")) %>%
            {if(ncol(.)>0) setNames(., paste0(ii, " (NEW)")) else .} #slice(1:MAXNROW)
        )
      ) %>% dplyr::bind_cols() %>% sumry(., min(nrow(unique(.)), 21, na.rm=T))
      ugh
    }) 
    
  }) %>% 
    setNames(BOTHLISTNAMES) 
  
  RETURN <- tryCatch({
    RETURN0 %>%
      {
        comparedData <- . # comparedData <- RETURN0
        purrr::map(1:length(RETURN0), function(i){ # i <- 1
          comparedColNames <- comparedData[[i]] %>% purrr::map(., function(x){  # x <- comparedData[[i]][[4]]
            if(is.null(colnames(x))){
              new_colnames0 <- paste0(F, " bc col missing from one of OR BOTH of the dfs")
            } else {
              new_colnames0 <- colnames(x) %>% gsub(" \\(OLD\\)|\\(NEW\\)", "", .) %>% trimws_() %>% unique() %>% .[[1]]
            }
            if(ncol(x)>1){
              cols_identical_or_no <- identical(x[, 1], x[, 2])
            } else {
              cols_identical_or_no <- paste0(F, " bc col missing from one of the dfs")
            }
            paste0(new_colnames0, " (identical=", cols_identical_or_no, ")")
          }) %>% unlist()
          comparedData[[i]] %<>% setNames(comparedColNames)
          comparedData[[i]]
        }) %>% setNames(names(comparedData))
      }
  }, error = function(e) RETURN0)
  
  RETURN
}
###################################################################################################################################################


#' Samantha Rhoads's function to compare the columns of two lists of dataframes to see their differences based on their `sumry()`s and if the column `sumry()`s are `identical()`
#' @export
#' @examples list1=list(nvc=NursesVC); list2=list(nvc=NursesVC2); exampleOfColumnName="EEID"; print_only_differences=T
#' compare_two_lods_enhanced(list1 = list(df1 = dfsampler()), list2 =  list(df2 = dfsampler(which="short")), exampleOfColumnName="^name$")
compare_two_lods_enhanced <- function(list1 = list(df = dfsampler()), list2 =  list(df = dfsampler(which="short")), exampleOfColumnName="^name$", print_only_differences=T){
  
  list1 %<>% .[sort(names(.))]
  list2 %<>% .[sort(names(.))]
  
  (BOTHLISTNAMES <- union(names(list1), names(list2)))
  
  (MAXLISTLENGTH <- max(length(list1), length(list2)))
  
  RETURN0 <- lapply(1:MAXLISTLENGTH, function(i){ #{i=1}
    dfname_of_interest <- BOTHLISTNAMES[i]
    # BOTHDATASETS <- list(OLD = list1[[i]], NEW = list2[[i]])
    # BOTHDATASETS <- list(OLD = list1[[dfname_of_interest]], NEW = list2[[dfname_of_interest]])
    BOTHDATASETS <- list(OLD = select_matches(list1, pat=paste0("^", dfname_of_interest, "$"), ignore.case = T) %>% flatten_list_from_top(), 
                         NEW = select_matches(list2, pat=paste0("^", dfname_of_interest, "$"), ignore.case = T) %>% flatten_list_from_top())
    
    MAXNROW <- BOTHDATASETS %>% purrr::map_if(is.data.frame, function(d) nrow(d)) %>% unlist() %>% max(., na.rm = T)
    if(!is.nanull(exampleOfColumnName)){
      BOTHDFS <- BOTHDATASETS %>%
        purrr::map_if(is.data.frame, function(d){
          headerRow <- grep_all_df(exampleOfColumnName, d[1:min(c(300, nrow(d))), ], rownums_only=T, ignore.case = T) %>% unique()
          if(length(headerRow)>0){
            headerRow %<>% .[[1]]
            headerNames <- d %>% slice(headerRow) %>% unlist() %>% replace_na(., "na") %>% gsub('NA', "na", .) %>% make.unique()
            d %<>% setNames(make.unique(headerNames)) %>% slice(-(1:headerRow))
          }
          d 
        })
    } else {
      BOTHDFS <- BOTHDATASETS
    }
    
    
    BOTHDFS <- tryCatch({
      BOTHDFS %>%
        lapply(., function(d){
          d$nrows_in_dataset_comparison <- NA
          d$nrows_in_dataset_comparison[1] <- nrow(d)
          d %>% select(one_of("nrows_in_dataset_comparison"), everything())
        })
      }, error=function(e){print(e); BOTHDFS})
    
    BOTHLISTDFNAMES <- BOTHDFS %>% purrr::map(names) %>% unlist() %>% unique()
    
    BOTHCOMPARISON <- lapply(BOTHLISTDFNAMES, function(ii){ # ii <- "lastname"      #    ii <- "gender"
      ugh <- list(
        hcOLD = (
          BOTHDFS$OLD %>% data.frame() %>% dplyr::select(dplyr::one_of(ii)) %>% mutate_if(is.factorchar, function(v){replace_na(v, "")}) %>% .[1:MAXNROW, ] %>% data.frame() %>% 
            # {if(ncol(.)==0) {.[[ii]] <- NA; .} else .} #slice(1:MAXNROW)
            {if(ncol(.)>0) setNames(., paste0(ii, " (OLD)")) else .}#slice(1:MAXNROW)
        ),
        hcNEW = (
          BOTHDFS$NEW %>% data.frame() %>% dplyr::select(dplyr::one_of(ii)) %>% mutate_if(is.factorchar, function(v){replace_na(v, "")}) %>% .[1:MAXNROW, ] %>% data.frame() %>% #setNames(paste0(ii, " (NEW)")) %>%
            {if(ncol(.)>0) setNames(., paste0(ii, " (NEW)")) else .} #slice(1:MAXNROW)
        )
      ) %>% dplyr::bind_cols() %>% sumry(., min(nrow(unique(.)), 21))
      ugh
    }) 
    
    BOTHCOMPARISON
    
  }) %>% 
    setNames(BOTHLISTNAMES) 
  
  RETURN <- tryCatch({
    RETURN0 %>%
      {
        comparedData <- . # {comparedData <- RETURN0}
        purrr::map(1:length(RETURN0), function(i){ # {i=1}
          comparedColNames <- comparedData[[i]] %>% purrr::map(., function(x){  # {x <- comparedData[[i]][[1]]}
            if(is.null(colnames(x))){
              new_colnames0 <- paste0(F, " bc col missing from one of OR BOTH of the dfs")
            } else {
              new_colnames0 <- colnames(x) %>% gsub(" \\(OLD\\)|\\(NEW\\)", "", .) %>% trimws_() %>% unique() %>% .[[1]]
            }
            if(ncol(x)>1){
              cols_identical_or_no <- identical(x[, 1], x[, 2])
            } else {
              cols_identical_or_no <- paste0(F, " bc col missing from one of the dfs")
            }
            paste0(new_colnames0, " (identical=", cols_identical_or_no, ")")
          }) %>% unlist()
          comparedData[[i]] %<>% setNames(comparedColNames)
          try({
            nrows_in_dataset_comparison_colname <- comparedData[[i]] %>% names() %>% select_matches("nrows_in_dataset_comparison") %>% .[[1]]
            comparedData[[i]][[nrows_in_dataset_comparison_colname]] <- comparedData[[i]][[nrows_in_dataset_comparison_colname]] %>% .[6,]
          })
          comparedData[[i]]
        }) %>% setNames(names(comparedData))
      }
  }, error = function(e) {
    print(e)
    RETURN0
  })
  
  if(print_only_differences){
    RETURN %<>% lapply(., function(l){ # l <- RETURN[[1]]
      l <- l[grep("identical=FALSE", names(l))]
      l <- drop_empty(l)
      if(length(l)==0){
        l <- "Nothing different between the columns shared by both lists of dataframes"
      }
      l
    })
  } else {
    RETURN %<>% lapply(., function(l){ # l <- RETURN[[1]]
      l <- drop_empty(l)
      if(length(l)==0){
        l <- "Nothing different between the columns shared by both lists of dataframes"
      }
      l
    })
  }
  
  RETURN
}
###################################################################################################################################################

# 07 30, 2020 (20200730) ##########################################################################################################################

#' Samantha Rhoads's function to visualize hex color codes
#' @export
#' @examples
#' preview_hex_colors(hexs = c("#b41e3b", "#337ab7", "#f37036", "#434447", "#37718e", "#da3051", "#f5f6f8"))
preview_hex_colors <- function(hexs = c("#b41e3b", "#337ab7", "#f37036", "#434447", "#37718e", "#da3051", "#f5f6f8", "#9a2138", "#ffffff", "#333333"), plotly=F){
  hexs <- rev(unique(hexs))
  temp <- setNames(rep(1, length(hexs)), hexs)
  
  if(plotly){
    P <- plotly::plot_ly(y=hexs, x=temp, type="bar", showlegend=F, size=4, text=hexs, textposition="inside", marker=list(color=hexs, line=list(color='rgb(8,48,107)', width=1.5))) %>% 
      layout(xaxis=list(showticklabels=F), 
             yaxis=list(tickwidth=1, tickfont=list(size=18)),
             margin = list(
               r = 20, t = 25, b = 40, l = 20, pad=20
             )) %>% plotly::config(displayModeBar=F, staticPlot=T)
  } else {
    par(las=2)
    par(mar=c(.5,5,.5,0.1))
    P <- barplot(temp, col=hexs, axisnames=T, horiz=T, border=NA, axes=F, cex.names=1)
  }
  P
}

#' Samantha Rhoads's function to assign column names by skipping the appropriate number of rows in a dataframe before the actual column names. Uses a reference name.
#' @export
#' @examples
#' set_names_skip_rows_until_match(d, example_colname="Employee ID", check_n_rows=100)
set_names_skip_rows_until_match <- function(d, example_colname="Employee ID", check_n_rows=100, doEvenIfColnameIsAlreadyIt=F){
  if (!(example_colname %in% names(d))|doEvenIfColnameIsAlreadyIt) {
    colnames_rownum <- grep_all_df(example_colname, d[1:check_n_rows, ], rownums_only=T)[1]
    dNewNames <- as.character(d[colnames_rownum, ]) %>% gsub('^$|\\`', 'UNNAMED', .) %>% replace_na(., "NA.0") %>% make.unique()
    if ((length(colnames_rownum)>0)&!is.na(colnames_rownum)) {
      d <- d %>% setNames(dNewNames) %>% slice(-(1:colnames_rownum))
    }
  }
  return(d %>% setNames(make.unique(names(.))))
}



#' Samantha Rhoads's function to assign column names by skipping the appropriate number of rows in a dataframe before the actual column names. Uses a reference name.
#' @export
#' @examples
#' set_names_skip_rows_until_match_loop(d, patterns=c('census_code','census_title', 'occp_code'), exact=F, check_n_rows=30, doEvenIfColnameIsAlreadyIt=F)
set_names_skip_rows_until_match_loop <- function (d, patterns=c('census_code','census_title', 'occp_code'), exact=F, check_n_rows=30, doEvenIfColnameIsAlreadyIt=F) {
  for (PATTERN in patterns){ # {PATTERN = patterns[1]}
    # if ((all(grepl("^(x|na_|NA\\.)[[:digit:]]|^\\.\\.\\.|^NA\\b", names(d)))) & (!(tolower(PATTERN) %in% tolower(names(d)))|doEvenIfColnameIsAlreadyIt)) {
    if ((all(grepl("^(x|na_|NA\\.)[[:digit:]]|^\\.\\.\\.|^NA\\b", names(d)))) | (!(tolower(PATTERN) %in% tolower(names(d)))|doEvenIfColnameIsAlreadyIt)) {
        colnames_rownum <- grep_all_df(PATTERN, d[1:check_n_rows, ], rownums_only = T, exact=exact)[1]
      dNewNames <- make.unique(as.character(d[colnames_rownum, ])) %>% replace_na(., "NA.0")
      if ((length(colnames_rownum) > 0)&!is.na(colnames_rownum)) {
        d <- d %>% setNames(dNewNames) %>% slice(-(1:colnames_rownum))
        catn("set_names_skip_rows_until_match_loop() names changed to: c(", paste0("'", names(d), "'", collapse=","), ")")
      } else{
        cat("\nset_names_skip_rows_until_match_loop() no match for... PATTERN = '", PATTERN, "'", sep="")
      }
    }
  }
  d %>% setNames(make.unique(names(.)))
}

#' Samantha Rhoads's function to unload packages
#' @export
#' @examples
#' unload_pkg(pkgs)
unload_pkg <- function(pkgs){ # pkgs <- c("srhoads", "ggmap")
  detach_unload_true <- function(name, pos = 2L, unload = T, character.only = T, force=F) {
    detach(name, pos = pos, unload = unload, character.only = character.only, force = force)
  }
  pkgsstrarg <- paste0("package:", pkgs)
  if(length(pkgs)==1){
    while(pkgsstrarg %in% search()){
      detach(pkgsstrarg, unload=T, character.only=T)
    }
  } else {
    lapply(pkgsstrarg, function(pkgstrarg) {
      while(pkgstrarg %in% search()){
        detach(pkgstrarg, unload=T, character.only=T)
      }
    })
  }
}

#' Samantha Rhoads's function to convert a windows path to a MAC path (specifically for the Y drive aapdata Volume mount)
#' @export
#' @examples
#' windows2macPath(path)
windows2macPath <- function(path='Y:\\AA_Secured\\CVS\\2020\\2020 Plan Prep\\Aetna\\PDF Files for Sam\\Narrative Information'){
  path <- gsub("\\\\", "/", path)
  path <- gsub("Y\\:|Y\\:\\/", "/Volumes/aapdata/", path)
  gsub("\\//", "/", path)
}


#' Samantha Rhoads's function to fuzzy-match (agrepl) a string to a dataframe column, returning a dataframe of matches with whichever other variables exist in the input dataframe
#' @export
#' @examples
#' fuzzy_match_rank(s="Data Scientist", strictest_max_distance=0, seqstep=.01, df, dfvartocompare="job", stop_at_n_matches=Inf)
fuzzy_match_rank <- function(s="Data Scientist", strictest_max_distance=0, seqstep=.01, df, dfvartocompare="job", stop_at_n_matches=Inf){
  df_copy <- df    # {s="samantha karlaina rhoads"; df=dfsampler(); dfvartocompare="name"; strictest_max_distance=0; seqstep=.01; stop_at_n_matches=5}
  df_copy[["DFVAR_TO_COMPARE"]] <- df_copy[[dfvartocompare]]
  INDICATOR_STRINGS <- tolower(trimws(strip_punct(s, replacewith=" "))) %>% c(paste0("^", ., "$"), .)
  MAX_DISTANCES <- unique(c(strictest_max_distance, seq(0, 1, seqstep)))
  matched_value <- tibble()
  for (INDICATOR_STRING in INDICATOR_STRINGS){     # {INDICATOR_STRING = INDICATOR_STRINGS[1]}
    if(nrow(matched_value)<stop_at_n_matches){     #catn("INDICATOR_STRING=", INDICATOR_STRING)
      for (MAX_DISTANCE in MAX_DISTANCES){         # {MAX_DISTANCE = MAX_DISTANCES[1]}
        if(nrow(matched_value)<stop_at_n_matches){ #catn("MAX_DISTANCE=", MAX_DISTANCE)
          if(MAX_DISTANCE==0 & grepl("\\^.*\\$", INDICATOR_STRING)){
            matched_value %<>% bind_rows(., dplyr::filter(df_copy, grepl(INDICATOR_STRING, DFVAR_TO_COMPARE, ignore.case=T)) %>% mutate(similarity = 1-MAX_DISTANCE))
            df_copy %<>% filter(., !grepl(INDICATOR_STRING, DFVAR_TO_COMPARE, ignore.case=T))
          } else if(!grepl("\\^.*\\$", INDICATOR_STRING)){
            matched_value %<>% bind_rows(., dplyr::filter(df_copy, agrepl(INDICATOR_STRING, DFVAR_TO_COMPARE, max.distance = c(all=MAX_DISTANCE))) %>% mutate(similarity = 1-MAX_DISTANCE))
            df_copy %<>% filter(., !agrepl(INDICATOR_STRING, DFVAR_TO_COMPARE, max.distance = list(all=MAX_DISTANCE)))
          }
        }
      }
    }
  }
  return(arrange(matched_value, desc(similarity), nchar(DFVAR_TO_COMPARE)))
}

#' Samantha Rhoads's function to get relevant, preferred, desired column from dataframe on a loop
#' @export
#' @examples
#' df_get_preferred_column(df, patterns=c('DateOpened', 'Date.*Opened'), ignore.case=T, fillmissingwith=NA, returnNameOnly=F, exactEnd=F, exactStart=F)
df_get_preferred_column <- function(df, patterns=c('DateOpened', 'Date.*Opened'), ignore.case=T, fillmissingwith=NA, returnNameOnly=F, exactEnd=F, exactStart=F, verbose=F){
  newcolname <- c()
  for (pattern in patterns){ # {pattern = patterns[1]}
    if (length(newcolname)==0){
      if(exactEnd){
        pattern <- paste0(pattern, "$")
      }
      if(exactStart){
        pattern <- paste0("^", pattern)
      }
      newcolname <- names(select(df, matches(pattern, ignore.case=ignore.case)))
    }
  }
  if(returnNameOnly){
    dfdesiredcolumn <- newcolname[[1]]
  } else {
    dfdesiredcolumn <- if(length(newcolname)==0){fillmissingwith} else {df[[newcolname[[1]]]]}  #[fillmissingwith]*len(xls) if len(newcolname)==0 else xls[newcolname[0]]
  }
  
  if(verbose){
    message(paste0(newcolname[[1]], " ==> ", newcolname))
  }
  dfdesiredcolumn
}


#' Samantha Rhoads's function to trycatch reading excel data from a file or list of files
#' @export
#' @examples
#' try_read_excel_somesheets(fns = NULL, keepshtvec = NULL, na = c("NA", "None", "N/A", "-", ""), col_types = "text", skip = 0)
try_read_excel_somesheets <- function (fns = NULL, keepshtvec = NULL, na = c("NA", "None", "N/A", "-", ""), col_types = "text", skip = 0) {
  if (is.null(fns)) (fns <- list.files(pattern = "\\.xlsx", recursive = T, full.names = T))
  if (is.null(keepshtvec)) keepshtvec <- lapply(fns, function(v) tryCatch(readxl::excel_sheets(v), error=function(e) 'ERROR')) %>% unlist() %>% unique()
  (fnshtlst <- lapply(fns, function(s) tryCatch(readxl::excel_sheets(s), error=function(e) paste0("ERROR ", as.character(e)))) %>%  setNames(fns))
  (keepshts <- lapply(fnshtlst, function(v) v[(v %in% keepshtvec) == T]))
  lapply(1:length(keepshts), function(i) {
    f <- names(keepshts[i])
    shts <- keepshts[[i]]
    (d <- lapply(shts, function(sht) tryCatch(readxl::read_excel(f, sheet = sht, skip = skip, na = na, col_types = col_types), error=function(e) tibble(ERROR = f))) %>% setNames(shts))
  }) %>% setNames(fns)
}

#' Samantha Rhoads's function to create a column in a dataframe if it doesn't already exist; defaults to all NA values
#' @export
#' @examples
#' mutate_col_if_not_exists(d, var='id')
mutate_col_if_not_exists <- function(d, var='YOS_Y'){
  if(is.null(d[[var]])){
    d[[var]] <- NA
  }
  d
}
###################################################################################################################################################


# 03 26, 2021 (20210326) ##########################################################################################################################


#' This function is same as as.numeric or readr::parse_number but u can do it on more than just a character vector + u can suppress the annoying warnings
#' @export
#' @examples
#' as_numeric(v, suppresswarnings=T)
as_numeric <- function(v, suppresswarnings=T){
  v_ <- if(suppresswarnings){suppressWarnings(readr::parse_number(as.character(v)))} else {readr::parse_number(as.character(v))}
  return(v_)
}

#' This function is a system wrapper to delete silly .DS_Store files
#' @export
#' @examples
#' rm_ds_store(v, suppresswarnings=T)
rm_ds_store <- function(){
  system("rm -f .DS_Store")
}


#' Samantha Rhoads's function to
#' @export
#' @examples
#' dbDisconnectAll(drivername='PostgreSQL')
dbDisconnectAll <- function(drivername='PostgreSQL'){
  (cons <- DBI::dbListConnections(DBI::dbDriver(drivername))); # RPostgres::dbDisconnect(cons[[3]])
  lapply( cons, function(x) DBI::dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n",  length(cons)))
}

#' Samantha Rhoads's function to connect to an azure file share + return specific parameters or objects depending on what you want
#' @export
#' @examples
#' connect_to_azure_file_share(share_name="somestring", storage_name="somestringalso", azure_key="somelongstringugetfromazureportal", return_share=F)
connect_to_azure_file_share <- function(share_name="somestring", storage_name="somestringalso", azure_key="somelongstringugetfromazureportal", return_share=F){
  pkg("AzureRMR", "AzureStor")
  endp_link <- paste0("https://", storage_name, ".file.core.windows.net/") 
  share_link <- paste0(endp_link, share_name)
  file_endp <- file_endpoint(endp_link, key=azure_key)
  share <- file_share(share_link, key=azure_key)
  if(return_share) {
    return(share)
  } else {
    container <- storage_container(file_endp, name=share_name) # fileStores <- list_azure_files(container))$name
    return(container)
  }
}

#' Samantha Rhoads's function to drop rows where not enough columns have data. IE: if you have 3 columns in a df but require at least 2 columns to have values, then you drop a row where two+ values are NA
#' @export
#' @examples
#' drop_rows_n_cols_not_na(x, min_nonna_cols=2)
drop_rows_n_cols_not_na <- function (x, min_nonna_cols=2) {
  x[rowSums(!is.na(x)) >= min_nonna_cols, ]
}

#' Samantha Rhoads's function to tell the shell to open a file in whatever its default program is
#' @export
#' @examples
#' system_open(paths)
system_open <- open_system <- function(paths){
  lapply(paths, function(s){
    cmdstr <- paste0('open "', s, '"')
    system(cmdstr)
  })
  return(paths)
}


#' Samantha Rhoads's function to get the column letter of a number
#' @export
#' @examples
#' get_column_letter(v)
get_column_letter <- function(v, get_number_from_letter=F){
  lett <- toupper(letters)
  numb <- as.character(1:length(lett))
  if(get_number_from_letter){
    result <- numb[match(toupper(v), lett)]
  } else {
    result <- lett[match(v, numb)]
  }
  if(is.na(result)){
    lett <- c(lett, lapply(lett, function(s) paste0(s, lett)) %>% unlist())
    numb <- as.character(1:length(lett))
    result <- lett[match(v, numb)]
  }
  return(result)
}

#' Samantha Rhoads's function to split a dataframe by specified row indeces (numbers)
#' @export
#' @examples
#' split_by_index(d, index=c(5, 10, 15))
split_by_index <- function(d, index=c(5, 10, 15)){
  split(d, cumsum(1:nrow(d) %in% index)) 
}

#' Samantha Rhoads's function to write to Excel just like writexl::write_xlsx (same args) but you tell the shell to open the file (in Excel); fxn returns the dataframe object!
#' @export
#' @examples
#' writexl_open(x, path=tempfile(fileext=".xlsx"), col_names=T, format_headers=T, use_zip64=F)
writexl_open <- function(x, path=tempfile(fileext=".xlsx"), col_names=T, format_headers=T, use_zip64=F, open_file=T){
  if(!grepl("xls(x|)$", path, ignore.case=T)){path <- paste0(path, ".xlsx")}
  writexl::write_xlsx(x, path=path, col_names=col_names, format_headers=format_headers, use_zip64=use_zip64)
  if(open_file){system(paste0('open "', path, '"'))}
  return(x)
}


#' Samantha Rhoads's function to write to Excel with some common dataframe special formatting
#' @export
#' @examples
#' writexl_open_formatted(df=NULL, filename, open_file=T, max_colwidth=50, colwidthplus=0)
writexl_open_formatted <- function(x=NULL, filename=NULL, open_file=T, maxcolwidth=50, colwidthplus=0, freeze_after_col=c("^(EEID|eeid)$", 1)[2], autofilter=T, baseFontSize=11, clean_colnames=T, colnames_toupper=F, force_as_tempfile=F,
                                   sheet_args=list("1"=list(wrap_headers=T), 
                                                   "2"=list(wrap_all=F)
                                                   )
                                   ){ #{filename="Notes & Annotations/jackson_lewis_people-20220509-temp.xlsx"; df=jackson_lewis_people}
  # library(openxlsx)
  
  if(!exists("loadWorkbook")){
    load_unload_openxlsx <- T
    pkg('openxlsx')
  } else {
    load_unload_openxlsx <- F
  }
  
  if(!is.nanull(filename)){
    if(!grepl("xlsx$", filename, ignore.case=T)){
      filename <- paste0(filename, ".xlsx")
    }
  }
  
  if(is.nanull(filename)){
    filename=tempfile(fileext = ".xlsx")
  } else if(force_as_tempfile) {
    dirname_temp=dirname(tempfile(fileext = ".xlsx"))
    filename <- paste0(dirname_temp, "/", filename)
  }
  
  
  if(!file.exists(filename)|is.data.frame(x)|is.list(x)){
    if(is.list(x)&!is.data.frame(x)){
      x1 <- lapply(x, function(d){
        d <- if(clean_colnames){d %>% setNames(names(.) %>% gsub("_", " ", .) %>% trimws_())} else {d}
        d <- if(colnames_toupper){d %>% setNames(names(.) %>% toupper())} else {d}
        d
      })
    } else {
      x1 <- if(clean_colnames){x %>% setNames(names(.) %>% gsub("_", " ", .) %>% trimws_())} else {x}
      x1 <- if(colnames_toupper){x1 %>% setNames(names(.) %>% toupper())} else {x1}
    }

    writexl::write_xlsx(x1, filename)
  }
  sheetnames <- readxl::excel_sheets(filename)
  sheetindex <- 1:length(sheetnames)
  wb = #openxlsx::
    loadWorkbook(filename)
  for (sheet_id in sheetindex){
    sheetname <- sheetnames[sheet_id]
    if(length(sheetindex)>1&!is.data.frame(x)){
      wbdf <- x[[sheet_id]]
    } else if(!is.data.frame(x)){
      wbdf <- x[[1]]
    } else {
      wbdf <- x
    }
    # wbdf = if(!is.data.frame(x)){readxl::read_excel(filename, sheet=sheetname)} else {x}
    ## activeSheet(wb) <- sheetname
    if(is.numeric(freeze_after_col)){
      freeze_before_colnum <- freeze_after_col + 1
    } else if(lookslike_number(freeze_after_col)){
      freeze_before_colnum <- as.numeric(freeze_after_col) + 1
    } else {
      freeze_before_colnum <- grep(freeze_after_col, names(wbdf)) %>% max() %>% {.+1}
    }
    LabelStyle <- #openxlsx::
      createStyle(halign = "center", border=c("bottom"), borderStyle="thin", textDecoration="bold", 
                  wrapText=T, valign="center", fontSize=baseFontSize# fgFill = "#2020FF", fontColour = "white"
      )
    if(autofilter){# openxlsx::
      addFilter(wb, sheet=sheetname, row=1, cols=1:ncol(wbdf))
    }
    addStyle(wb, sheet=sheetname, style=createStyle(fontSize=baseFontSize), rows=(1:nrow(wbdf)+1), cols=1:ncol(wbdf), gridExpand=T, stack=T)
    # openxlsx::
    addStyle(wb, sheet=sheetname, style=LabelStyle, rows=1, cols=1:ncol(wbdf), stack=T)
    # freezePane(wb, 1, firstRow=T, firstCol=T)
    tryCatch({#openxlsx::
      freezePane(wb, sheet=sheetname, firstActiveRow=2, firstActiveCol=freeze_before_colnum)}, error=function(e){#openxlsx::
        freezePane(wb, sheet=sheetname, firstActiveRow=2, firstActiveCol=1)})
    
    if(clean_colnames){
      sheet_colnames <- colnames(wbdf) %>% gsub("_", " ", .) %>% trimws_()
    } else {
      sheet_colnames <- colnames(wbdf) 
    }
    width_vec1 <- apply(wbdf, 2, function(x){max(nchar(as.character(x))+1+colwidthplus, na.rm = TRUE)})
    width_vec2 <- sheet_colnames %>% sapply(., function(x){
      xw <- strsplit(x, split=" |[[:space:]]|-|\\.") %>% unlist() %>% trimws_() %>% as.list() %>% setNames(names(.)<-.) %>% nchar() %>% max()
      # xw <- map_chr(strsplit(x, " |[[:space:]]|-|\\."), ~ .[which.max(nchar(.))])
      xw+1+colwidthplus
    })
    width_vec <- tibble(width_vec1, width_vec2) %>% rowwise() %>% mutate(width_vec = max(width_vec1, width_vec2)) %>% .$width_vec %>% sapply(., function(n){min(maxcolwidth, n, na.rm=T)})
    # openxlsx::
    setColWidths(wb, sheet=sheetname, cols=1:ncol(wbdf), widths=width_vec)
    
    tryCatch({
      addl_args <- sheet_args[[sheet_id]]
      if(addl_args$wrap_all==T){
        addStyle(wb, sheet=sheetname, style=createStyle(wrapText=T), rows=(1:nrow(wbdf)+1), cols=1:ncol(wbdf), gridExpand=T, stack=T)
      }
    },
    error=function(e){
      NULL
    })
    
  }
  # openxlsx::
  saveWorkbook(wb, filename, overwrite=TRUE)
  if(open_file){system_open(filename)}
  if(load_unload_openxlsx){unload_pkg("openxlsx")}
  # wbdf
  x
}


#' Samantha Rhoads's function to write to Excel just like writexl::write_xlsx (same args) but you can specify Excel formulas to be written based on your current columns
#' @export
#' @examples
#' writexl_with_formulas(x, path = tempfile(fileext = ".xlsx"), formula="=total - white", newcol="minority", col_names = T, format_headers = T, use_zip64 = F, write=F)
writexl_with_formulas <- function(x, path = tempfile(fileext = ".xlsx"), formula="=total - white", newcol="minority", col_names = T, format_headers = T, use_zip64 = F, write=F){
  formula_matched_names <- stringr::str_extract_all(formula, paste0("\\b", names(x), "\\b", collapse="|")) %>% unlist()
  excel_ref_df <- dplyr::tibble("cols"=formula_matched_names) %>%
    dplyr::rowwise() %>% dplyr::mutate(col_num = which(colnames(x)==cols), col_letter = get_column_letter(col_num)) %>% dplyr::ungroup() %>%
    split(., .$cols)
  new_formula <- formula
  for (each in excel_ref_df){
    new_formula %<>% gsub(each$cols, each$col_letter, .)
  }
  if(!grepl("^\\=", new_formula)){new_formula <- paste0("=", new_formula)}
  # excel_formula <- hi
  x[[newcol]] <- x %>% dplyr::mutate(row_num = (1:nrow(.))+1) %>% dplyr::rowwise() %>% dplyr::mutate(newcol = gsub("(\\b[[:upper:]]{1,2})(\\b)", paste0("\\1", row_num, "\\2"), new_formula) %>% writexl::xl_formula()) %>% .$newcol
  if(write){
    writexl_open(x, path)
  }
  x
}


#' Samantha Rhoads's function to write a dataframe as an Excel sheet to currently existing Excel file (uses `XLConnect` package but not putting in dependencies or referencing it directly)
#' @export
#' @examples
#' xlsx_write_sheet(x, file, sheetName, col.names=T, row.names=F, append=T, overwrite=T, open_file=F)
xlsx_write_sheet <- function(x, file, sheetName, col.names=T, row.names=F, append=T, overwrite=T, open_file=F){
  pkg('XLConnect')
  # devtools::install_version("XLConnectJars", version = "0.2-12", repos = "http://cran.us.r-project.org") 
  # devtools::install_version("XLConnect", version = "0.2-12", repos = "http://cran.us.r-project.org")
  if(file.exists(file)){
    wb <- #XLConnect::
      loadWorkbook(file)
  } else{
    wb <- #XLConnect::
      loadWorkbook(file, create=T)
  }
  if(any(grepl(sheetName, (#xlsx::
    getSheets(wb))))&overwrite){
    # XLConnect::
    clearSheet(wb, sheetName)
    # XLConnect::
    writeWorksheet(wb, x, sheetName, startRow=1, startCol=1, header=T)
    cat('\ncurrent sheet cleared before overwriting\n')
  }
  if(is.data.frame(x)){
    x <- as.data.frame(x)
    # if(any("xl_formula" %in% unlist(sapply(x, class)))){XLConnect::setCellFormula()}
  } else {
    x <- lapply(x, function(d) as.data.frame(d))
  }
  
  for(colnum in 1:ncol(x)){
    colname <- names(x)[colnum]
    colvec <- x[[colnum]]
    if(any("xl_formula" %in% class(colvec))){
      for(rownum in 1:length(colvec)){ #{colnum=40}
        cellvalue <- gsub("^=", "",as.character(colvec[rownum]))
        # XLConnect::
        setCellFormula(wb, sheetName, row=rownum+1, col=colnum, formula=cellvalue)
      }
    }
  }
  saveWorkbook(wb, file)
  
  # write.xlsx2(x=x, file=file, sheetName=sheetName, col.names=col.names, row.names=row.names, append=append, keepFormulas=T, forceFormulaRefresh=T)
  if(open_file){system_open(file)}
  # forceFormulaRefresh(file, output = file, verbose = T)
  x
}



#' Samantha Rhoads's function to clean a vector of job titles (jlcentric)
#' @export
#' @examples
#' clean_jobtitle(v)
clean_jobtitle <- function(v){
  v %>%
    tolower() %>% strip_punct(., replacewith=" ") %>% trimws_() %>%
    gsub("(^| )[[:digit:]][[:digit:]]($| )", "", .) %>% trimws_() %>%
    gsub("[[:digit:]][[:digit:]][[:digit:]][[:digit:]]|^[[:digit:]][[:digit:]]$", "", .) %>%
    gsub("( |^)\\d{1,9}( |$)|now included in | \\d$|^\\d{1,9}[[:alpha:]]$", "", .)  %>% trimws_() %>% 
    gsub(".*accountable (for|to).*", "", .) %>% 
    gsub(".*job profile.*", "", .) %>% 
    gsub("^(r|g|)\\d{1,19}$", "", .) %>% 
    gsub("^(i|ii|iii|iv|v|vi)$|r\\d{8}", "", .) %>%
    gsub(" (filled|i|ii|iii|iv|f|h|m|r|s)$", "", .) %>%
    trimws_() %>% na_if(., "") %>% na_if(., "not found") %>% na_if(., "unknown")
}

#' Samantha Rhoads's function to change one record of one variable in a SQL database (jlcentric)
#' @export
#' @examples
#' edit_client_name(con, table='clientmanagement_client', CLIENT_NAME_TO_RENAME = "GROSSOLDNAME", CLIENT_NAME_NEW = "PLEASEWORKCOOLNAME")
edit_client_name <- function(con, table='clientmanagement_client', CLIENT_NAME_TO_RENAME = "GROSSOLDNAME", CLIENT_NAME_NEW = "PLEASEWORKCOOLNAME"){
  pkg('RPostgreSQL')
  UPDATE_ROW_QUERY <- paste0("update ", table, " set client_name='", CLIENT_NAME_NEW, "' WHERE client_name='", CLIENT_NAME_TO_RENAME, "'")
  update <- DBI::dbSendQuery(con, UPDATE_ROW_QUERY)
  postgresqlCloseResult(update)
}


#' Samantha Rhoads's function to preliminarily clean a vector of job groups (jlcentric)
#' @export
#' @examples
#' clean_job_group(v)
clean_job_group <- function(v){
  "job_group|job_code|job_group_name|desc|job"
  return(v %>% gsub("(^| )\\d{1,12}($| )", "", .))
}


#' Samantha Rhoads's function to pad 0s in front of string to make string a certain length
#' @export
#' @examples
#' recode_0s_pad_str(v)
recode_0s_pad_str <- function(v=c('1234'), length=5, empty_if_all_0s=T, only_numbers=F){
  v_ <- trimws_(as.character(v))
  v_ <- if(only_numbers) gsub("-|[a-zA-Z]", "", v_)  else v_
  v_ <- ifelse(nchar(v_)>=length, v_, stringr::str_sub(paste0(paste0(rep(0, length), collapse=''), v_), start=-length))
  return(v_)
}

#' Samantha Rhoads's function to pad 0s in front of string to make string a certain length
#' @export
#' @examples
#' recode_0s_pad_str(v)
pad_leading_0s <- recode_0s_pad_str

#' Samantha Rhoads's function to (jlcentric)
#' @export
#' @examples
#' pad_occp(v)
pad_occp <- function(v){
  ifelse(nchar(v)==3 & nchar(gsub("[[:digit:]]", "", v))==0, paste0("0", v), 
         ifelse(nchar(v)==2 & nchar(gsub("[[:digit:]]", "", v))==0, paste0("00", v),
                v))
}


#' Samantha Rhoads's function to crosswalk older OCCP (Census) Codes to the most current iteration (2018-2019) codes (jlcentric)
#' @export
#' @examples
#' crosswalk_occp_codes(v, remove_comma_sep=F)
crosswalk_occp_codes <- function(v, remove_comma_sep=F){
  v %>% # HOLISTIC CROSSWALK: 'https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2006-2010-acs-pums-occupation-conversion-rates.xlsx'
    dplyr::recode(., # 2000-2004 codes # https://usa.ipums.org/usa/volii/c2ssoccup.shtml
                  '0030'='0010',
                  '0130'='0135, 0136, 0137',
                  '0200'='0205',
                  '0210'='0205',
                  '0320'='4465, 0335, 0440, 0705',
                  '0400'='0440',
                  '0430'='0335, 0440, 0705',
                  
                  '0560'='0565, 3945',
                  '0620'='0630, 0640, 0650',
                  '0720'='0725',
                  '0730'='0735, 0705, 0750',
                  '1000'='1005, 1006',
                  '1107'='0705, 1108, 1065, 1022, 1032',
                  
                  '1040'='1050',
                  '1100'='1105',
                  '1110'='1106, 1007, 1031, 1032',
                  '1210'='1240',
                  '1230'='1240',
                  '1500'='1520',
                  '1510'='1530',
                  '1810'='0735',
                  '1830'='1860',
                  '1940'='1935', #'https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2006-2010-acs-pums-occupation-conversion-rates.xlsx'
                  '1950'='1970', #'https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2006-2010-acs-pums-occupation-conversion-rates.xlsx'
                  '1960'='1935, 1970',
                  '2020'='2015, 2016, 2025',
                  '2110'='2100',
                  '2140'='2145',
                  '2150'='2170, 2180, 2862',
                  '2820'='2825',
                  '3130'='3255, 3256, 3258',
                  '3240'='3245',
                  '3410'='3420',
                  '3530'='3535',
                  '3650'='3645, 3646, 3647, 3648, 3649, 3655',
                  '3830'='3840',
                  '3920'='3930',
                  '3950'='3955',
                  '4550'='9050, 9415',
                  '4960'='4965',
                  '5130'='5165',#, 4400', 
                  '5200'='5420',#{fuzzy_match_occps("Gaming/Gamblind Cage Workers",.05); fuzzy_match_occps("Brokerage Clerks",.05)}
                  
                  '5210'='5350',
                  '5830'='5940, 5165',
                  '5930'='5940', #???	'Office and Administrative Support Workers, All Other'
                  '6000'='6005',
                  '6020'='6050',
                  '6350'='6355',
                  '6500'='6220',#'https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2006-2010-acs-pums-occupation-conversion-rates.xlsx'
                  '6510'='6515',
                  '6750'='6765', '6760'='6765',
                  '6920'='6800',
                  '7050'='7100',
                  '7110'='7100',#'https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2006-2010-acs-pums-occupation-conversion-rates.xlsx'
                  '7310'='7315',
                  '7520'='7630',
                  '7550'='7640', # ???? 'Manufactured Building and Mobile Home Installers' #'https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2006-2010-acs-pums-occupation-conversion-rates.xlsx'
                  '7620'='7630',
                  '7710'='7750',#, 7140', #??? 'Aircraft Structure, Surfaces, Rigging, and Systems Assemblers'
                  '8060'='8100', #??? 'Model Makers and Patternmakers, Metal and Plastic'	
                  '8230'='8256',
                  '8240'='8256, 8255',
                  '8260'='8255',
                  '8840'='8990','8900'='8990','8960'='8990',
                  '8965'='7905, 8990',
                  '9330'='9300',
                  '9340'='9420',
                  'bbbb'='0000') %>% 
    dplyr::recode(., # 2012, codes
                  '0330'='0335', '0950'='0960', '1060'='1065', '1930'='1935', '2430'='2435', '2540'='2545', 
                  '3535'='3545', '4300'='4330', '4460'='4461', '4610'='3602', '5030'='5040', '5620'='9645', '5800'='1108',
                  '6830'='6835', '6910'='6850', '7900'='7905', '8840'='8990', 
                  '9000'='9005', '9340'='9430', '9360'='9365', '9500'='9570', '9560'='9570', '9730'='6850') %>% 
    gsub('^(3850|3860)$', '3870', .) %>% gsub('^(4050|4060)$', '4055', .) %>% gsub('^(4410|4430)$', '4435', .) %>% 
    gsub('^(6100|6110)$', '6115', .) %>% gsub('^(6300|6310|6320)$', '6305', .) %>% gsub('^(6420|6430)$', '6410', .) %>% 
    gsub('^(6930|6940)$', '6950', .) %>% gsub('^(7600|7630)$', '7640', .) %>% gsub('^(7920|7930|7940)$', '7925', .) %>% 
    gsub('^(7960|8010|8020)$', '8025', .) %>% gsub('^(8120|8150|8160|8200|8210|8220)$', '8225', .) %>% gsub('^(8330|8340)$', '8335', .) %>% 
    gsub('^(8360|8400|8410|8420)$', '8365', .) %>% gsub('^(8430|8440|8460)$', '8465', .) %>% gsub('^(8520|8550)$', '8555', .) %>% 
    gsub('^(8860|8900)$', '8965', .) %>% gsub('^(9230|9260)$', '9265', .) %>% gsub('^(9740|9750)$', '9760', .) %>%
    dplyr::recode(., 
                  '0050'='0051, 0052', '0100'='0101, 0102', '0430'='0335, 0440, 0705', '0740'='0705, 0750', '0840'='0845, 0960', '1107'='0705, 1108, 1065, 1022, 1032', 
                  '1020'='1021, 1022', '1030'='1031, 1032', '1300'='1305, 1306', '1540'='1541, 1545', '1550'='1551, 1555', '1740'='1745, 1750', '1820'='1821, 1822, 1825', 
                  '1965'='1935, 1970', '2000'='2001, 2002, 2003, 2004, 2005, 2006', '2010'='2011, 2012, 2013, 2014', '2160'='2170, 2180, 2862', '2200'='2205, 2545',
                  '2340'='2350, 2360', '2550'='2435, 2555', '2630'='2631, 2632, 2633, 2634, 2635, 2636, 2640', '2720'='2721, 2722, 2723', '2750'='2751, 2752', 
                  '2760'='2755, 2770', '2800'='2805, 2865', '2860'='2861, 2865', '2900'='2905, 5040', '2960'='2905, 2970', '3060'='3090, 3100',# 3065, 3070', 
                  '3260'='3270, 3261', 
                  '3320'='3321, 3322, 3323, 3324, 3330', '3400'='3401, 3402', '3420'='3421, 3422, 3423, 3424, 3430, 3545', '3510'='3515, 3550', '3540'='1980, 3550',
                  '3600'='3601, 3603, 3605', '3730'='3725', '3800'='3801, 3802', '3955'='3946, 3960', '4250'='4251, 4252, 4255', '4320'='4330, 9005',
                  '4520'='4521, 4522, 4525', '4620'='4621, 4622', '4650'='4461, 4655', '5520'='5521, 5522', '5700'='5710, 5720, 5730, 5740', '6440'='6441, 6442',
                  '6820'='6825, 6835', '6840'='6850, 6950', '8965'='7905, 8990', '9120'='9121, 9122, 9141', '9140'='9141, 9142', '9200'='9210, 9265', '9420'='9365, 9430',
                  '9520'='9570, 9760, 6850, 6825', '9820'='1555, 9825') %>%
    gsub('(3730|3735)', '3725', .) %>%
    gsub('(3065|3070)', '3090, 3100', .) %>%
    gsub('(6821)', '9570, 9760, 6850, 6825', .) %>%
    gsub('(0325|0400|0426)', '0440', .) %>%
    gsub('(8840|8900|8960)', '8990', .) %>%
    # gsub('()', '', .) %>%
    # gsub('()', '', .) %>%
    
    {if(remove_comma_sep) gsub(",", " ", .) else .} %>% 
    trimws_()
}


#' Samantha Rhoads's function to convert NAICS codes to their sector names or codes (jlcentric)
#' @export
#' @examples
#' naics_to_sector(v, return_description=F)
naics_to_sector <- function(v, return_description=F){
  v_ <- substr(v, 1,2)
  v_sector <- ifelse(v_ %in% c('31','32','33','3M'), '31-33', ifelse(v_ %in% c('44','45','4M'), '44-45', ifelse(v_ %in% c('48','49'), '48-49', ifelse(v_ %in% c('99','0'), '-', v_))))
  if(return_description){
    v_sector <- recode(v_sector, 
                       '11'='Agriculture, Forestry, Fishing and Hunting',
                       '21'='Mining, Quarrying, and Oil and Gas Extraction',
                       '22'='Utilities',
                       '23'='Construction',
                       '31-33'='Manufacturing',
                       '42'='Wholesale Trade',
                       '44-45'='Retail Trade',
                       '48-49'='Transportation and Warehousing',
                       '51'='Information',
                       '52'='Finance and Insurance',
                       '53'='Real Estate and Rental and Leasing',
                       '54'='Professional, Scientific, and Technical Services',
                       '55'='Management of Companies and Enterprises',
                       '56'='Administrative and Support and Waste Management and Remediation Services',
                       '61'='Educational Services',
                       '62'='Health Care and Social Assistance',
                       '71'='Arts, Entertainment, and Recreation',
                       '72'='Accommodation and Food Services',
                       '81'='Other Services (except Public Administration)',
                       '92'='Public Administration')
  }
  return(v_sector)
}


#' Samantha Rhoads's function to make state-puma out of state and puma
#' @export
#' @examples
#' get_state_puma(st=c("CA"), puma=c("00100"), return_na_if_no_match=T, puma_nchar=5)
get_state_puma <- function(st=c("CA"), puma=c("00100"), return_na_if_no_match=T, puma_nchar=5){
  if(any(nchar(st)>2|grepl("\\d", st))){state_abb <- recode_state(st, abb=T)} else {state_abb <- toupper(st)}
  if(any(nchar(puma)!=puma_nchar)){
    if(any((nchar(puma)>puma_nchar)|grepl("[[:alpha:]|[:punct:]]", puma))){puma <- extract_digits(puma)} else if(any(nchar(puma)<puma_nchar)){puma <- pad_leading_0s(puma, length=puma_nchar)}
  }
  state_pumas <- paste0(replace_na(state_abb, ''), '-', replace_na(puma, '')) %>% gsub('^-|-$', '', .)
  if(return_na_if_no_match){
    return(ifelse(nchar(state_pumas)!=8, NA, state_pumas))
  }
}

#' Samantha Rhoads's function to bind rows of object if they're dataframes
#' @export
#' @examples
#' bind_rows_(..., .id = NULL)
bind_rows_ <- function(..., .id = NULL){
  if(is.data.frame(...)|all(sapply(..., is.data.frame))){
    return(dplyr::bind_rows(..., .id = NULL))
  } else {
    return(...)
  }
}

#' Samantha Rhoads's function to bind rows going multiple depths
#' @export
#' @examples
#' bind_rows2(l)
bind_rows2 <- function(l){
  lapply(lapply2(l, bind_rows_), bind_rows_)
}


#' Samantha Rhoads's function to
#' @export
#' @examples
#' clean_unique_na_sep()
clean_unique_na_sep <- function(v, sep=",", sort_strings=F){
  splitv <- strsplit(v, sep)
  uniqv <- lapply(splitv, function(s){
    s_ <- na.omit(unique(na_if(trimws_(s), 'NA')))
    if(sort_strings){
      s_ <- sort(s)
    }
    paste0(s_, collapse=sep)
  })
  as.character(unlist(uniqv))
}


#' A function to recode specified values into NAs
#' @export
#' @examples
#' recode_na(x, ...)
recode_na <- function(x, ...) {
  x[x %in% c(...)] <- NA
  x
}


###################################################################################################################################################


# 05 27, 2021 (20210527) ##########################################################################################################################

#' Samantha Rhoads's function to recode a vector from 2 reference vectors
#' @export
#' @examples
#' recode_match(v_input=c('girl', 'dude'), match_vec=c('dude', 'man', 'girl', 'woman'), desired_vec=c('male', 'male', 'female', 'female'))
recode_match <- function(v_input=c('girl', 'dude'), match_vec=c('dude', 'man', 'girl', 'woman'), desired_vec=c('male', 'male', 'female', 'female')){
  result <- desired_vec[match(v_input, match_vec)]
  ifelse(is.na(result), v_input, result)
}


#' Samantha Rhoads's function to 
#' @export
#' @examples
#' monthYear_to_dateRangeStr(v = c('2020-01, 2020-02, 2020-03', '2020-05, 2021-01', '2021-10, 2021-11, 2021-12, 2022-02'), to_symbol=" to ")
monthYear_to_dateRangeStr <- function(v = c('2020-01, 2020-02, 2020-03', '2020-05, 2021-01', '2021-10, 2021-11, 2021-12, 2022-02'), to_symbol=" to "){
  tryCatch({
    v %>% gsub('-', '', .) %>% strsplit(., ', ') %>% sapply(., function(x){ # {x <- v %>% gsub('-', '', .) %>% strsplit(., ', ') %>% .[[2]]}
      x_ <- readr::parse_number(as.character(x))
      x_0 <- split(x_, cumsum(c(1, diff(x_) != 1)))
      x_1 <- x_0 %>% sapply(., function(y){if(length(unique(y))>1){paste0(min(y,na.rm=T), to_symbol, max(y,na.rm=T))} else y}) %>% paste0(., collapse=', ')
      gsub('(\\d{4})', '\\1-', x_1)
    })
  }, 
  error=function(e){v})
}

# source("https://raw.githubusercontent.com/tidyverse/dplyr/master/R/colwise-funs.R")
# source("https://raw.githubusercontent.com/tidyverse/dplyr/master/R/colwise-mutate.R")
# list2 <- list
# names2 <- names
# is_quosure <- rlang#:#:is_quosure
# call2 <- rlang#:#:call2
# 
# summarize_all_paste_unique_sort <- function(.tbl, .funs, ...){
#   lifecycle#:#:signal_superseded("1.0.0", "summarise_all()", "across()")
#   funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ..., .caller = "summarise_all")
#   summarise(.tbl, !!!funs)
# }

#' Samantha Rhoads's function to take a vector of strings, split each by a separator (arg sep), keep only unique items in each split string, sort those unique items in each split string, then re-paste/collapse the items of each split string back into strings separated by what the user originally defined as sep
#' @export
#' @examples
#' unique_sep_sort(v, sep = "; ", sort_str=T)
unique_sep_sort <- function (v, sep = "; ", sort_str=T) {
  # splitv <- strsplit(v, sep)
  # uniqv <- lapply(splitv, function(x) sort(unique(x)))
  # lapply(uniqv, function(s) paste0(s, collapse = sep)) %>% dplyr::combine() %>% as.character()
  do_nothing_fxn <- function(x){x}
  sort_custom_fxn <- if(sort_str==T){sort} else {do_nothing_fxn}
  splitv <- sapply(v, function(s) {strsplit(s, sep) %>% unlist() %>% unique() %>% sort_custom_fxn() %>% paste0(., collapse=sep)}) %>% as.character()
  splitv
}

#' Samantha Rhoads's function to 
#' @export
#' @examples
#' paste_unique_sep_sort(v, sep = "; ", collapse=sep)
paste_unique_sep_sort <- function (v, sep = "; ", collapse=sep, sort_str=T) {
  v_ <- paste0(v, collapse=collapse)
  unique_sep_sort(v_, sep=sep, sort_str=sort_str)
}

#' Samantha Rhoads's function to summarize_all remaining columns in a grouped df by pasting their unique values
#' @export
#' @examples
#' summarize_all_paste0(.tbl, .funs, ..., collapse=", ", unique_sep_sort_str=T, recode_na_vals=c("", "NA"), ungroup=F)
summarize_all_paste0 <- function(.tbl, .funs, ..., collapse="; ", unique_sep_sort_str=T, unique_sep_str=T, sort_str=T, recode_na_vals=c("", "NA"), ungroup=F){
  # lifecycle#:#:signal_superseded("1.0.0", "summarise_all()", "across()")
  # funs <- manip_all(.tbl, .funs, enquo(.funs), caller_env(), ..., .caller = "summarise_all")
  # summarise(.tbl, !!!funs)
  do_nothing_fxn <- function(x){x}
  sort_custom_fxn <- if(sort_str==T){sort} else {do_nothing_fxn}
  
  d <- dplyr::summarize_all(.tbl, function(v){ 
    v_ <- paste0(sort_custom_fxn(unique(v)), collapse=collapse)
    
    if(unique_sep_sort_str){
      v_ <- unique_sep_sort(v_, sep=collapse, sort_str=sort_str)
    }
    if(length(recode_na_vals)>0){
      v_ <- recode_na(v_, recode_na_vals)
    }
  })
  if(ungroup){
    d <- ungroup(d)
  }
  return(d)
}


#' Samantha Rhoads's function to 
#' @export
#' @examples
#' describe_rgb_to_hex_translation(RGB="rgb(35, 59, 150)")
describe_rgb_to_hex_translation <- function(RGB="rgb(35, 59, 150)"){
  letter_digit_recode_list <- list('A'='10','B'='11','C'='12','D'='13','E'='14','F'='15')
  rba_separated <- as.numeric(trimws(gsub('[^[:space:]:[:digit:]]', '', unlist(strsplit(RGB, ',')))))
  cat('rba_separated=', rba_separated)
  
  hex_separated <- sapply(rba_separated, function(rgb_specific_segment){ #{rgb_specific_segment=199}
    catn('\n_____________________________')
    calc1 <- rgb_specific_segment/16
    calc1b <- unlist(strsplit(as.character(calc1), '.', fixed=T))
    cat('\nrgb_specific_segment/16 ==', rgb_specific_segment/16, '== (', calc1b[1], 'and', paste0('.',calc1b[2]) ,')')
    hexchar1a <- recode_from_list(calc1b[1], recode_list=letter_digit_recode_list)
    hexchar1b <- names(letter_digit_recode_list)[match(hexchar1a, as.character(unlist(letter_digit_recode_list)))]
    if(is.na(hexchar1b)){hexchar1b<-hexchar1a}
    # cat(paste0('hexchar1a ==', calc1b[1], '\nhexchar1b ==', hexchar1b))
    cat(paste0('\nhexchar1 == ', calc1b[1], ' == ', hexchar1b))
    
    hexchar2a <- as.numeric(paste0('.',calc1b[2]))*16
    hexchar2b <- names(letter_digit_recode_list)[match(hexchar2a, as.character(unlist(letter_digit_recode_list)))]
    if(is.na(hexchar2b)){hexchar2b<-hexchar2a}
    cat(paste0('\nhexchar2 == ', paste0('.',calc1b[2]), '*16 == ', hexchar2a, ' == ', hexchar2b))
    # cat(paste0('hexchar2a == ', paste0('.',calc1b[2]), '*16 ==', hexchar2a, '\nhexchar2b ==', hexchar2b))
    
    hex_specific_segment <- paste0(hexchar1b, hexchar2b)
    cat('\nhex_specific_segment ==', hex_specific_segment, '\n')
    print(hex_specific_segment)
    hex_specific_segment
  })
  hex_code <- paste0(hex_separated, collapse="")
  cat('\n============================\nhex_code ==', hex_code, '\n')
  return(hex_code)
}


#' Samantha Rhoads's function to check if your in your local environment
#' @export
#' @examples
#' is_local()
is_local <- function() {Sys.getenv('SHINY_PORT') == ""}


#' Samantha Rhoads's function to fill a dataframe variable (at_var) by a name of an ID variable (by_var)
#' @export
#' @examples
#' fillr_by_at(d, by_var='occ_code', at_var='occ_description')
fillr_by_at <- function(d, by_var='occ_code', at_var='occ_description'){
  d0 <- d
  d0[['AT_VAR']] <- d0[[at_var]]
  d1 <- d0 %>% group_by_at(vars(one_of(by_var))) %>% tidyr::fill(., AT_VAR) %>% tidyr::fill(., AT_VAR, .direction = "up")
  d1[[at_var]] <- d1[['AT_VAR']]
  ungroup(mutate(d1, AT_VAR=NULL))
}

###################################################################################################################################################


# MM DD, YYYY (YYYYMMDD) ##########################################################################################################################

#' Samantha Rhoads's function to add escape characters to a string
#' @export
#' @examples
#' add_escape_characters(v)
add_escape_characters <- function(v){
  gsub("(\\W)", "\\\\\\1", v)
}

#' Samantha Rhoads's function to re-try running something multiple times (a designated number of `n_attempts`) if it fails until it succeeds.
#' @export
#' @examples
#' retry_fxn(FUN=function(){"hi"}, n_attempts=5)
retry_fxn <- function(FUN=function(){"hi"}, n_attempts=5){
  r <- NULL
  attempt <- 1
  while( is.null(r) && attempt <= n_attempts ) {
    attempt <- attempt + 1
    try({
      r <- FUN(); 
      return(r)
    })
  } 
}

#' Samantha Rhoads's function to re-try running something multiple times (a designated number of `n_attempts`) if it fails until it succeeds.
#' @export
#' @examples
#' retry_fxn(x, path=tempfile(fileext=".html"))
dt_open <- function(x, path=tempfile(fileext=".html")){
  if(!grepl("xls(x|)$", path, ignore.case=T)){path <- paste0(path, ".html")}
  if(is.data.frame(x)){widget <- DT::datatable(x)} else if("datatables" %in% class(x)){widget <- x}
  htmlwidgets::saveWidget(widget, path, selfcontained=TRUE, libdir=NULL, background="white", title=class(widget)[[1]], knitrOptions=list())
  system(paste0('open "', path, '"'))
  return(x)
}


#' Samantha Rhoads's function to force recode something that looks like it might be a date into a more consistent date structure.
#' @export
#' @examples
#' retry_fxn(x, path=tempfile(fileext=".html"))
recode_date <- function(v){
  v_ <- v %>% gsub("[[:alpha:]]|\\(|\\)|\\\n", " ", .) %>% trimws_() 
  v_new <- sapply(v_, function(vs) {
    v_0 <- vs %>% extract_date() %>% sapply(function(s) {
      s %>% trimws_() %>% sort_str_by_nchar() %>% .[1]
    }) %>% as.character() %>% as.date.varioustypes() %>% as.character()
    v_1 <- vs %>% as.character() %>% srhoads::excelToDateIf5DigitStrAndManyDigitTime() %>% srhoads::as.date.varioustypes() %>% as.character() %>% recode_na("NA", "NULL", "")
    v_1 <- tryCatch(v_1 %>% lubridate::date() %>% as.character(), error=function(e) tryCatch(extract_date(vs) %>% unlist() %>% .[1] %>% trimws_(), error=function(e) NA))
    
    v_2 <- c(v_1, v_0) %>% na.omit() %>% unique() %>% sort_str_by_nchar() %>% .[1] #paste0(., collapse=" ~ ")
    v_3 <- ifelse(is.null(v_2), NA, v_2) %>% recode_na("NA", "NULL", "") %>% as.date.varioustypes()
    v_3
    v_4 <- tryCatch({tryCatch({if(lubridate::date(v_3)<"1999-12-31"){NA} else {v_3}}, error=function(e){v_3})}, error=function(e) NA)
    v_4
  }) 
  # })
}

#' Samantha Rhoads's function to strip accents from characters
#' @export
#' @examples
#' strip_accents(v, id="Latin-ASCII")
strip_accents <- function(v, id="Latin-ASCII"){
  iconv(stringi::stri_trans_general(str=v, id=id))
}

#' Samantha Rhoads's function to take a numeric range and split/bin it by the number of groups you want
#' @export
#' @examples
#' bin_range(v, n_groups=10)
bin_range <- function(v, n_groups=10){
  split(v,{cut(v,seq(min(v, na.rm=T)-1,max(v, na.rm=T),length.out=n_groups))})
}

#' Samantha Rhoads's function to delete a directory and all of its contents
#' @export
#' @examples
#' remove_dir(path)
remove_dir <- function(path){
  subfiles_to_remove <- list.files(path, recursive=T, full.names=T)
  for(filename in subfiles_to_remove){
    file.remove(filename)
  }
  file.remove(path)
}

#' Samantha Rhoads's function to select columns from a dataframe based on colname match and data contents match
#' @export
#' @examples
#' select_at_if(d, colname_pattern=vars(everything()), data_pattern=function(x){T}, condition=c("or", "and")[1] )
select_at_if <- function(d, colname_pattern=vars(everything()), data_pattern=function(x){T}, condition=c("or", "and")[1] ){
  data_selected_at <- d %>% select_at(colname_pattern)
  data_selected_if <- d %>% select_if(data_pattern)
  if(tolower(condition) %in% c("and", "&")){
    data_selected_if <- data_selected_at %>% select_at(colname_pattern)
  }
  colnames_union <- union(names(data_selected_at), names(data_selected_if))
  data_selected <- select(d, any_of(colnames_union))
  return(data_selected)
}

#' Samantha Rhoads's function to find the max date appropriately, without getting Inf when there are NAs
#' @export
#' @examples
#' max_date(dates, include_time=T, na.rm=T)
max_date <- function(dates, include_time=T, na.rm=T){
  if(include_time){
    if_else(!all(is.na(lubridate::as_datetime(dates))), max(lubridate::as_datetime(dates), na.rm=na.rm), lubridate::as_datetime(NA))
  } else {
    if_else(!all(is.na(lubridate::as_date(dates))), max(lubridate::as_date(dates), na.rm=na.rm), lubridate::as_date(NA))
  }
}
max_datetime <- function(dates, na.rm=T){
  if_else(!all(is.na(lubridate::as_datetime(dates))), max(lubridate::as_datetime(dates), na.rm=na.rm), lubridate::as_datetime(NA))
}

#' Samantha Rhoads's function to find the min date appropriately, without getting Inf when there are NAs
#' @export
#' @examples
#' min_date(dates, include_time=T, na.rm=T)
min_date <- function(dates, include_time=T, na.rm=T){
  if(include_time){
    if_else(!all(is.na(lubridate::as_datetime(dates))), min(lubridate::as_datetime(dates), na.rm=na.rm), lubridate::as_datetime(NA))
  } else {
    if_else(!all(is.na(lubridate::as_date(dates))), min(lubridate::as_date(dates), na.rm=na.rm), lubridate::as_date(NA))
  }
}

min_datetime <- function(dates, na.rm=T){
  if_else(!all(is.na(lubridate::as_datetime(dates))), min(lubridate::as_datetime(dates), na.rm=na.rm), lubridate::as_datetime(NA))
}

#' Samantha Rhoads's function to set names to a specific row and concatenate the contents above into the names
#' @export
#' @examples
#' set_names_skip_rows_until_match_concat(d, example_colname="Employee ID")
set_names_skip_rows_until_match_concat <- function(d, example_colname="Employee ID"){
  # example_colname <- "Employee ID"
  doEvenIfColnameIsAlreadyIt=F; check_n_rows=100; concat_all_prior_rows=T
  if (!(example_colname %in% names(d))|doEvenIfColnameIsAlreadyIt) {
    colnames_rownum <- grep_all_df(example_colname, d[1:check_n_rows, ], rownums_only=T)[1]
    if(concat_all_prior_rows){
      if(length(colnames_rownum)>0&!is.na(colnames_rownum)){
        # if(colnames_rownum>0){
        prior_rows <- 1:(colnames_rownum)
        dNewNames <- d %>% 
          slice(prior_rows) %>% 
          group_by() %>% 
          summarize_all_paste0(., collapse="__") %>% 
          ungroup() %>% 
          unlist() %>%  
          as.character() %>% 
          replace_na(., "NA.0") %>% 
          make.unique()
        # }
      }
    } else {
      dNewNames <- as.character(d[colnames_rownum, ]) %>% 
        gsub('^$|\\`', 'UNNAMED', .) %>% 
        replace_na(., "NA.0") %>% 
        make.unique()
    }
    if ((length(colnames_rownum)>0)&!is.na(colnames_rownum)) {
      d2 <- d %>% 
        setNames(dNewNames) %>% 
        slice(-(1:colnames_rownum))
    } else {
      d2 <- d
    }
    d2
  } else {
    d2 <- d
  }
  d2 #%>% janitor::clean_names() 
}

#' Samantha Rhoads's function to fill in missing values with the value directly above the given cell with an NA in a specific column of a dataframe
#' @export
#' @examples
#' recode_lag_while_NAs(x, colname="eeid")
recode_lag_while_NAs <- function(x, colname="eeid"){
  if(is.data.frame(x)){
    while(any(is.na(x[[colname]]))){
      x[[colname]] <- if_else(!is.na(x[[colname]]), x[[colname]], lag(x[[colname]]))
    }
  } else {
    while(any(is.na(x))){
      x <- if_else(!is.na(x), x, lag(x))
    }
  }
  return(x)
}


###################################################################################################################################################


# MM DD, YYYY (YYYYMMDD) ##########################################################################################################################
###################################################################################################################################################


# MM DD, YYYY (YYYYMMDD) ##########################################################################################################################
###################################################################################################################################################


# MM DD, YYYY (YYYYMMDD) ##########################################################################################################################
###################################################################################################################################################


# MM DD, YYYY (YYYYMMDD) ##########################################################################################################################
###################################################################################################################################################





########################################################################################################################

#  #######################################################################################################################

########################################################################################################################


cat("\nyey u loaded sam's fxns!\n")


