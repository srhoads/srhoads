library(srhoads); pkg('tidyverse', 'magrittr')

#===========================DATA==================================

if(!exists("puma_crosswalk")){
  puma_crosswalk <- rio::import(file="https://usa.ipums.org/usa/resources/volii/PUMA2000_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('pop|land|gisjoin|cpuma00|geoid')) %>% 
    mutate(stab00 = recode_state(state00), stab10 = recode_state(state10),
           state_puma00 = get_state_puma(stab00, puma00), state_puma10 = get_state_puma(stab10, puma10)) #%>% 
}

if(!exists("puma_msa_ref")){
  puma_msa_ref <- readr::read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv") %>% select(-one_of("X8", "X9", "notes", "msa"))
}

if(!exists("df_1_row_per_msa")){
  df_1_row_per_msa <- puma_msa_ref %>%
    group_by_at(vars(one_of("state_msa"))) %>% 
    summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% unique_sep_sort2(., ", ") %>% recode_na('', 'NA')) %>% ungroup()
}
if(!exists("df_1_row_per_puma")){
  df_1_row_per_puma <- puma_msa_ref %>%
    group_by_at(vars(one_of("state_puma"))) %>% 
    summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% unique_sep_sort2(., ", ") %>% recode_na('', 'NA')) %>% ungroup()
}

if(!exists('zip_code_db')|!exists('zip_puma_ref')){
  # zip_puma_ref <- bind_rows(geocorr::zcta2010_to_puma2012, geocorr::zcta2010_to_puma2000 %>% setNames(gsub('2kName', 'name', names(.)) %>% gsub('2k', '12', .) )) %>% distinct() %>% distinct(puma12, zcta5, .keep_all=T) %>% select(-matches('intpt|pop10|afact')) %>% mutate(zip=zcta5, zipcode=zcta5) %>% mutate_all(function(v) tolower(iconv(enc2utf8(v))))
  zip_puma_ref <- geocorr::zcta2010_to_puma2012 %>% select(-matches('intpt|pop10|afact')) %>% mutate(zip=zcta5, zipcode=zcta5) %>% mutate_all(function(v) tolower(iconv(enc2utf8(v))))
  # writexl_open(zip_code_db, "zip_code_db.xlsx")
  zip_code_db <- (zip_code_db_github <- read_csv('https://raw.githubusercontent.com/DataUSA/datausa-tutorials/master/commuting_viz_tutorial/csv/zip_code_database.csv') %>% 
                    mutate(major_city=primary_city, zipcode=pad_leading_0s(zip), lat=latitude, lng=longitude, common_city_list=acceptable_cities %>% blob::vec_cast.blob() ) %>%
                    select(-one_of(setdiff(names(.), names(zipcodeR::zip_code_db)))) %>% # filter(!zipcode %in% zipcodeR::zip_code_db$zipcode) %>%
                    mutate(post_office_city = major_city )
  ) %>%
    bind_rows(zipcodeR::zip_code_db, .) %>%
    as_tibble()
  
  # writexl_open(zip_code_db, "zip_code_db.xlsx")
  
  # 20227, 20520, 20565 
  zip_puma_ref %>% filter(grepl('20227|20520|20565', zipcode))
  setdiff(zip_code_db$zipcode, zip_puma_ref$zcta5)
  puma_to_censustract_county <- "https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt" %>% read_csv(col_names=T) %>% janitor::clean_names() %>%
    mutate(state_county = paste0(statefp, countyfp)) %>%
    left_join(., geocorr::county2014_to_puma2012 %>% mutate(state_county=county14)) %>%
    select(-matches('afact|pop')) %>%
    mutate(county = gsub(' [[:alpha:]]{2}$', '', cntyname2))
  
  # writexl_open(puma_to_censustract_county, "puma_to_censustract_county.xlsx")
  
  if(F){
    puma_to_censustract_county %>%
      rowwise() %>% mutate(zip = zipcodeR::search_county(county, stab) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>% paste0(., collapse=", ")) %>% ungroup()
  }
  zipcodeR::zcta_crosswalk # ZCTA5 TRACT GEOID
  zipcodeR::zip_code_db %>% filter(grepl('20227|20520|20565', zipcode))
  # zipcodeR::search_county("District of Columbia", "DC")
}


#=========================FUNCTIONS====================================

unique_sep_sort2 <- srhoads::unique_sep_sort #function(v, sep = "; "){sapply(v, function(s) strsplit(s, sep) %>% unlist() %>% unique() %>% sort() %>% paste0(., collapse=sep)) %>% as.character()}

get_state_puma <- function(st, puma){
  state_abb <- srhoads::recode_state(st, abb=T)
  paste0(replace_na(state_abb, ''), '-', replace_na(puma, '')) %>% gsub('^-|-$', '', .)
}



# puma_msa_ref %>% filter(is.na(msa_description)) %>% select(-matches('msa_desc')) %>%
#   left_join()

# puma_msa_ref %>% writexl_open('puma_msa_ref.xlsx')
"Users/rhoadss/Downloads/PUMA2000_PUMA2010_crosswalk.xlsx" # has manually added msa x puma codes

recode_msa_to_puma <- function(s, return_na_if_no_match=T){
  # if(!exists('puma_msa_ref')){puma_msa_ref <- read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv")}
  v_ <- s %>% strsplit(., ', ') %>% unlist()
  to <- c(df_1_row_per_msa$state_puma, df_1_row_per_msa$state_puma)
  from <- c(df_1_row_per_msa$state_msa, df_1_row_per_msa$msa)
  result <- (to[match(v_, from)]) %>% paste0(., collapse=", ")
  if(return_na_if_no_match){
    return(result)
  } else {
    return(ifelse(is.na(result), v_, result))
  }
}

recode_puma_to_msa <- function(s, return_na_if_no_match=T){
  # df_1_row_per_puma <- puma_msa_ref %>%
  #   group_by(state_puma) %>% 
  #   summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup()
  
  # print(s)
  v_ <- s %>% unique() %>% paste0(., collapse=", ") %>% recode_na(., "NA", "") %>% strsplit(., ', ') %>% unlist()
  # v_ <- s %>% strsplit(., ', ') %>% unlist()
  to <- df_1_row_per_puma$state_msa
  from <- df_1_row_per_puma$state_puma
  result <- (to[match(v_, from)]) %>% paste0(., collapse=", ")
  if(return_na_if_no_match){
    return(result)
  } else {
    return(ifelse(is.na(result), v_, result))
  }
}


# 
# recode_puma_to_msa <- function(s){
#   # if(!exists('puma_msa_ref')){
#   #   puma_msa_ref <- rio::import(file="https://usa.ipums.org/usa/resources/volii/MSA2013_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('population')) %>% mutate_all(tolower) %>% 
#   #     mutate(state_abb = statetoabb(state_name), state_puma = get_state_puma(state_abb, puma_code))
#   # }
#   
#   df_1_row_per_puma <- puma_msa_ref %>%
#     group_by(state_puma) %>% 
#     summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup()
#   
#   v_ <- s %>% strsplit(., ', ') %>% unlist()
#   to <- df_1_row_per_puma$state_msa
#   from <- df_1_row_per_puma$state_puma
#   result <- to[match(v_, from)]
#   ifelse(is.na(result), v_, result) %>% paste0(., collapse=", ")
# }

# recode_zipcode_to_puma <- function(s="20175, 20176, 20177, 20178"){
#   v_ <- s %>% strsplit(., ', ') %>% unlist()
#   df_1_row_per_zipcode <- geocorr::zcta2010_to_puma2012 %>% mutate(state_puma = get_state_puma(stab, puma12)) %>% select(matches("zcta|puma")) %>% group_by(zcta5) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup()
#   to <- df_1_row_per_zipcode %>% .$state_puma
#   from <- df_1_row_per_zipcode$zcta5
#   result <- to[match(v_, from)]
#   ifelse(is.na(result), v_, result) %>% paste0(., collapse=", ")
# }

df_zipcode_puma_ref <- full_join(
  geocorr::zcta2010_to_puma2012 %>% mutate(state_puma = get_state_puma(stab, puma12)) %>% select(matches("zcta|puma"), -matches('name')),
  geocorr::zcta2010_to_puma2000 %>% mutate(state_puma = get_state_puma(stab, puma2k)) %>% select(matches("zcta|puma"), -matches('name')),
  by="zcta5",
  suffix=c("_12", "_2k")
) %>%
  mutate(state_puma = ifelse(state_puma_12==state_puma_2k, state_puma_12, paste0(state_puma_12, ", ", state_puma_2k)))

df_1_row_per_zipcode <- df_zipcode_puma_ref %>% group_by(zcta5) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% unique_sep_sort2(., ", ") %>% recode_na('')) %>% ungroup()

# puma_ref_with_2k_pumas <- left_join(df_1_row_per_puma, df_zipcode_puma_ref %>% mutate(state_puma=state_puma_12, zcta5=NULL, puma2k=NULL, puma12=NULL) %>% distinct()) %>% distinct()

recode_zipcode_to_puma <- function(s="20175, 20176, 20177, 20178", include_2000_pumas=T, return_na_if_no_match=T){
  v_ <- s %>% strsplit(., ', ') %>% unlist()
  if(include_2000_pumas){
    df_1_row_per_zipcode
  } else {
    df_1_row_per_zipcode <- geocorr::zcta2010_to_puma2012 %>% mutate(state_puma = get_state_puma(stab, puma12)) %>% select(matches("zcta|puma"), -matches('name')) %>% group_by(zcta5) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup()
  }
  to <- df_1_row_per_zipcode %>% .$state_puma
  from <- df_1_row_per_zipcode$zcta5
  result <- (to[match(v_, from)]) %>% unique() %>% na.omit()
  if(return_na_if_no_match){
    return(result %>% paste0(., collapse=", "))
  } else {
    return(ifelse(is.na(result), v_, result) %>% paste0(., collapse=", "))
  }
}

# recode_zipcode_to_puma <- function(s="20175, 20176, 20177, 20178"){
#   v_ <- s %>% strsplit(., ', ') %>% unlist()
#   to <- geocorr::zcta2010_to_puma2012 %>% mutate(state_puma = get_state_puma(stab, puma12)) %>% .$state_puma
#   from <- geocorr::zcta2010_to_puma2012$zcta5
#   result <- to[match(v_, from)]
#   ifelse(is.na(result), v_, result) %>% paste0(., collapse=", ")
# }


recode_zipcode_to_city <- function(zipcode=c("36101")){ #zipcodes=c("36101", "60007")
  ## tryCatch({lapply(zipcodes, function(s) zipcodeR::reverse_zipcode(s) %>% drop_na(major_city) %>% .$major_city %>% unique() %>% paste0(., collapse="; ")) %>% as.character()},  error=function(e){NA})
  # sapply(zipcodes, function(s){tryCatch({ zipcodeR::reverse_zipcode(s) %>% drop_na(major_city) %>% .$major_city %>% unique() %>% paste0(., collapse="; ")}, error=function(e){NA})}) %>% as.character()
  tryCatch({zipcodeR::reverse_zipcode(zipcode) %>% drop_na(major_city) %>% .$major_city %>% unique() %>% paste0(., collapse="; ")}, error=function(e) NA)
}


zipcode_to_puma <- function(zipcode){
  zip_puma_ref <- geocorr::zcta2010_to_puma2012 %>% select(-matches('intpt|pop10|afact')) %>% mutate(state_puma = get_state_puma(stab, puma12))
  result <- zip_puma_ref$state_puma[match(zipcode, zip_puma_ref$zcta5)]
  paste0(result, collapse=", ")
}

# state2abb_or_abb2state <- function(v, abb=F){
#     v_ <- tolower(v)
#     if(any(!tolower(v) %in% tolower(c(state.abb, state.name)))){
#         v_ <- gsub('united states', 'us', v_)
#     }
#     st1 <- statetoabb(v_) %>% abbtostate()
#     st2 <- abbtostate(v_)
#     result <- if(!abb) ifelse(is.na(st1), st2, st1) else statetoabb(ifelse(is.na(st1), st2, st1))
#     return(result)
# }

revgeo <- function(lonlat='-86.3, 32.4', ..., output=c('all', 'zip', 'city', 'county', 'state')[1]){
  lonlat_ <- trimws(unlist(strsplit(as.character(lonlat), split=',')))
  if(length(lonlat_)!=2){
    lonlat_ <- trimws(unlist(strsplit(as.character(lonlat), split=' ')))
    if(!is.null(c(...))){
      lonlat_ <- trimws(unlist(strsplit(as.character(c(lonlat_, c(...))),split=',') ) )
    }
  }
  URL <- paste0("http://photon.komoot.de/reverse?lon=", lonlat_[1], "&lat=", lonlat_[2], "")
  # URL <- "photon.komoot.io/reverse?lon=10&lat=52"
  res <- tryCatch({
    jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() # lapply(bind_rows) %>% bind_cols()
  }, error=function(e){
    URL <- paste0("http://photon.komoot.io/reverse?lon=", lonlat_[1], "&lat=", lonlat_[2], "")
    jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() # lapply(bind_rows) %>% bind_cols()
  })
  
  if(output=='zip'){
    res <- res$properties_postcode
  } else if(output=='city'){
    res <- res$properties_city
  } else if(output=='county'){
    res <- res$properties_county
  } else if(output=='state'){
    res <- res$properties_state
  }
  
  res
}


# rusps::validate_address_usps(street='1156 Susan Way', city="Sunnyvale", state="CA", username='448JL0000161')
zipcode_to_puma <- function(zipcode){
  zip_puma_ref <- geocorr::zcta2010_to_puma2012 %>% select(-matches('intpt|pop10|afact')) %>% mutate(state_puma = get_state_puma(stab, puma12))
  result <- zip_puma_ref$state_puma[match(zipcode, zip_puma_ref$zcta5)]
  paste0(result, collapse=", ")
}

# revgeo <- function(lonlat='-86.3, 32.4', ..., output=c('all', 'zip', 'city', 'county', 'state')[1]){
#   lonlat_ <- trimws(unlist(strsplit(as.character(lonlat), split=',')))
#   if(length(lonlat_)!=2){
#     lonlat_ <- trimws(unlist(strsplit(as.character(lonlat), split=' ')))
#     if(!is.null(c(...))){
#       lonlat_ <- trimws(unlist(strsplit(as.character(c(lonlat_, c(...))),split=',') ) )
#     }
#   }
#   URL <- paste0("http://photon.komoot.de/reverse?lon=", lonlat_[1], "&lat=", lonlat_[2], "")
#   res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() # lapply(bind_rows) %>% bind_cols()
#   
#   if(output=='zip'){
#     res <- res$properties_postcode
#   } else if(output=='city'){
#     res <- res$properties_city
#   } else if(output=='county'){
#     res <- res$properties_county
#   } else if(output=='state'){
#     res <- res$properties_state
#   }
#   res
# }
cat("L243_FLAG ")

recode_state_to_zipcode <- function(state_abb="hi"){ # {city="pennington"; state_abb="sd"}
  # catn('city'); print(city); catn('state_abb'); print(state_abb)
  if(length(state_abb)>1|length(state_abb)>1){cat('\nALERT: Did you forget to make your dataframe rowwise() before running recode_city_state_to_zipcode()??\n')}
  if(nchar(state_abb)>2|lookslike_number(state_abb)){
    state_abb <- recode_state(state_abb)
  }
  # if(!exists('zip_code_db')){# zip_puma_ref <- geocorr::zcta2010_to_puma2012 %>% select(-matches('intpt|pop10|afact')) %>% mutate(zip = zcta5) %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))); zip_code_db <<- (zip_code_db_github <- read_csv('https://raw.githubusercontent.com/DataUSA/datausa-tutorials/master/commuting_viz_tutorial/csv/zip_code_database.csv') %>% mutate(major_city=primary_city, zipcode=pad_leading_0s(zip), lat=latitude, lng=longitude, common_city_list=acceptable_cities %>% blob::vec_cast.blob() ) %>%select(-one_of(setdiff(names(.), names(zipcodeR::zip_code_db)))) %>% mutate(post_office_city = major_city )) %>% bind_rows(zipcodeR::zip_code_db, .) %>% as_tibble()}
  result_0 <- tryCatch({
    zipcodeR::search_state(state_abb) %>% mutate_all(as.character) %>% mutate_at(vars(one_of('zipcode')), function(v) pad_leading_0s(v, 5)) %>% .$zipcode %>% unique()# %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
  }, error=function(e) {print(e); NA}) # summarize(state_puma = paste0(sort(unique(state_puma)), collapse=", "))#group_by(primary_city, state) %>% 
  
  # if(is.na(result)){ # city="milton"; state_abb="ga"
  result_1 <- tryCatch({
    geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
      filter(tolower(stab)==tolower(state_abb)) %>% .$zipcode %>% unique()# %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
  }, error=function(e) {print(e); NA})
  # }
  # if(is.na(result)){
  # pkg('usa')
  result_2 <- tryCatch({
    city_str <- city
    result <- usa::zipcodes %>% filter(tolower(state)==tolower(state_abb)) %>% as_tibble() %>% 
      mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
      select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort()# %>%  paste0(., collapse=", ") %>% recode_na('')
    result
  }, error=function(e) {print(e); NA})
  # }
  
  result <- c(result_0, result_1, result_2) %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
  
  if(is.na(result)|length(result)==0){
    result <- tryCatch({
      pkg('noncensus'); # install.packages('https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz', repos=NULL)
      city_str <- city
      data(zip_codes)
      result <- zip_codes %>% filter(tolower(state)==tolower(state_abb)) %>% as_tibble() %>% 
        mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
        select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      result
    }, error=function(e) {print(e); NA})
  }
  
  return(result)
}




state_puma_extra_df <- data.frame(stringsAsFactors = FALSE, # tbl(con, "puma_dictionary") %>% select(matches("state_puma|state")) %>% filter(state %in% c('pr')|state_puma %in% c('LA-77777')) %>% as_tibble() %>% datapasta::df_paste()
                                  state = c("la", 
                                            "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", 
                                            "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr", "pr"),
                                  state_puma = c("LA-77777", 
                                                 "PR-00100", "PR-00101", "PR-00102", "PR-00200", "PR-00201", "PR-00202", "PR-00300", "PR-00301", "PR-00302", "PR-00400", "PR-00401", "PR-00402", "PR-00403", "PR-00500", 
                                                 "PR-00501", "PR-00502", "PR-00503", "PR-00600", "PR-00601", "PR-00602", "PR-00700", "PR-00701", "PR-00801", "PR-00802", "PR-00803", "PR-00804", "PR-00805", "PR-00806", 
                                                 "PR-00900", "PR-00901", "PR-00902", "PR-01001", "PR-01002", "PR-01003", "PR-01004", "PR-01100", "PR-01101", "PR-01102", "PR-01200", "PR-01300", "PR-01400", "PR-01500", 
                                                 "PR-01600", "PR-01700", "PR-01800", "PR-01900", "PR-02000", "PR-02100", "PR-02200", "PR-02300", "PR-02400", "PR-02500", "PR-02600"))

# sp_a <- geocorr::puma2000_to_puma2012 %>% select(matches("stab|puma"), -matches('name')) %>% distinct() %>% mutate(state_puma = get_state_puma(stab, puma12)) %>% mutate(state_puma_2k = get_state_puma(stab, puma2k)) %>% select(matches('state_puma')) %>% unlist() %>% unique()
# sp_b <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt") %>% select(matches('state|puma')) %>% distinct() %>% mutate(state = recode_state(STATEFP)) %>% mutate(state_puma = get_state_puma(state, PUMA5CE)) %>%  .$state_puma %>% unique()
# sp_c <- tbl(con, "puma_dictionary") %>% select(matches("state_puma")) %>% as_tibble() %>% .$state_puma %>% unique()
# setdiff(sp_c, sp_a) %>% length()
# setdiff(sp_c, c(sp_a, sp_b)) %>% length()
# setdiff(sp_c, c(sp_a, sp_b, state_puma_extra_df$state_puma)) %>% length()
# length(sp_a)
# length(sp_b)
# length(sp_c)

if(!exists("state_puma_reference")){
  sp_a <- geocorr::puma2000_to_puma2012 %>% select(matches("stab|puma"), -matches('name')) %>% distinct() %>% mutate(state_puma = get_state_puma(stab, puma12)) %>% select(state=stab, state_puma) %>% distinct() #%>% unlist() %>% unique()
  sp_b <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt") %>% select(matches('state|puma')) %>% distinct() %>% mutate(state = recode_state(STATEFP)) %>% mutate(state_puma = get_state_puma(state, PUMA5CE)) %>% select(state, state_puma) %>% distinct()
  state_puma_reference <- bind_rows(sp_a, sp_b, state_puma_extra_df) %>% distinct()
  # tryCatch({
  #   puma_dictionary <- tbl(con, "puma_dictionary") %>% select(matches("state|state_puma")) %>% as_tibble() %>% group_by(state) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('', 'NA')) %>% ungroup() 
  # }, 
  # error=function(e){
  #   con <- tryCatch({RPostgreSQL::dbConnect(DBI::dbDriver("PostgreSQL"), dbname="postgres", host="diversity-planning-data.postgres.database.azure.com", port=5432, user="diversityplanner@diversity-planning-data", password="jacksonlewisdatascience1!")}, error=function(e){ catn("Method 1 failed, so using method 2"); RPostgreSQL::dbConnect(RPostgres::Postgres(), dbname="postgres", host="diversity-planning-data.postgres.database.azure.com", port=5432, user="diversityplanner@diversity-planning-data", password="jacksonlewisdatascience1!")})
  #   puma_dictionary <- tbl(con, "puma_dictionary") %>% select(matches("state|state_puma")) %>% as_tibble() %>% group_by(state) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('', 'NA')) %>% ungroup() 
  # })
}

if(!exists("df_1_state_per_row_puma_ref")){
  df_1_state_per_row_puma_ref <- state_puma_reference %>% group_by(state) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('', 'NA')) %>% ungroup() 
}

recode_state_to_puma <- function(state_abb="WY"){
  state_pumas_str <- df_1_state_per_row_puma_ref %>% filter(state %in% toupper(state_abb)) %>% .$state_puma
  return(state_pumas_str)
}


if(!exists("df_1_state_per_row_msa_ref")){
  df_1_state_per_row_msa_ref <- puma_msa_ref %>% select(matches("state_msa|^state$")) %>% group_by(state) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('', 'NA')) %>% ungroup() 
}

recode_state_to_msa <- function(state_abb="WY"){
  STATE_ABB_STR <- state_abb
  state_msa_str <- df_1_state_per_row_msa_ref %>% filter(state_abb %in% toupper(STATE_ABB_STR)) %>% .$msa_code
  return(state_msa_str)
}


cat("L348_FLAG ")
# city="milton"       ;state_abb="ga" 
# city="mt. pleasant" ;state_abb="sc"
# city="new fairview" ;state_abb="tx"

recode_city_state_to_zipcode <- function(city="kona", state_abb="hi"){ # {city="st. marys"; state_abb="pa"}; #{city="bedford park"; state_abb="il"}
  # catn('city'); print(city); catn('state_abb'); print(state_abb)
  if(length(city)>1|length(state_abb)>1){cat('\nALERT: Did you forget to make your dataframe rowwise() before running recode_city_state_to_zipcode()??\n')}
  if(tolower(city) != 'remote'&tolower(city) != '.*'){
    # if(!exists('zip_code_db')){
    #   # zip_puma_ref <- geocorr::zcta2010_to_puma2012 %>% select(-matches('intpt|pop10|afact')) %>% mutate(zip = zcta5) %>% mutate_all(function(v) tolower(iconv(enc2utf8(v))))
    #   zip_code_db <<- (zip_code_db_github <- read_csv('https://raw.githubusercontent.com/DataUSA/datausa-tutorials/master/commuting_viz_tutorial/csv/zip_code_database.csv') %>% mutate(major_city=primary_city, zipcode=pad_leading_0s(zip), lat=latitude, lng=longitude, common_city_list=acceptable_cities %>% blob::vec_cast.blob() ) %>%select(-one_of(setdiff(names(.), names(zipcodeR::zip_code_db)))) %>% mutate(post_office_city = major_city )) %>% bind_rows(zipcodeR::zip_code_db, .) %>% as_tibble()
    # }
    city <- gsub("\\.", "", city)
    result <- tryCatch({
      zipcodeR::search_city(tools::toTitleCase(city), state_abb) %>% mutate_all(as.character) %>% mutate_at(vars(one_of('zipcode')), function(v) pad_leading_0s(v, 5)) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
    }, error=function(e) {print(e); NA}) # summarize(state_puma = paste0(sort(unique(state_puma)), collapse=", "))#group_by(primary_city, state) %>% 
    
    if((is.na(result)|nchar(result)<=5)&!is.na(state_abb)){
      result_ <- tryCatch({
        CityName <- gsub(' ','%20', city) %>% gsub("\\.", "", .) #remove space for URLs
        URL <- paste0("http://photon.komoot.io/api/?q=", CityName, "?state=", state_abb)
        (res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() %>% filter(grepl(paste0(abbtostate(state_abb),"|^",state_abb, "$"), properties_state, ignore.case=T)) %>% select(matches('city|state|zip|post'), everything()))
        result_ <- res %>% drop_na(properties_postcode) %>% filter(!duplicated(properties_postcode)) %>% slice(1:3) %>% mutate(zipcode = na_if_(as.character(srhoads::zipcode5(properties_postcode)))) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('', 'NA') %>% gsub('c\\(\\\"|\\\"|\\)|\\(', "", .) %>% unique_sep_sort2(., ", ")
        result_
      }, error=function(e) {print(e); NA})
      
      if(!is.na(result_)&!is.na(result)){
        result <- paste0(result, ", ", result_) %>% unique_sep_sort(., ', ') %>% na_if('NA')
      } else if(is.na(result)|result=="") {
        result <- result_
      }
    } else if(is.na(state_abb)){
      result_ <- tryCatch({
        CityName <- gsub(' ','%20', city) %>% gsub("\\.", "", .) #remove space for URLs
        URL <- paste0("http://photon.komoot.io/api/?q=", CityName)
        (res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() %>% select(matches('city|state|zip|post'), everything()))
        result_ <- res %>% drop_na(properties_postcode) %>% filter(!duplicated(properties_postcode)) %>% slice(1) %>% mutate(zipcode = na_if_(as.character(srhoads::zipcode5(properties_postcode)))) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('', 'NA') %>% gsub('c\\(\\\"|\\\"|\\)|\\(', "", .) %>% unique_sep_sort2(., ", ")
        result_
      }, error=function(e) {print(e); NA})
      
      if(!is.na(result_)&!is.na(result)){
        result <- paste0(result, ", ", result_) %>% unique_sep_sort(., ', ') %>% na_if('NA')
      } else if(is.na(result)|result=="") {
        result <- result_
      }
    }

    if(is.na(result)|nchar(result)<=5){
      result_ <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
          filter(tolower(stab)==tolower(state_abb), 
                 grepl(paste0("\\b", city, "\\b"), zipname, ignore.case=T)
          ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
      
      if(!is.na(result_)&!is.na(result)){
        result <- paste0(result, ", ", result_) %>% unique_sep_sort(., ', ') %>% na_if('NA')
      } else if(is.na(result)|result=="") {
        result <- result_
      }
    }
    
    if(is.na(result)){ #here
      result <- tryCatch({
        # pkg('noncensus'); # install.packages('https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz', repos=NULL)
        city_str <- city %>% gsub('\\b(S|m)(t)\\b', '\\1.*\\2 ', ., ignore.case=T) %>% strip_punct(replacewith = ".*") %>% paste0("\\b", ., "\\b") %>% trimws_()
        result <- zip_codes %>% 
          mutate(city_state = paste0(city, ', ', state)) %>%
          filter_if(is.factorchar, any_vars(grepl(paste0(city_str, '.*', state_abb), ., ignore.case=T))) %>% as_tibble() %>% 
          mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
          select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
        result
      }, error=function(e) {print(e); NA})
    }
    
    if(is.na(result)){
      # xmlToList <- XML::xmlToList
      pkg('XML') 
      result <- tryCatch({ # remotes::install_github("hansthompson/rusps")
        rusps::validate_address_usps(street='1 1st St', city=city, state=state_abb, username='448JL0000161') %>% as_tibble() %>% 
          mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
          select(zip=Zip5) %>% drop_na(zip) %>% .$zip %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e){NA})
    }
    if(is.na(result)){ # city="milton"; state_abb="ga"
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
          filter(tolower(stab)==tolower(state_abb), 
                 grepl(paste0("\\b", city, "\\b"), zipname, ignore.case=T)
          ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    
    if(is.na(result)){
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
          filter(tolower(stab)==tolower(state_abb), 
                 grepl(paste0("\\b", city %>% strip_punct(replacewith = ".*"), "\\b"), zipname, ignore.case=T)
          ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    if(is.na(result)){
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
          filter(tolower(stab)==tolower(state_abb), 
                 grepl(paste0("\\b", city %>% strip_punct(replacewith = ".*"), ""), zipname, ignore.case=T)
          ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    if(is.na(result)){
      city_stripped <- trimws_(gsub("\\b(mt|ft|pt|st|west|south|north|east|sw|ne|nw|se|mount|fort|port|saint|township|new|lake of the|city(| of))\\b", "", tolower(city)))
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
          filter(tolower(stab)==tolower(state_abb), 
                 grepl(paste0("\\b", city_stripped), zipname, ignore.case=T)
          ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    if(is.na(result)){
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
          filter(tolower(stab)==tolower(state_abb), 
                 grepl(paste0("\\b", city_stripped), pum_aname, ignore.case=T)
          ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    if(is.na(result)){
      addrs <- c('1000 1st','1000 2nd', '1000 3rd', '1000 4th', '1000 5th', '1000 Park', '1000 Main', '1000 Oak', '1000 Pine', '7499 Donna')
      for(addr in addrs){
        if(is.na(result)){
          result <- tryCatch({
            rusps::validate_address_usps(street=addr, city=city, state=state_abb, username='448JL0000161') %>% as_tibble() %>% 
              mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
              select(zipcode=Zip5) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
          }, error=function(e){NA})
        }
      }
    }
    if(is.na(result)){
      result <- tryCatch({
        pkg('noncensus'); # install.packages('https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz', repos=NULL)
        city_str <- city
        data(zip_codes)
        result <- zip_codes %>% 
          mutate(city_state = paste0(city, ', ', state)) %>%
          filter_if(is.factorchar, any_vars(grepl(paste0(city_str, '.*', state_abb), ., ignore.case=T))) %>% as_tibble() %>% 
          mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
          select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
        result
      }, error=function(e) {print(e); NA})
    }
    if(is.na(result)){
      # pkg('usa')
      result <- tryCatch({
        city_str <- city
        result <- usa::zipcodes %>% 
          mutate(city_state = paste0(city, ', ', state)) %>%
          filter_if(is.factorchar, any_vars(grepl(paste0(city_str, '.*', state_abb), ., ignore.case=T))) %>% as_tibble() %>% 
          mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
          select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
        result
      }, error=function(e) {print(e); NA})
    }
    if(is.na(result)){
      result <- tryCatch({
        CityName <- gsub(' ','%20', city) #remove space for URLs
        URL <- paste0("http://photon.komoot.io/api/?q=", CityName, "?state=", state_abb)
        (res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() %>% filter(grepl(paste0(abbtostate(state_abb),"|^",state_abb, "$"), properties_state, ignore.case=T)) %>% select(matches('city|state|zip|post'), everything()))
        result <- res  %>% mutate(zipcode = na_if_(as.character(srhoads::zipcode5(properties_postcode)))) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
        result
      }, error=function(e) {print(e); NA})
    }
    #here    
    if(is.na(result)){
      # pkg('usa')
      result <- tryCatch({
        city_str <- city %>% gsub('\\b(St)\\b', 'S.*t ', ., ignore.case=T) %>% strip_punct(replacewith = ".*") %>% paste0("\\b", ., "\\b") %>% trimws_()
        result <- usa::zipcodes %>% 
          mutate(city_state = paste0(city, ', ', state)) %>%
          filter_if(is.factorchar, any_vars(grepl(paste0(city_str, '.*', state_abb), ., ignore.case=T))) %>% as_tibble() %>% 
          mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
          select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
        result
      }, error=function(e) {print(e); NA})
    }
    
    if(is.na(result)){ # city="milton"; state_abb="ga"
      city_str <- city %>% gsub('\\b(St)\\b', 'S.*t ', ., ignore.case=T) %>% strip_punct(replacewith = ".*") %>% paste0("\\b", ., "\\b") %>% trimws_()
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
          filter(tolower(stab)==tolower(state_abb), 
                 grepl(city_str, zipname, ignore.case=T)
          ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    
    
    if(is.na(result)|nchar(result)<=5){
      CityName <- city %>% gsub("\\.|new", "", ., ignore.case=T) %>% trimws_() %>% gsub(' ','%20', .) #remove space for URLs
      result_ <- tryCatch({
        URL <- paste0("http://photon.komoot.io/api/?q=", CityName, "?state=", state_abb)
        (res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() %>% filter(grepl(paste0(abbtostate(state_abb),"|^",state_abb, "$"), properties_state, ignore.case=T)) %>% select(matches('city|state|zip|post'), everything()))
        if(!"properties_postcode" %in% names(res)){
          URL <- paste0("http://photon.komoot.io/api/?q=", res$properties_county[[1]], "?state=", state_abb) %>% gsub(' ','%20', .)
          (res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() %>% filter(grepl(paste0(abbtostate(state_abb),"|^",state_abb, "$"), properties_state, ignore.case=T)) %>% select(matches('city|state|zip|post'), everything()))
        }
        result_ <- res %>% drop_na(properties_postcode) %>% filter(!duplicated(properties_postcode)) %>% slice(1:3) %>% mutate(zipcode = na_if_(as.character(srhoads::zipcode5(properties_postcode)))) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('', 'NA') %>% gsub('c\\(\\\"|\\\"|\\)|\\(', "", .) %>% unique_sep_sort2(., ", ")
        result_
      }, error=function(e) {print(e); NA})
      
      if(!is.na(result_)&!is.na(result)){
        result <- paste0(result, ", ", result_) %>% unique_sep_sort(., ', ') %>% na_if('NA')
      } else if(is.na(result)|result=="") {
        result <- result_
      }
    } else {
      CityName <- city
    }
    
    if(is.na(result)){ #here
      result <- tryCatch({
        # pkg('noncensus'); # install.packages('https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz', repos=NULL)
        city_str <- CityName %>% gsub('\\b(S|m)(t)\\b', '\\1.*\\2 ', ., ignore.case=T) %>% strip_punct(replacewith = ".*") %>% paste0("\\b", ., "\\b") %>% trimws_()
        result <- zip_codes %>% 
          mutate(city_state = paste0(city, ', ', state)) %>%
          filter(state %in% toupper(state_abb)) %>%
          filter_if(is.factorchar, any_vars(grepl(paste0(city_str, '.*', state_abb), ., ignore.case=T))) %>% as_tibble() %>% 
          mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
          select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
        result
      }, error=function(e) {print(e); NA})
    }
    
    result <- unique_sep_sort2(result)
    
  } else {
    result <- NA
  }
  return(result)
}


# recode_city_state_to_zipcode <- function(city="reseda", state_abb="ca"){ # {city="pennington"; state_abb="sd"}
#   # catn('city'); print(city); catn('state_abb'); print(state_abb)
#   if(length(city)>1|length(state_abb)>1){cat('\nALERT: Did you forget to make your dataframe rowwise() before running recode_city_state_to_zipcode()??\n')}
#   if(tolower(city) != 'remote'&tolower(city) != '.*'){
#     # zip_code_db <- read_csv('https://raw.githubusercontent.com/DataUSA/datausa-tutorials/master/commuting_viz_tutorial/csv/zip_code_database.csv') %>% mutate(major_city = primary_city)
#     # if(!exists('zip_code_db')){
#     #   # zip_puma_ref <- geocorr::zcta2010_to_puma2012 %>% select(-matches('intpt|pop10|afact')) %>% mutate(zip = zcta5) %>% mutate_all(function(v) tolower(iconv(enc2utf8(v))))
#     #   zip_code_db <<- (zip_code_db_github <- read_csv('https://raw.githubusercontent.com/DataUSA/datausa-tutorials/master/commuting_viz_tutorial/csv/zip_code_database.csv') %>% mutate(major_city=primary_city, zipcode=pad_leading_0s(zip), lat=latitude, lng=longitude, common_city_list=acceptable_cities %>% blob::vec_cast.blob() ) %>%select(-one_of(setdiff(names(.), names(zipcodeR::zip_code_db)))) %>% mutate(post_office_city = major_city )) %>% bind_rows(zipcodeR::zip_code_db, .) %>% as_tibble()
#     # }
#     result <- tryCatch({
#       zipcodeR::search_city(tools::toTitleCase(city), state_abb) %>% mutate_all(as.character) %>% mutate_at(vars(one_of('zipcode')), function(v) pad_leading_0s(v, 5)) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#     }, error=function(e) {print(e); NA}) # summarize(state_puma = paste0(sort(unique(state_puma)), collapse=", "))#group_by(primary_city, state) %>% 
#     
#     if(is.na(result)){
#       # xmlToList <- XML::xmlToList
#       pkg('XML') 
#       result <- tryCatch({
#         # remotes::install_github("hansthompson/rusps")
#         rusps::validate_address_usps(street='1 1st St', city=city, state=state_abb, username='448JL0000161') %>% as_tibble() %>% 
#           mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
#           select(zip=Zip5) %>% drop_na(zip) %>% .$zip %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#       }, error=function(e){NA})
#     }
#     
#     # if(is.na(result)){
#     #   result <- tryCatch({
#     #     rusps::validate_address_usps(street='1000 1st Ave', city=city, state=state_abb, username='448JL0000161') %>% as_tibble() %>% 
#     #       mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
#     #       select(zip=Zip5) %>% drop_na(zip) %>%
#     #       .$zip %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#     #   }, error=function(e){NA})
#     # }
#     # if(is.na(result)){
#     #   addrs <- c('1000 1st','1000 2nd', '1000 3rd', '1000 4th', '1000 5th', '1000 Park', '1000 Main', '1000 Oak', '1000 Pine', '7499 Donna')
#     #   for(addr in addrs){
#     #     if(is.na(result)){
#     #       result <- tryCatch({
#     #         rusps::validate_address_usps(street=addr, city=city, state=state_abb, username='448JL0000161') %>% as_tibble() %>% 
#     #           mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
#     #           select(zip=Zip5) %>% drop_na(zip) %>%
#     #           .$zip %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#     #       }, error=function(e){NA})
#     #     }
#     #   }
#     # }
#     
#     # if(is.na(result)){
#     #     result <- tryCatch({
#     #         zipcodeR::search_city(city, state_abb) %>% mutate_all(as.character) %>% mutate_at(vars(one_of('zip')), function(v) pad_leading_0s(v, 5)) %>%
#     #             .$zip %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#     #     }, error=function(e) {print(e); NA})
#     # }
#     
#     # if(is.na(result)){
#     #     # city="kansas city"; state_abb="mo"
#     #     result <- tryCatch({
#     #         options(tigris_use_cache = TRUE)
#     #         tigris::zctas(state=state_abb, year=2017, starts_with='07') %>% as_tibble() %>% janitor::clean_names() %>% select(matches('state|puma|name')) %>%
#     #             filter(grepl(city, namelsad10, ignore.case=T))%>%
#     #             .$zip %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#     #     }, error=function(e) {print(e); NA})
#     # }
#     
#     if(is.na(result)){
#       # city="milton"; state_abb="ga"
#       result <- tryCatch({
#         geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
#           filter(tolower(stab)==tolower(state_abb), 
#                  grepl(paste0("\\b", city, "\\b"), zipname, ignore.case=T)
#           ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#       }, error=function(e) {print(e); NA})
#     }
#     
#     if(is.na(result)){
#       result <- tryCatch({
#         geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
#           filter(tolower(stab)==tolower(state_abb), 
#                  grepl(paste0("\\b", city %>% strip_punct(replacewith = ".*"), "\\b"), zipname, ignore.case=T)
#           ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#       }, error=function(e) {print(e); NA})
#     }
#     
#     if(is.na(result)){
#       result <- tryCatch({
#         geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
#           filter(tolower(stab)==tolower(state_abb), 
#                  grepl(paste0("\\b", city %>% strip_punct(replacewith = ".*"), ""), zipname, ignore.case=T)
#           ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#       }, error=function(e) {print(e); NA})
#     }
#     if(is.na(result)){
#       city <- tolower(city)
#       city_stripped <- trimws_(gsub("\\b(mt|ft|pt|st|west|south|north|east|sw|ne|nw|se|mount|fort|port|saint|township|new|lake of the|city(| of))\\b", "", city))
#       result <- tryCatch({
#         geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
#           filter(tolower(stab)==tolower(state_abb), 
#                  grepl(paste0("\\b", city_stripped), zipname, ignore.case=T)
#           ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#       }, error=function(e) {print(e); NA})
#     }
#     
#     if(is.na(result)){
#       result <- tryCatch({
#         geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% rename(zipcode=zcta5) %>% select(matches('stab|puma|name|zip')) %>%
#           filter(tolower(stab)==tolower(state_abb), 
#                  grepl(paste0("\\b", city_stripped), pum_aname, ignore.case=T)
#           ) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#       }, error=function(e) {print(e); NA})
#     }
#     
#     if(is.na(result)){
#       addrs <- c('1000 1st','1000 2nd', '1000 3rd', '1000 4th', '1000 5th', '1000 Park', '1000 Main', '1000 Oak', '1000 Pine', '7499 Donna')
#       for(addr in addrs){
#         if(is.na(result)){
#           result <- tryCatch({
#             rusps::validate_address_usps(street=addr, city=city, state=state_abb, username='448JL0000161') %>% as_tibble() %>% 
#               mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
#               select(zipcode=Zip5) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#           }, error=function(e){NA})
#         }
#       }
#     }
#     
#     if(is.na(result)){
#       result <- tryCatch({
#         pkg('noncensus'); # install.packages('https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz', repos=NULL)
#         city_str <- city
#         data(zip_codes)
#         result <- zip_codes %>% 
#           mutate(city_state = paste0(city, ', ', state)) %>%
#           filter_if(is.factorchar, any_vars(grepl(paste0(city_str, '.*', state_abb), ., ignore.case=T))) %>% as_tibble() %>% 
#           mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
#           select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#         result
#       }, error=function(e) {print(e); NA})
#     }
#     
#     if(is.na(result)){
#       # pkg('usa')
#       result <- tryCatch({
#         city_str <- city
#         result <- usa::zipcodes %>% 
#           mutate(city_state = paste0(city, ', ', state)) %>%
#           filter_if(is.factorchar, any_vars(grepl(paste0(city_str, '.*', state_abb), ., ignore.case=T))) %>% as_tibble() %>% 
#           mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
#           select(zipcode=zip) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#         result
#       }, error=function(e) {print(e); NA})
#       # zip_codes %>% 
#       #   mutate(city_state = paste0(city, ', ', state)) %>%
#       #   filter_if(is.factorchar, any_vars(grepl(paste0('pennington', '.*', 'SD'), ., ignore.case=T)))
#     }
#     
#     if(is.na(result)){
#       result <- tryCatch({
#         CityName <- gsub(' ','%20', city) #remove space for URLs
#         URL <- paste0("http://photon.komoot.io/api/?q=", CityName, "?state=", state_abb)
#         (res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names())
#         result <- res %>% filter(grepl(abbtostate(state_abb), properties_state)) %>% mutate(zipcode = as.character(srhoads::zipcode5(properties_postcode))) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
#         result
#       }, error=function(e) {print(e); NA})
#     }
#       # library(RJSONIO)
#       # nrow <- nrow(test)
#       # counter <- 1
#       # test$lon[counter] <- 0
#       # test$lat[counter] <- 0
#       # while (counter <= nrow){
#       # CityName <- gsub(' ','%20', city) #remove space for URLs
#       #   url <- paste(
#       #     "http://nominatim.openstreetmap.org/search?city="
#       #     , CityName
#       #     , "&state="
#       #     , abbtostate(state_abb)
#       #     , "&countrycodes="
#       #     , "US"
#       #     , "&limit=9&format=json&addressdetails=1&polygon_svg=0"
#       #     , sep="")
#       #   url <- paste("http://nominatim.openstreetmap.org/search?city=", "Sunnyvale", "&state=", "CA", "&countrycodes=", "US", "&limit=9&format=json&addressdetails=1&polygon_svg=0", sep="")
#       #   (x <- jsonlite::fromJSON(url))
#       #   # revgeo::revgeo(x$lon, x$lat)
#       #   d <- revgeo(paste0(x$lon, ', ', x$lat))
#       #   d
#       #   # if(is.vector(x)){
#       #   #   test$lon[counter] <- x[[1]]$lon
#       #   #   test$lat[counter] <- x[[1]]$lat    
#       #   # }
#       #     # counter <- counter + 1
#       #   # }
#       # }
#       
#       # URL <- paste0("http://photon.komoot.de/reverse?lon=", lonlat_[1], "&lat=", lonlat_[2], "")
#       # URL <- paste0("http://photon.komoot.de/reverse?lon=", "")
#       # res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() # lapply(bind_rows) %>% bind_cols()
#       # URL <- "http://photon.komoot.io/api/?q=sunnyvale?state=CA"
#     } else {
#       result <- NA
#     }
#     return(result)
#   }










geocode_zipcode <- function(v, return_vec_latlon=T){
  # {  if(!require(zipcode)) {install.packages("https://cran.r-project.org/src/contrib/Archive/zipcode/zipcode_1.0.tar.gz", type="source", repos=NULL); .rs.restartR(); library(zipcode)}   }
  library(zipcode); data(zipcode)
  zipcode$latlon <- gsub('NA, NA', NA, paste0(zipcode$latitude, ', ', zipcode$longitude))
  if(return_vec_latlon){
    result <- zipcode$latlon[match(v, zipcode$zip)]
  } else {
    result <- dplyr::left_join(tibble(zip=v), zipcode)
  }
  return(result)
}

recode_city_state_to_puma_state <- function(city, state_abb){
  if(tolower(city) != 'remote'){
    
    if(!exists('zip_code_db')){
      zip_puma_ref <- geocorr::zcta2010_to_puma2012 %>% select(-matches('intpt|pop10|afact')) %>% mutate(zip = zcta5) %>% mutate_all(function(v) tolower(iconv(enc2utf8(v))))
      zip_code_db <<- (zip_code_db_github <- read_csv('https://raw.githubusercontent.com/DataUSA/datausa-tutorials/master/commuting_viz_tutorial/csv/zip_code_database.csv') %>% 
                         mutate(major_city=primary_city, zipcode=pad_leading_0s(zip), lat=latitude, lng=longitude, common_city_list=acceptable_cities %>% blob::vec_cast.blob() ) %>%
                         select(-one_of(setdiff(names(.), names(zipcodeR::zip_code_db)))) %>%
                         # filter(!zipcode %in% zipcodeR::zip_code_db$zipcode) %>%
                         mutate(post_office_city = major_city )
      ) %>%
        bind_rows(zipcodeR::zip_code_db, .) %>%
        as_tibble()
    }
    # zip_code_db <- zipcodeR::zip_code_db
    # zip_code_db <- zip_code_db_github
    # zip_code_db %>% filter(zipcode=='36101')
    # zipcodeR::zip_code_db %>% filter(zipcode=='36101')
    # zip_code_db_github %>% filter(zipcode=='36101')
    # zipcodeR::geocode_zip("36101")
    # 
    # res <- ggmap::revgeocode(c(-86.3, 32.4), output="more")
    # pkg('revgeo')
    # (what <- revgeo::revgeo(-86.3, 32.4, output='hash'))
    # (what <- revgeo::revgeo(-86.3, 32.4, output='frame'))
    # (what <- revgeo::revgeo(-40.6, 73.9, output='frame'))
    # (what <- revgeo::revgeo(-40.6342, 73.9143, output='frame'))
    # revgeo(longitude=-77.0229529, latitude=38.89283435)
    # what
    # 
    # res = 
    # rawToChar(res$content)
    # res <- jsonlite::fromJSON(rawToChar(httr::GET("http://photon.komoot.de/reverse?lon=-86.3&lat=32.4")$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() # lapply(bind_rows) %>% bind_cols()
    # res
    # 
    # revgeo(lonlat='-86.3, 32.4', 'zip')
    
    
    BING_API_KEY = "AiwaE_mkhXssMiIuUtG6k2a02Cs9TWeo3npZTs3ZyZLQnDxiRrxHeFfSkNeCE_8t"
    # rowwise() %>%
    # mutate(common_city_list=strsplit(acceptable_cities, ', ') %>% sapply(blob::vec_cast.blob)  ) %>% # mutate(common_city_list=strsplit(acceptable_cities, ', ') %>% as.vector() ) %>% # mutate(common_city_list=acceptable_cities %>% sapply(as.vector) ) %>%
    # ungroup()
    
    #  hexToText <- function(msg){
    #      hex <- sapply(seq(1, nchar(as.character(msg)), by=2), function(x) substr(msg, x, x+1))
    #      hex <- subset(hex, !hex == "00")
    #      gsub('[^[:print:]]+', '', rawToChar(as.raw(strtoi(hex, 16L))))
    #  }
    #  
    #  zipcodeR::zip_code_db %>% as_tibble()
    #  zip_code_db %>% slice(nrow(.)-20:nrow(.)) %>% .$common_city_list %>% .[1]
    #  zip_code_db %>% slice(1:10) %>% .$common_city_list %>% .[1]
    #  # rawToChar()
    #  myblob <- zip_code_db %>% slice(1:10) %>% .$common_city_list %>% .[1]
    #  myblob_ <- zip_code_db %>% slice(1:10) %>% .$common_city_list %>% .[[1]]
    #  
    #  myblob <- zip_code_db %>% .$common_city_list %>% .[length(.)]
    #  
    #  (myblob <- zip_code_db %>% .$common_city_list %>% .[24])
    #  (myblob_ <- zip_code_db %>% .$common_city_list %>% .[[24]])
    #  
    #  (myblob <- zipcodeR::zip_code_db %>% .$common_city_list %>% .[24])
    #  (myblob_ <- zipcodeR::zip_code_db %>% .$common_city_list %>% .[[24]])
    #  
    #  (myblob <- zip_code_db %>% .$common_city_list %>% rev() %>% .[24])
    #  (myblob_ <- zip_code_db %>% .$common_city_list %>% rev() %>% .[[24]])
    #  
    #  (myblobs <- zip_code_db %>% .$common_city_list %>% .[c(1:10, (length(.)-30):length(.))])
    #  rawToChar(myblobs[[1]] %>% .[.!='00'])
    #  sapply(myblobs, function(x) x %>% .[.!='00'] %>% rawToChar())
    # # (myblob_ <- zip_code_db %>% .$common_city_list %>% .[1] %>% hexToText()) #%>% .[[50]]
    #  # blob::validate_blob(myblob) %>% rawToChar()
    #  blob::is_blob(myblob)
    #  library(blob)
    #  vec_restore
    #  vctrs::vec_restore(myblob, to=character())
    #  # vctrs::vec_cast(myblob, to=character())
    #  rawToBits(myblob_)
    #  rawConnectionValue(myblob_)
    #  myblob_ %>% .[.!='00'] %>% rawToChar()
    #  myblob_ %>% .[.!='00'] %>% rawToChar()
    #  
    #  zipcodeR::zip_code_db %>% as_tibble() %>% select(1:5) %>% lapply(class)
    #  zipcodeR::zip_code_db %>% as_tibble() %>% select(1:5) %>% lapply(class)
    #  str(myblob)
    #  class(myblob)
    #  class(myblob_)
    #  rawToChar(myblob_)
    #  
    #  zipcodeR::zip_code_db %>% slice(1:10) %>% .$common_city_list %>% class()
    #  # blob::as_blob(zip_code_db %>% drop_na(acceptable_cities) %>% select(1:3))
    #  blob::vec_cast.blob(c('sam', 'cat'))
    #  # zip_code_db %>% drop_na(acceptable_cities) %>% .$common_city_list %>% .[1:10]
    #  # full_join(zipcodeR::zip_code_db, zip_code_db %>% select(-matches('lng|lat'))) %>% as_tibble()
    
    result <- tryCatch({
      zipcodeR::search_city(tools::toTitleCase(city), state_abb) %>% mutate_at(vars(one_of('zipcode')), function(v) pad_leading_0s(v, 5)) %>%
        left_join(., zip_puma_ref, by='zip', suffix=c('', '_y')) %>%
        mutate(state_puma = paste0(state, '-', puma12) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
        .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
    }, error=function(e) {print(e); NA}) # summarize(state_puma = paste0(sort(unique(state_puma)), collapse=", "))#group_by(primary_city, state) %>% 
    
    if(is.na(result)){
      result <- tryCatch({
        rusps::validate_address_usps(street='1 1st St', city=city, state=state_abb, username='448JL0000161') %>% as_tibble() %>% 
          mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
          select(zip=Zip5) %>% drop_na(zip) %>%
          left_join(., zip_puma_ref, by='zip', suffix=c('', '_y')) %>%
          mutate(state_puma = paste0(toupper(stab), '-', puma12) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
          .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e){NA})
    }
    
    if(is.na(result)){
      result <- tryCatch({
        rusps::validate_address_usps(street='1 1st Ave', city=city, state=state_abb, username='448JL0000161') %>% as_tibble() %>% 
          mutate_all(., function(v) recode_na(as.character(v), 'NA')) %>% #.$Zip5 %>% na.omit()
          select(zip=Zip5) %>% drop_na(zip) %>%
          left_join(., zip_puma_ref, by='zip', suffix=c('', '_y')) %>%
          mutate(state_puma = paste0(toupper(stab), '-', puma12) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
          .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e){NA})
    }
    
    if(is.na(result)){
      result <- tryCatch({
        zipcodeR::search_city(city, state_abb) %>% mutate_at(vars(one_of('zip')), function(v) pad_leading_0s(v, 5)) %>%
          left_join(., zip_puma_ref, by='zip', suffix=c('', '_y')) %>%
          mutate(state_puma = paste0(toupper(state_abb), '-', puma12) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
          .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    
    if(is.na(result)){
      # city="kansas city"; state_abb="mo"
      result <- tryCatch({
        tigris::pumas(state=state_abb) %>% as_tibble() %>% janitor::clean_names() %>% select(matches('state|puma|name')) %>%
          filter(grepl(city, namelsad10, ignore.case=T))%>%
          mutate(state_puma = paste0(toupper(state_abb), '-', pumace10) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
          .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    
    if(is.na(result)){
      # city="milton"; state_abb="ga"
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% select(matches('state|puma|name|zip')) %>%
          mutate(state = recode_state(state)) %>%
          filter(grepl(paste0("\\b", city, "\\b"), zipname, ignore.case=T), tolower(state_abb)==tolower(state)) %>%
          mutate(state_puma = paste0(toupper(state), '-', puma12) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
          .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    
    if(is.na(result)){
      # city="Boston"; state_abb="ma"
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% select(matches('state|puma|name|zip')) %>%
          mutate(state = recode_state(state)) %>%
          filter(grepl(paste0("\\b", city %>% strip_punct(replacewith = ".*"), "\\b"), zipname, ignore.case=T), tolower(state_abb)==tolower(state)) %>%
          mutate(state_puma = paste0(toupper(state), '-', puma12) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
          .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    
    if(is.na(result)){
      # city="milton"; state_abb="ga"
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% select(matches('state|puma|name|zip')) %>%
          mutate(state = recode_state(state)) %>%
          filter(grepl(paste0("\\b", city %>% strip_punct(replacewith = ".*"), ""), zipname, ignore.case=T), tolower(state_abb)==tolower(state)) %>%
          mutate(state_puma = paste0(toupper(state), '-', puma12) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
          .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    # city="Narcoossee"; state_abb="FL"
    if(is.na(result)){
      city <- tolower(city)
      city_stripped <- trimws_(gsub("\\b(mt|ft|pt|st|west|south|north|east|sw|ne|nw|se|mount|fort|port|saint|township|city(| of))\\b", "", city))
      # city="milton"; state_abb="ga"
      result <- tryCatch({
        geocorr::zcta2010_to_puma2012 %>% janitor::clean_names() %>% mutate_all(function(v) tolower(iconv(enc2utf8(v)))) %>% select(matches('state|puma|name|zip')) %>%
          mutate(state = recode_state(state)) %>%
          filter(grepl(paste0("\\b", city_stripped), zipname, ignore.case=T), tolower(state_abb)==tolower(state)) %>%
          mutate(state_puma = paste0(toupper(state), '-', puma12) %>% gsub('.*-NA$|^NA-.*', '', .) %>% recode_na('')) %>%
          .$state_puma %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
      }, error=function(e) {print(e); NA})
    }
    
  } else {
    result <- NA
  }
  return(result)
}






if(recreate_msa_puma_ref<-F){
  
  "https://www.uspto.gov/web/offices/ac/ido/oeip/taf/cls_cbsa/cbsa_countyassoc.htm"
  
  state_pumas_missing_msas <- c(
    "AK-00400", "AL-00300", "AL-00400", "AL-00700", "AL-00901", "AL-00902", "AL-00903", "AL-00904", "AL-00905", "AL-01000", "AL-01300", "AL-02200", "AL-02300", "AR-00400", "AR-00800", "AR-01500", "AR-01900", "AZ-00300", 
    "CA-00100", "CA-00200", "CA-00300", "CA-00400", "CA-00500", "CA-00600", "CA-00700", "CA-02402", "CA-00800", "CA-00900", "CA-01000", "CA-01100", "CA-01101", "CA-01102", "CA-01103", "CA-01201", "CA-01202", "CA-01401", "CA-01402", "CA-01403", "CA-01500", "CA-01501", "CA-01502", "CA-02401", "CA-01503", "CA-01504", "CA-01505", "CA-01506", "CA-01601", "CA-01602", "CA-01800", "CA-02001", "CA-02002", "CA-02101", "CA-02102", "CA-02103", "CA-02104", "CA-02105", "CA-02106", "CA-02107", "CA-02108", "CA-02201", "CA-02202", "CA-02203", "CA-02204", "CA-02205", "CA-02206", "CA-02207", "CA-02300", "CA-02301", "CA-02302", "CA-02303", "CA-02304", "CA-02305", "CA-02306", "CA-02403", "CA-02404", "CA-02405", "CA-02406", "CA-02407", "CA-02408", "CA-02409", "CA-02410", "CA-02601", "CA-02602", "CA-02701", "CA-02702", "CA-02703", "CA-02704", "CA-02705", "CA-02706", "CA-02707", "CA-02708", "CA-02709", "CA-03501", "CA-03502", "CA-02710", "CA-02711", "CA-02712", "CA-02713", "CA-02714", "CA-02801", "CA-02802", "CA-02900", "CA-03001", "CA-03002", "CA-03200", "CA-03300", "CA-03301", "CA-03302", "CA-03401", "CA-03402", "CA-03503", "CA-03600", "CA-03800", "CA-03901", "CA-03902", "CA-03903", "CA-04000", "CA-04100", "CA-04200", "CA-04300", "CA-05419", "CA-04401", "CA-04402", "CA-04403", "CA-04404", "CA-04405", "CA-04406", "CA-04407", "CA-04500", "CA-04600", "CA-04700", "CA-04800", "CA-04900", "CA-05000", "CA-05100", "CA-05200", "CA-05300", "CA-05401", "CA-05402", "CA-05403", "CA-05404", "CA-05405", "CA-05406", "CA-05407", "CA-05408", "CA-05409", "CA-05410", "CA-05411", "CA-05412", "CA-05413", "CA-05414", "CA-05415", "CA-05416", "CA-05417", "CA-05418", "CA-05420", "CA-05421", "CA-05422", "CA-05423", "CA-05424", "CA-05600", "CA-05700", "CA-05701", "CA-05702", "CA-05703", "CA-05800", "CA-05900", "CA-06000", "CA-06104", "CA-06105", "CA-06106", "CA-06107", "CA-06108", "CA-06109", "CA-06110", "CA-06111", "CA-06112", "CA-06113", "CA-06114", "CA-06115", "CA-06116", "CA-06117", "CA-06118", "CA-06119", "CA-06120", "CA-06121", "CA-06122", "CA-06123", "CA-06124", "CA-06125", "CA-06602", "CA-06126", "CA-06200", "CA-06300", "CA-06400", "CA-06500", "CA-06601", "CA-06801", "CA-06802", "CA-06900", "CA-07000", "CA-07100", "CA-07200", "CA-07300", "CA-07400", "CA-07601", "CA-07602", "CA-07603", "CA-07604", "CA-07605", "CA-07606", "CA-07607", "CA-07700", "CA-07801", "CA-07802", "CA-07900", "CA-08001", "CA-08002", "CA-08003", "CA-08004", "CA-08005", "CA-08107", "CA-08108", "CA-08109", "CA-08110", "CA-08111", "CA-08112", "CA-08113", "CA-08114", "CA-08115", "CA-08116", "CA-08200", 
    "CO-00101", "CO-00200", "CO-00400", "CO-00501", "CO-00502", "CO-00900", "CO-00901", "CO-00902", "CO-00903", "CO-00904", "CO-00905", "CO-01000", "CT-00200", "CT-00400", "CT-00500", "CT-00600", "CT-00800", "CT-01000", "CT-01200", "CT-01400", "CT-01600", "CT-01700", "CT-01800", "CT-01900", "CT-02000", "CT-02100", "CT-02200", "CT-02300", "CT-02400", "CT-02500", "FL-00200", "FL-00300", "FL-00400", "FL-00600", "FL-00701", "FL-00702", "FL-00800", "FL-00900", "FL-01001", "FL-01002", "FL-01200", "FL-01300", "FL-01400", "FL-01501", "FL-01502", "FL-01600", "FL-01700", "FL-01801", "FL-01802", "FL-01901", "FL-01902", "FL-01903", "FL-02001", 
    "FL-02002", "FL-02003", "FL-02104", "FL-02201", "FL-02202", "FL-02203", "FL-02204", "FL-02205", "FL-02206", "FL-02207", "FL-02401", "FL-02402", "FL-02403", "FL-02404", "FL-02501", "FL-02502", "FL-02503", "FL-02601", "FL-02602", "FL-02603", "FL-02604", "FL-02605", "FL-02606", "FL-02607", "FL-02608", "FL-02701", "FL-02702", "FL-02703", "FL-02704", "FL-02705", "FL-02706", "FL-02707", "FL-02708", "FL-02801", "FL-02802", "FL-02901", "FL-02902", "FL-03000", "FL-03100", "FL-03200", "FL-03300", "FL-03400", "FL-03501", "FL-03502", "FL-03503", "FL-03504", "FL-03505", "FL-03506", "FL-03507", "FL-03508", "FL-03509", "FL-03601", "FL-03602", "FL-03603", "FL-03604", "FL-03605", "FL-03606", "FL-03607", "FL-03608", "FL-03609", "FL-03610", "FL-03611", "FL-03612", "FL-03613", "FL-03701", "FL-03702", "FL-03800", "FL-03901", "FL-03902", "FL-03903", "FL-04001", "FL-04002", "FL-04003", "FL-04004", "FL-04005", "FL-04006", "FL-04007", "FL-04008", "FL-04009", "FL-04010", "FL-04011", "FL-04012", "FL-04013", "FL-04014", "FL-04015", "FL-04016", "FL-04017", "FL-04018", "FL-04019", "FL-04020", "MN-02100", "FL-12100", 
    "GA-00400", "GA-01000", "GA-01101", "GA-01102", "GA-01103", "GA-01104", "GA-01105", "GA-01106", "GA-01107", "GA-01200", "GA-01201", "GA-01202", "GA-01203", "GA-01204", "GA-01205", "GA-01206", "GA-01300", "GA-01301", "GA-01302", "GA-01303", "GA-01304", "GA-01305", "GA-01401", "GA-01402", "GA-01501", "GA-01502", "GA-01503", "GA-01504", "GA-01505", "GA-02000", "GA-03000", "GA-03100", "GA-03500", "HI-00200", "IA-00100", "IA-00200", "IA-00300", "IA-01900", "IA-02300", "ID-00700", "ID-00900", "ID-01000", 
    "IL-00101", "IL-00102", "IL-00103", "IL-00104", "IL-00200", "IL-00300", "IL-00400", "IL-00500", "IL-00600", "IL-00700", "IL-01000", "IL-01101", "IL-01102", "IL-01201", "IL-01202", "IL-01400", "IL-01600", "IL-01700", "IL-01800", "IL-02400", "IL-02500", "IL-02600", "IL-02700", "IL-02800", "IL-02900", "IL-03001", "IL-03002", "IL-03003", "IL-03004", "IL-03006", "IL-03101", "IL-03103", "IL-03104", "IL-03201", "IL-03206", "IL-03301", "IL-03302", "IL-03303", "IL-03304", "IL-03305", "IL-03402", "IL-03403", "IL-03404", "IL-03405", "IL-03406", "IL-03505", "IL-03506", "IL-03507", "IL-03508", "IL-03509", "IL-03510", "IL-03511", "IL-03512", "IL-03513", "IL-03514", "IL-03515", "IL-03516", "IL-03517", "IL-03518", "IL-03519", 
    "IN-00100", "IN-00201", "IN-00202", "IN-00203", "IN-00400", "IN-00600", "IN-00800", "IN-01000", "IN-01400", "IN-01500", "IN-01800", "IN-01901", "IN-01902", "IN-02001", "IN-02002", "IN-03400", "IN-03700", "IN-03800", "KS-00100", "KS-00200", "KS-00800", "KS-00900", "KS-01000", "KS-01200", "KS-01300", "KS-01401", "KS-01402", "KS-01403", "KS-01500", "KS-01600", "KY-00100", "KY-00200", "KY-00600", "KY-00700", "KY-00800", "KY-00900", "KY-01000", "KY-01100", "KY-02200", "LA-00102", "LA-01000", "LA-01401", "LA-01402", "LA-01803", "LA-01804", "LA-01901", "LA-01902", "LA-01903", "LA-01904", "LA-02001", "LA-02002", "LA-77777", 
    "MA-00500", "MA-00600", "MA-00700", "MA-00800", "MA-00900", "MA-01100", "MA-01200", "MA-02900", "MA-01500", "MA-01700", "MA-01800", "MA-02000", "MA-02100", "MA-02200", "MA-02300", "MA-02500", "MA-02600", "MA-02700", "MA-03000", "MA-03100", "MA-03200", "MA-03600", "MA-03700", "MA-03800", "MA-04100", "MA-04300", "MA-04400", "MA-04600", "MD-00300", "MD-00806", "ME-00100", "ME-00200", "ME-00400", "ME-00500", "MI-00100", "MI-00200", "MI-00300", "MI-00400", "MI-00500", "MI-00600", "MI-00800", "MI-00900", "MI-01000", "MI-01200", "MI-01300", "MI-01401", "MI-01402", "MI-01403", "MI-01600", "MI-01700", "MI-01800", "MI-02100", "MI-02200", "MI-02301", "MI-02302", "MI-02500", "MI-02501", "MI-02502", "MI-02503", "MI-02504", "MI-02505", "MI-02506", "MI-02507", "MI-02508", "MI-02601", "MI-02602", "MI-02700", "MI-02900", "MI-03000", "MI-03200", "MI-03400", "MI-03500", "MI-03600", "MI-03701", "MI-03702", "MI-03703", "MI-03704", "MI-03705", "MI-03706", "MI-03707", "MI-03708", "MI-03801", "MI-03802", "MI-03803", "MI-03804", "MI-03805", "MI-03806", "MI-03807", "MI-03900", "MI-04000", "MI-04101", "MI-04102", "MI-04103", "MI-04104", "MN-00200", "MN-00700", "MN-00800", "MN-01001", "MN-01002", "MN-01100", "MN-01203", "MN-01601", "MN-01602", "MN-02000", "MO-00100", "MO-00300", "MO-00700", "MO-01200", "MO-01400", "MO-01500", "MO-01601", "MO-01602", "MO-01704", "MO-01705", "MO-01706", "MO-01707", "MO-01708", "MO-01900", "MO-02000", "MO-02100", "MO-02300", "MO-02400", "MO-02500", "MO-02600", "MS-00400", "MS-00500", "MS-00600", "MS-00700", "MS-00800", "MS-01400", "MS-01500", "MS-01600", "MS-01700", "MS-02200", "MS-02300", 
    "MT-00100", "MT-00300", "NC-00100", "NC-00200", "NC-00201", "NC-00202", "NC-00600", "NC-00800", "NC-00901", "NC-00902", "NC-00903", "NC-00904", "NC-00905", "NC-01000", "NC-01200", "NC-01300", "NC-01601", "NC-01602", "NC-01700", "NC-01800", "NC-02200", "NC-02400", "NC-02600", "NC-02601", "NC-02602", "NC-02701", "NC-02702", "NC-02703", "NC-02801", "NC-02802", "NC-03000", "NC-03100", "NC-03700", "NC-03800", "NC-03900", "NC-04900", "NC-05100", "ND-00200", "NE-00100", "NE-00400", "NE-00500", "NH-00100", "NH-00200", "NH-00400", "NH-00500", "NH-01100", "NJ-00200", "NM-00300", "NM-00400", "NM-00601", "NM-00602", "NM-00603", "NM-00604", "NM-00605", "NM-00800", "NM-01000", "NM-01100", "NM-01200", "NV-00100", "NV-00300", "NV-00400", "NV-00501", "NV-00502", "NV-00503", "NV-00504", "NV-00505", "NV-00506", "NV-00507", "NV-00508", "NV-00509", "NV-00510", "NV-00511", "NY-00100", "NY-00200", "NY-00700", "NY-00801", "NY-00802", "NY-00803", "NY-00804", "NY-01001", "NY-01002", "NY-01003", "NY-01004", "NY-01005", "NY-01100", "NY-01200", "NY-01401", "NY-01402", "NY-01501", "NY-01502", "NY-01600", "NY-01601", "NY-01602", "NY-01603", "NY-01604", "NY-01605", "NY-01800", "NY-02000", "NY-02100", "NY-02500", "NY-02600", "NY-02601", "NY-02602", "NY-02700", "NY-02800", "NY-02900", "NY-03000", "NY-03400", "NY-03501", "NY-03502", "NY-03503", "NY-03504", "NY-03505", "NY-03506", "NY-03601", "NY-03602", "NY-04201", "NY-04202", "NY-04203", "NY-04204", "NY-04205", "NY-04206", "NY-04207", "NY-04208", "NY-04209", "NY-04210", "NY-04211", "NY-04212", "NY-04301", "NY-04302", "NY-04303", "NY-04304", "NY-04305", "NY-04306", "NY-04307", "NY-04308", "NY-04309", "NY-04310", "NY-04311", "NY-04312", 
    "OH-00100", "OH-00201", "OH-00202", "OH-00301", "OH-00302", "OH-00303", "OH-00501", "OH-00502", "OH-00601", "OH-00602", "OH-00603", "OH-00604", "OH-00605", "OH-00606", "OH-00607", "OH-00608", "OH-00609", "OH-00610", "OH-00611", "OH-00612", "OH-00700", "OH-00701", "OH-00702", "OH-00703", "OH-00800", "OH-01101", "OH-01102", "OH-01103", "OH-01201", "OH-01202", "OH-01300", "OH-01800", "OH-02000", "OH-02100", "OH-02201", "OH-02202", "OH-02203", "OH-02300", "OH-02400", "OH-02600", "OH-02700", "OH-02900", "OH-03000", "OH-03001", "OH-03002", "OH-03101", "OH-03102", "OH-03103", "OH-03104", "OH-03105", "OH-03106", "OH-03107", "OH-03108", "OH-03109", "OH-03400", "OH-03600", "OH-04301", "OH-04302", "OH-04401", "OH-04402", "OH-04403", "OH-04404", "OH-04500", "OH-04501", "OH-04502", "OH-04503", "OH-04800", "OH-05000", "OH-05200", "OK-00400", "OK-00500", "OK-01400", "OK-01500", "OK-00600", "OK-00700", "OK-00701", "OK-00702", "OK-01000", "OK-01100", "OK-01200", "PA-04108", "OK-01600", "OK-01700", "OR-00100", "OR-00200", "OR-00300", "OR-00701", "OR-00702", "OR-00900", "OR-01000", "OR-01101", "OR-01102", "OR-01304", "OR-01306", "OR-01307", "OR-01308", "OR-01309", "OR-01310", "OR-01311", "OR-01312", "OR-01313", "PA-00100", "PA-00200", "PA-00300", "PA-00400", "PA-00700", "PA-00901", "PA-00902", "PA-00903", "PA-01100", "PA-01300", "PA-01500", "PA-01703", "PA-02101", "PA-02102", "PA-02103", "PA-02201", "PA-02202", "PA-02300", "PA-02400", "PA-02501", "PA-02502", "PA-02600", "PA-02700", "PA-02900", "PA-03500", "PA-03600", "PA-03800", "PA-03801", "PA-03802", "PA-03901", "PA-03902", "PA-03903", "PA-03904", "PA-04003", "PA-04004", "PA-04005", "PA-04006", "PA-04101", "PA-04102", "PA-04103", "PA-04104", "PA-04105", "PA-04106", "PA-04107", "PA-04109", "PA-04110", "PA-04111", "PA-04201", "PA-04202", "PA-04203", "PA-04204", "PA-04301", "PA-04302", "PA-04303", 
    "PR-00100", "PR-00200", "PR-00300", "PR-00400", "PR-00500", "PR-00600", "PR-00700", "PR-00900", "PR-01003", "PR-01004", "PR-01100", "PR-01200", "PR-01300", "PR-01400", "PR-01500", "PR-01600", "PR-01700", "PR-01800", "PR-01900", "PR-02000", "PR-02100", "PR-02200", "PR-02300", "PR-02400", "PR-02500", "PR-02600", "RI-00100", "RI-00200", "RI-00500", "RI-00600", "RI-00700", "SC-00100", "SC-00201", "SC-00202", "SC-00500", "SC-00600", "SC-01000", "SC-01001", "SC-01002", "SC-01100", "SC-01200", "SC-01300", "SC-01600", "SC-01700", "SC-01800", "SC-01900", "SC-02000", "SC-02101", "SC-02102", "SC-02200", "SC-02300", "SD-00200", "SD-00300", "SD-00400", "SD-00700", "TN-00200", "TN-00501", "TN-00502", "TN-00700", "TN-00800", "TN-00801", "TN-00802", "TN-01301", "TN-01302", "TN-01600", "TN-02000", "TN-02200", "TN-02201", "TN-02202", "TN-02203", "TN-02204", "TN-02205", "TN-02400", "TN-02500", "TN-02800", "TN-02900", "TN-03101", "TN-03102", "TN-03103", "TN-03104", "TN-03105", "TX-01000", "TX-01300", "TX-01500", "TX-01800", "TX-01900", "TX-02000", "TX-02103", "TX-02104", "TX-02201", "TX-02202", "TX-03300", "TX-03503", "TX-03504", "TX-03505", "TX-03600", "TX-03701", "TX-03702", "TX-03703", "TX-03900", "TX-04000", "TX-04300", "TX-05401", "TX-05402", "TX-05601", "TX-05602", "TX-05603", "TX-05604", "TX-05605", "TX-05606", "TX-05607", "TX-05608", "TX-05609", "TX-05610", "TX-05611", "TX-05900", "TX-06200", "TX-06400", "TX-06600", "TX-06704", "TX-06800", "TX-06900", "UT-00100", "UT-00200", "UT-00301", "UT-00302", "UT-00400", "UT-00501", "UT-00502", "UT-00503", "UT-00504", "UT-00505", "UT-00506", "UT-00507", "UT-00601", "UT-00602", "UT-00603", "UT-00700", "UT-13001", 
    "VA-00100", "VA-00200", "VA-00301", "VA-00302", "VA-00303", "VA-00304", "VA-00305", "VA-00400", "VA-00501", "VA-00502", "VA-00600", "VA-00700", "VA-00800", "VA-00900", "VA-01000", "VA-01100", "VA-01200", "VA-01300", "VA-01400", "VA-01500", "VA-01600", "VA-01700", "VA-01800", "VA-01900", "VA-02000", "VA-02100", "VA-02200", "VA-02300", "VA-02400", "VA-02500", "VA-02600", "VA-02700", "VA-02801", "VA-02802", "VA-02803", "VA-02900", "VA-03000", "VA-03100", "VA-03200", "VA-03300", "VA-03400", "VA-03500", "VA-51097", "VT-00200", "VT-00300", "VT-00400", "WA-00100", "WA-00200", "WA-00300", "WA-00400", "WA-00500", "WA-00601", "WA-00602", "WA-00700", "WA-00800", "WA-00901", "WA-00902", "WA-01001", "WA-01002", "WA-01003", "WA-01004", "WA-01005", "WA-01100", "WA-01201", "WA-01202", "WA-01300", "WA-01401", "WA-01402", "WA-01403", "WA-01404", "WA-01500", "WA-01600", "WA-01701", "WA-01702", "WA-01801", "WA-01802", "WA-01803", "WA-01804", "WA-01805", "WA-01900", "WA-02001", "WA-02002", "WA-02003", "WA-02004", "WA-02005", "WA-02006", "WA-02007", "WA-02008", "WA-02009", "WA-02101", "WA-02102", "WA-02200", "WA-10800", "WA-11300", "WA-11900", 
    "WI-00400", "WI-00500", "WI-00600", "WI-01001", "WI-01100", "WI-01200", "WI-01400", "WI-01601", "WI-01700", "WI-01800", "WI-01900", "WI-02001", "WI-02002", "WI-02003", "WI-02004", "WY-00100", "WI-02101", "WI-02102", "WI-02201", "WI-02202", "WI-02203", "WI-02300", "WI-50000", "WV-00200", "WV-00500", "WV-00600", "WV-01100", "WV-01300", "WY-00200", "WY-00500")   #setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma)
  
  puma_msa_ref_0 <-  rio::import(file="https://usa.ipums.org/usa/resources/volii/MSA2013_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('population')) %>% mutate_all(tolower) %>% 
    mutate(state_abb = statetoabb(state_name), state_puma = get_state_puma(state_abb, puma_code))
  
  found_missing_msa_state_pumas <- bind_rows(
    puma_crosswalk %>% filter_at(vars(matches('state_puma')), any_vars(. %in% state_pumas_missing_msas)) %>% left_join(., mutate(puma_msa_ref_0, state_puma00=state_puma)) %>% filter(!is.na(msa_code)),
    puma_crosswalk %>% filter_at(vars(matches('state_puma')), any_vars(. %in% state_pumas_missing_msas)) %>% left_join(., mutate(puma_msa_ref_0, state_puma10=state_puma)) %>% filter(!is.na(msa_code))
  ) %>% distinct() %>%
    select(matches("state_puma|msa_code")) %>%
    gather(., "twastwas", "state_puma", matches("state_puma")) %>% select(-one_of('twastwas')) %>% distinct() %>%
    filter(state_puma %in% state_pumas_missing_msas  ) %>%
    arrange(state_puma) %>% 
    mutate(
      puma_code =  substr(state_puma, 4, 20),
      state_abb = substr(state_puma, 1, 2),
      state_name = abbtostate(state_abb),
      state_fips_code = recode_state(state_name, to_fips=T))
  
  still_missing <- c("AZ-00300", "CA-00200", "CA-01100", "CA-02300", "CA-03300", "CA-05700", "FL-12100", "HI-00200", "IA-00100", "IL-00104", "IL-00300", "IL-00600", "IL-00700", "IL-02400", "KS-01000", "KY-00100", "KY-00700", "KY-00800", "LA-77777", "MI-00100", "MI-00200", "MI-00300", "MI-00400", "MI-00500", "MI-01000", "MI-02500", "MS-00800", "NC-05100", "NH-00100", "NH-00500", "NM-01000", "NM-01200", "NY-00100", "NY-02600", "OH-00800", "OK-00701", "OK-00702", "OR-00100", "PA-01500", "PA-03800", "SD-00300", "VA-03500", "VA-51097", "VT-00300", "WA-01600", "WA-10800", "WA-11300", "WA-11900", "WI-01700", "WI-50000")
  
  
  puma_msa_ref <-  bind_rows(puma_msa_ref_0, found_missing_msa_state_pumas) %>%
    bind_rows(., data.frame(stringsAsFactors = FALSE,
                            state_puma = c("AZ-00300","CA-00200",
                                           "CA-01100","CA-02300","CA-03300","CA-05700","FL-12100",
                                           "HI-00200","IA-00100","IL-00104","IL-00300","IL-00600",
                                           "IL-00700","IL-02400","KS-01000","KY-00100","KY-00700",
                                           "KY-00800","LA-77777","MI-00100","MI-00200",
                                           "MI-00300","MI-00400","MI-00500","MI-01000","MI-02500",
                                           "MS-00800","NC-05100","NH-00100","NH-00500","NM-01000",
                                           "NM-01200","NY-00100","NY-02600","OH-00800","OK-00701",
                                           "OK-00702","OR-00100","PA-01500","PA-03800",
                                           "SD-00300","VA-03500","VA-51097","VT-00300","WA-01600",
                                           "WA-10800","WA-11300","WA-11900","WI-01700","WI-50000"),
                            msa_code = c("38060, 43320","21700",
                                         "39780","21700","46380","46020","29380","25900","43580",
                                         "44580","39500","16660","16460, 20820, 34500","36860",
                                         "26740","37140, 34660, 32460","43700","30940, 40080",
                                         "35380","26340, 27020","21540, 31940, 42300","10980",
                                         "15620, 45900","45900","10940","25880",
                                         "17380, 26940, 24740","22180, 31300, 29900","17200","17200, 28300",
                                         "17580, 38780","16100, 26020","36300","27460",
                                         "11780","10220","10220, 20460","25840",
                                         "32740, 47620, 36340","43740, 27780","10100, 47980","19260, 32300",
                                         "19260, 32300","17200","10140, 38820, 43220","21260",
                                         "10140, 43220","38820","48020, 48580","48580")
    )) %>%
    mutate(state_abb = ifelse(!is.na(state_abb), state_abb, substr(state_puma, 1, 2)),
           puma_code = ifelse(!is.na(puma_code), puma_code, substr(state_puma, 4, 20)),
           state_fips_code = ifelse(!is.na(state_fips_code), state_fips_code, recode_state(state_abb, to_fips=T)),
           state_name = ifelse(!is.na(state_name), state_name, tolower(abbtostate(state_abb)))
    ) %>%
    separate(msa_code, into=paste0('msa_code_', 1:100), sep=", ") %>% select_if(not_all_na) %>% gather(., 'twastwas', 'msa_code', matches('^msa_code_\\d')) %>% select(-one_of('twastwas')) %>% distinct() %>%
    separate(puma_code, into=paste0('puma_code_', 1:100), sep=", ") %>% select_if(not_all_na) %>% gather(., 'twastwas', 'puma_code', matches('^puma_code_\\d')) %>% select(-one_of('twastwas')) %>% distinct() %>%
    distinct()
  
  
  
  cbsa_to_csa <- "https://public.opendatasoft.com/explore/dataset/core-based-statistical-areas-cbsas-and-combined-statistical-areas-csas/export/"
  # "https://public.opendatasoft.com/explore/dataset/core-based-statistical-areas-cbsas-and-combined-statistical-areas-csas/download/?format=xls&timezone=America/New_York&lang=en&use_labels_for_header=true" %>% rio::import()
  msa_to_county <- c(
    "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2009/historical-delineation-files/list1.txt",
    "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2003/historical-delineation-files/03mfips.txt")
  geographic_delineation_files <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/"
  cbsa_to_county_crosswalk <- read_csv("https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv")
  cbsa_category_with_county_fips <- "https://www.uspto.gov/web/offices/ac/ido/oeip/taf/cls_cbsa/cbsa_countyassoc.htm"
  msa_2007_2011 <- read_table("https://www2.census.gov/programs-surveys/susb/technical-documentation/msa_codes_2007_to_2011.txt", skip=3) %>% janitor::clean_names() %>% separate(names(.), into=paste0('x', 1:10), sep="  ") %>% select_if(not_all_na) %>% mutate_all(trimws_)
  msa_2017_2021 <- read_table("https://www2.census.gov/programs-surveys/susb/technical-documentation/msa_codes_2017_to_2021.txt", skip=3) %>% janitor::clean_names()
  naics_msa_etc_code_files <- "https://www2.census.gov/programs-surveys/susb/technical-documentation/"
  micro_metro_statistical_areas <- rio::import("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list1_2020.xls", skip=2) %>% janitor::clean_names() %>% as_tibble() %>%
    mutate(msa_description = paste(cbsa_title, metropolitan_micropolitan_statistical_area) %>% gsub("Micropolitan Statistical Area", "MicroSA", .) %>% gsub("Metropolitan Statistical Area", "MSA", .), metropolitan_micropolitan_statistical_area=NULL,
           stab = statetoabb(state_name),
           state_msa = get_state_puma(stab, cbsa_code)) %>%
    group_by(cbsa_code, msa_description) %>%
    summarize_all(., function(v) paste0(sort(unique(v)), collapse='; ') %>% recode_na('')) %>% ungroup() %>%
    distinct() %>%
    mutate(msa_description = paste0(msa_description, " - ", gsub(" County", "", county_county_equivalent)))
  micro_metro_statistical_areas %>% sumry(10)
  
  
  bind_rows(
    read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv") %>% filter(!is.na(msa_description)),
    
    read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv") %>% filter(is.na(msa_description)) %>%
      mutate(msa_description=NULL) %>%
      left_join(., select(micro_metro_statistical_areas, state_msa, msa_description)) %>%
      print(n=nrow(.))
  ) %>%
    arrange(state_msa, state_puma) %>%
    writexl_open()
  
  
  micro_metro_statistical_areas %>% filter_all(any_vars(grepl("27500|36980|28700|34980|37140", .)))
  micro_metro_statistical_areas %>% filter_all(any_vars(grepl(paste_regex(leftovers), .))) %>% select(matches('msa|csa')) %>% print(n=nrow(.)) %>% writexl_open()
  
  geocorr::county2010_to_puma2000 %>%
    select(-matches("afact|intp|pop")) %>% distinct() %>%
    group_by(PUMA2kName) %>%
    summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup() %>%
    mutate(state_puma = get_state_puma(stab, puma2k)) %>%
    writexl_open()
  
  geocorr::puma2000_to_puma2012
  puma_msa_ref <- read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv")
  puma_msa_ref %>% filter(is.na(msa_description))
  
  puma_msa_ref_0 <-  rio::import(file="https://usa.ipums.org/usa/resources/volii/MSA2013_PUMA2010_crosswalk.xls", which=1) %>%
    mutate_if(is.character, function(v){
      stringi::stri_trans_general(str=v, id="Latin-ASCII") # great!
    })  %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('population')) %>% mutate_at(vars(-matches('msa_title|puma_name')), tolower) %>% 
    mutate(state_abb = statetoabb(state_name), state_puma = get_state_puma(state_abb, puma_code), state_msa = get_state_puma(state_abb, msa_code), msa_title = paste0(msa_title, " MSA"))
  
  bind_rows(
    puma_msa_ref %>% filter(!state_msa %in% puma_msa_ref_0$state_msa) ,
    
    puma_msa_ref %>% filter(state_msa %in% puma_msa_ref_0$state_msa) %>%
      mutate(msa_description = NULL) %>%
      left_join(., distinct(select(puma_msa_ref_0, msa_description=msa_title, state_msa))) %>% distinct() %>%
      # mutate(msa_description = paste0(msa_description, " MSA")) %>%
      # arrange(desc(nchar(msa_description))) %>%
      # select(matches('msa')) %>% distinct() %>%
      print(n=nrow(.))
  ) %>% 
    arrange(state_msa, state_puma) %>%
    writexl_open()
  
  puma_msa_ref <- read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv")
  puma_msa_ref %>% filter(is.na(msa_description)) %>% select(-matches('msa_desc')) %>%
    left_join(., mutate(puma_msa_ref_0, msa_description=msa_title, state_msa = get_state_puma(state_abb, msa_code), state_puma=NULL, msa_description=msa_title)) %>%
    bind_rows(puma_msa_ref %>% filter(!is.na(msa_description)), .) %>%
    arrange(state_puma, state_msa) %>%
    select(1:5) %>% distinct() %>%
    group_by(state_puma) %>% fill(., puma_description, .direction="updown") %>% ungroup() %>% 
    group_by(state_msa) %>% fill(., msa_description, .direction="updown") %>% ungroup() %>% 
    distinct() %>%
    writexl_open()
  
  puma_msa_ref <- read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv") %>% arrange(state_puma, state_msa) %>% writexl_open()
  
  more_puma_cbsa_ref <- EEOALL1R_2 %>% select(matches('geoname|state_msa')) %>% distinct() %>% filter(!is.na(geoname)) %>%
    mutate_if(is.character, function(v){
      stringi::stri_trans_general(str=v, id="Latin-ASCII") # great!
    }) %>% print(n=nrow(.)) %>%
    mutate(city = gsub(', .*',"", geoname) %>% trimws_(),
           state = substr(state_msa, 1, 2))
  
  more_puma_cbsa_ref %>% filter(state_msa %in% c("WA-21260"))
  
  puma_msa_ref %>% filter(is.na(msa_description)) %>% select(-matches('msa_desc')) %>%
    left_join(., select(more_puma_cbsa_ref, state_msa, msa_description=geoname)) %>%
    bind_rows(puma_msa_ref %>% filter(!is.na(msa_description)), .) %>%
    mutate_if(is.character, function(v){
      stringi::stri_trans_general(str=v, id="Latin-ASCII") # great!
    }) %>%
    arrange(state_puma, state_msa) %>%
    select(1:5) %>% distinct() %>%
    group_by(state_puma) %>% fill(., puma_description, .direction="updown") %>% ungroup() %>% 
    group_by(state_msa) %>% fill(., msa_description, .direction="updown") %>% ungroup() %>% 
    distinct() %>%
    writexl_open()
  
  puma_msa_ref <- read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv")
  puma_dictionary <- tbl(con, 'puma_dictionary') %>% as_tibble() %>%
    mutate(puma_description = gsub('.*\\d: ', '', code_description))
  puma_msa_ref %>% select(-matches('puma_desc')) %>%
    left_join(., mutate(puma_dictionary, state=NULL)) %>%
    select(state, puma_description, msa_description, state_puma, state_msa) %>%
    mutate_if(is.character, function(v){
      stringi::stri_trans_general(str=v, id="Latin-ASCII") # great!
    }) %>%
    distinct() %>%
    arrange(state_puma, state_msa) %>%
    writexl_open()
  # filter(nchar(puma_description)<19)
  
  
  puma_msa_ref %>% filter(is.na(puma_description)) %>% select(-matches('msa_desc')) %>%
    left_join(., mutate(puma_msa_ref_0, msa_description=msa_title, state_msa = get_state_puma(state_abb, msa_code), state_puma=NULL, msa_description=msa_title)) %>%
    bind_rows(puma_msa_ref %>% filter(!is.na(msa_description)), .) %>%
    arrange(state_puma, state_msa) %>%
    select(1:5) %>% distinct() %>%
    group_by(state_puma) %>% fill(., puma_description, .direction="updown") %>% ungroup() %>% 
    group_by(state_msa) %>% fill(., msa_description, .direction="updown") %>% ungroup() %>% 
    distinct() %>%
    writexl_open()
  
  
  puma_crosswalk <- rio::import(file="https://usa.ipums.org/usa/resources/volii/PUMA2000_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('pop|land|gisjoin|cpuma00|geoid')) %>% 
    mutate(stab00 = recode_state(state00), stab10 = recode_state(state10),
           state_puma00 = get_state_puma(stab00, puma00), state_puma10 = get_state_puma(stab10, puma10)) #%>% 
  
  found_missing_msa_state_pumas <- bind_rows(
    puma_crosswalk %>% filter_at(vars(matches('state_puma')), any_vars(. %in% state_pumas_missing_msas)) %>% left_join(., mutate(puma_msa_ref_0, state_puma00=state_puma)) %>% filter(!is.na(msa_code)),
    puma_crosswalk %>% filter_at(vars(matches('state_puma')), any_vars(. %in% state_pumas_missing_msas)) %>% left_join(., mutate(puma_msa_ref_0, state_puma10=state_puma)) %>% filter(!is.na(msa_code))
  ) %>% distinct() %>%
    select(matches("state_puma|msa_code")) %>%
    gather(., "twastwas", "state_puma", matches("state_puma")) %>% select(-one_of('twastwas')) %>% distinct() %>%
    filter(state_puma %in% state_pumas_missing_msas  ) %>%
    arrange(state_puma) %>% 
    mutate(
      puma_code =  substr(state_puma, 4, 20),
      state_abb = substr(state_puma, 1, 2),
      state_name = abbtostate(state_abb),
      state_fips_code = recode_state(state_name, to_fips=T))
  
  still_missing <- c("AZ-00300", "CA-00200", "CA-01100", "CA-02300", "CA-03300", "CA-05700", "FL-12100", "HI-00200", "IA-00100", "IL-00104", "IL-00300", "IL-00600", "IL-00700", "IL-02400", "KS-01000", "KY-00100", "KY-00700", "KY-00800", "LA-77777", "MI-00100", "MI-00200", "MI-00300", "MI-00400", "MI-00500", "MI-01000", "MI-02500", "MS-00800", "NC-05100", "NH-00100", "NH-00500", "NM-01000", "NM-01200", "NY-00100", "NY-02600", "OH-00800", "OK-00701", "OK-00702", "OR-00100", "PA-01500", "PA-03800", "SD-00300", "VA-03500", "VA-51097", "VT-00300", "WA-01600", "WA-10800", "WA-11300", "WA-11900", "WI-01700", "WI-50000")
  
  
  puma_msa_ref <-  bind_rows(puma_msa_ref_0, found_missing_msa_state_pumas) %>%
    bind_rows(., data.frame(stringsAsFactors = FALSE,
                            state_puma = c("AZ-00300","CA-00200",
                                           "CA-01100","CA-02300","CA-03300","CA-05700","FL-12100",
                                           "HI-00200","IA-00100","IL-00104","IL-00300","IL-00600",
                                           "IL-00700","IL-02400","KS-01000","KY-00100","KY-00700",
                                           "KY-00800","LA-77777","MI-00100","MI-00200",
                                           "MI-00300","MI-00400","MI-00500","MI-01000","MI-02500",
                                           "MS-00800","NC-05100","NH-00100","NH-00500","NM-01000",
                                           "NM-01200","NY-00100","NY-02600","OH-00800","OK-00701",
                                           "OK-00702","OR-00100","PA-01500","PA-03800",
                                           "SD-00300","VA-03500","VA-51097","VT-00300","WA-01600",
                                           "WA-10800","WA-11300","WA-11900","WI-01700","WI-50000"),
                            msa_code = c("38060, 43320","21700",
                                         "39780","21700","46380","46020","29380","25900","43580",
                                         "44580","39500","16660","16460, 20820, 34500","36860",
                                         "26740","37140, 34660, 32460","43700","30940, 40080",
                                         "35380","26340, 27020","21540, 31940, 42300","10980",
                                         "15620, 45900","45900","10940","25880",
                                         "17380, 26940, 24740","22180, 31300, 29900","17200","17200, 28300",
                                         "17580, 38780","16100, 26020","36300","27460",
                                         "11780","10220","10220, 20460","25840",
                                         "32740, 47620, 36340","43740, 27780","10100, 47980","19260, 32300",
                                         "19260, 32300","17200","10140, 38820, 43220","21260",
                                         "10140, 43220","38820","48020, 48580","48580")
    )) %>%
    mutate(state_abb = ifelse(!is.na(state_abb), state_abb, substr(state_puma, 1, 2)),
           puma_code = ifelse(!is.na(puma_code), puma_code, substr(state_puma, 4, 20)),
           state_fips_code = ifelse(!is.na(state_fips_code), state_fips_code, recode_state(state_abb, to_fips=T)),
           state_name = ifelse(!is.na(state_name), state_name, tolower(abbtostate(state_abb)))
    ) %>%
    mutate_at(vars(matches('^state(_abb|name|$)')), function(v) tolower(v)) %>%
    separate(msa_code, into=paste0('msa_code_', 1:100), sep=", ") %>% select_if(not_all_na) %>% gather(., 'twastwas', 'msa_code', matches('^msa_code_\\d')) %>% select(-one_of('twastwas')) %>% distinct() %>% drop_na(msa_code) %>%
    separate(state_puma, into=paste0('state_puma_', 1:100), sep=", ") %>% select_if(not_all_na) %>% gather(., 'twastwas', 'state_puma', matches('^state_puma_\\d')) %>% select(-one_of('twastwas')) %>% distinct() %>%
    mutate(state_msa = get_state_puma(state_abb, msa_code)) %>%
    distinct() %>%
    transmute(puma_code=NULL, msa_code=NULL, state=state_abb, state_name=NULL, state_fips_code=NULL, puma_description=puma_name, msa_description=msa_title, state_msa=state_msa, state_puma=state_puma) %>%
    mutate_at(vars(matches("description")), function(v) {
      v %>%
        gsub('--', '-', .) %>% gsub(' & ', '+', .) %>% gsub(', ', '/', .) %>% gsub('\\.', '', .) %>% 
        gsub('Southwest', 'SW', .) %>% gsub('Southeast', 'SE', .) %>% gsub('North(\\)|\\+| )', 'N\\1', .) %>% gsub('South(\\)|\\+| )', 'S\\1', .) %>% gsub('Northeast', 'NE', .) %>% gsub('Northwest', 'NW', .) %>% gsub('East(\\)|\\+| )', 'E\\1', .) %>% gsub('West(\\)|\\+| )', 'W\\1', .) %>% #gsub('Counties|County', 'Cnt', .) %>% gsub('Cities|City', 'Cit', .) %>% 
        gsub(' (,|;)', '\\1', .) %>%
        gsub(', &', ' &', .)
    }) %>%
    arrange(state_puma) %>% distinct()
  write.csv(puma_msa_ref, "puma_msa_dictionary.csv")
  
  
  
  
  read.table_fromClipboard("state_puma	cbsa (manual)
AZ-00300	38060, 43320
CA-00200	21700
CA-01100	39780
CA-02300	21700
CA-03300	46380
CA-05700	46020
FL-12100	29380
HI-00200	25900
IA-00100	43580
IL-00104	44580
IL-00300	39500
IL-00600	16660
IL-00700	16460, 20820, 34500
IL-02400	36860
KS-01000	26740
KY-00100	37140, 34660, 32460
KY-00700	43700
KY-00800	30940, 40080
LA-77777	35380
MI-00100	26340, 27020
MI-00200	21540, 31940, 42300
MI-00300	10980
MI-00400	15620, 45900
MI-00500	45900
MI-01000	10940
MI-02500	25880
MS-00800	17380, 26940, 24740
NC-05100	22180, 31300, 29900
NH-00100	17200
NH-00500	17200, 28300
NM-01000	17580, 38780
NM-01200	16100, 26020
NY-00100	36300
NY-02600	27460
OH-00800	11780
OK-00701	10220
OK-00702	10220, 20460
OR-00100	25840
PA-01500	32740, 47620, 36340
PA-03800	43740, 27780
SD-00300	10100, 47980
VA-03500	19260, 32300
VA-51097	19260, 32300
VT-00300	17200
WA-01600	10140, 38820, 43220
WA-10800	21260
WA-11300	10140, 43220
WA-11900	38820
WI-01700	48020, 48580
WI-50000	48580
  ") %>% janitor::clean_names() %>%
    setNames(c("state_puma", 'msa'))
  # puma_dictionary <- tbl(con, "puma_dictionary") %>% select(matches("state|state_puma")) %>% as_tibble() 
  # puma_msa_ref <- rio::import(file="https://usa.ipums.org/usa/resources/volii/MSA2013_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('population')) %>% mutate_all(tolower) %>% 
  #   mutate(state_abb = statetoabb(state_name), state_puma = get_state_puma(state_abb, puma_code))
  # 
  still_missings <- tbl(con, "puma_dictionary") %>% filter(state_puma %in% still_missing) %>% as_tibble() %>% print(n=nrow(.))# %>% .$state_puma %>% edit()
  manual_puma_msa_list <- list(
    "AZ-00300", "CA-00200", "CA-01100", "CA-02300", "CA-03300", 
    "CA-05700", "FL-12100", "HI-00200", "IA-00100", "IL-00104", "IL-00300", 
    "IL-00600", "IL-00700", "IL-02400", "KS-01000", "KY-00100", "KY-00700", 
    "KY-00800", "LA-77777", "MI-00100", "MI-00200", "MI-00300", "MI-00400", 
    "MI-00500", "MI-01000", "MI-02500", "MS-00800", "NC-05100", "NH-00100", 
    "NH-00500", "NM-01000", "NM-01200", "NY-00100", "NY-02600", "OH-00800", 
    "OK-00701", "OK-00702", "OR-00100", "PA-01500", "PA-03800", "SD-00300", 
    "VA-03500", "VA-51097", "VT-00300", "WA-01600", "WA-10800", "WA-11300", 
    "WA-11900", "WI-01700", "WI-50000")
  
  c("AZ-00300", "CA-00200", "CA-01100", "CA-02300", "CA-03300", 
    "CA-05700", "FL-12100", "HI-00200", "IA-00100", "IL-00104", "IL-00300", 
    "IL-00600", "IL-00700", "IL-02400", "KS-01000", "KY-00100", "KY-00700", 
    "KY-00800", "LA-77777", "MI-00100", "MI-00200", "MI-00300", "MI-00400", 
    "MI-00500", "MI-01000", "MI-02500", "MS-00800", "NC-05100", "NH-00100", 
    "NH-00500", "NM-01000", "NM-01200", "NY-00100", "NY-02600", "OH-00800", 
    "OK-00701", "OK-00702", "OR-00100", "PA-01500", "PA-03800", "SD-00300", 
    "VA-03500", "VA-51097", "VT-00300", "WA-01600", "WA-10800", "WA-11300", 
    "WA-11900", "WI-01700", "WI-50000")
  
  "https://www.uspto.gov/web/offices/ac/ido/oeip/taf/cls_cbsa/cbsa_countyassoc.htm"
  "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2015/delineation-files/list1.xls"
  # poo <- tigris::combined_statistical_areas()
  cbsas_df <- rio::import("https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2015/delineation-files/list1.xls", skip=2) %>% janitor::clean_names() %>% as_tibble() %>%
    mutate(state = statetoabb(state_name),
           description = paste0(cbsa_title, ", ", metropolitan_division_title, ", ", csa_title,", ",county_county_equivalent) %>% gsub("NA, |, NA", "", .)) #%>%
  # mutate(description = paste0(cbsa_title, ", ", metropolitan_micropolitan_statistical_area, ", ",metropolitan_division_title, ", ", csa_title,", ",county_county_equivalent, ", ", state_name)) %>% writexl_open()
  cbsas_df %>% mutate(description = paste0(cbsa_title, ", ", metropolitan_micropolitan_statistical_area, ", ",metropolitan_division_title, ", ", csa_title,", ",county_county_equivalent, ", ", state_name)) %>% 
    # filter_if(is.factorchar, any_vars(grepl('humbol', ., ignore.case=T)))
    filter(grepl('(hamilton|suwan|madison|taylor|lafay).*florida', description, ignore.case=T))
  
  cbsas_df %>% mutate(description = paste0(cbsa_title, ", ", metropolitan_micropolitan_statistical_area, ", ",metropolitan_division_title, ", ", csa_title,", ",county_county_equivalent, ", ", state_name)) %>% 
    filter(agrep("IA-00100: Sioux/Clay/Dickinson/O'Brien/Lyon/Emmet/Palo Alto+Osceola Counties 2010-2019", description, ignore.case=T))
  
  LOCATIONS <- cbsas_df %>% mutate(description = paste0(cbsa_title, ", ", metropolitan_micropolitan_statistical_area, ", ",metropolitan_division_title, ", ", csa_title,", ",county_county_equivalent, ", ", state_name)) %>% .$description
  agrep("IA-00100: Sioux/Clay/Dickinson/O'Brien/Lyon/Emmet/Palo Alto+Osceola Counties 2010-2019", LOCATIONS, max.distance = .75, value=T)
  
  results_list <- list()
  for(i in 1:nrow(still_missings)){ # {i<-1}
    row_to_search <- still_missings[i, ] %>% mutate(description = gsub('20\\d{2}.*|county|counties', '', description, ignore.case=T) %>% trimws_()) %>% 
      mutate_if(is.factorchar, function(v) replace_na(v, "o") %>% gsub("^$", "o", .))
    cbsas_df_state <- cbsas_df %>% filter(tolower(state)==tolower(row_to_search$state)) %>% select(matches("_code|DFVAR_TO|simil"), everything()) %>% 
      mutate(description = gsub("counties|county", "", description, ignore.case=T) %>% trimws_()) %>% 
      mutate_if(is.factorchar, function(v) replace_na(v, "o") %>% gsub("^$", "o", .))
    result_indiv <- fuzzy_match_rank(s=row_to_search$description, df=cbsas_df_state, dfvar="description") %>% mutate(input = row_to_search$description) %>% select(matches("_code|DFVAR_TO|simil|input"))
    results_list[[row_to_search$description]] = result_indiv
  }
  results_list %>% lapply(., function(d) select(d, -matches("input")))
  # fuzzy_match_rank(s="Sioux/Clay/Dickinson/O'Brien/Lyon/Emmet/Palo Alto+Osceola Counties", df=cbsas_df, dfvar="county_county_equivalent")
  results_list$`Adams, Pike, Brown, Schuyler & Mason` %>% print(n=nrow(.)) %>% filter(grepl("adams|pike|brown|schuyler|mason", DFVAR_TO_COMPARE, ignore.case=T))
  results_list$`Clark, Jasper, Crawford, Lawrence, Richland, Clay & Wayne` %>% print(n=nrow(.)) %>% filter(grepl("Clark|jasper|craw|lawr|rich|clay|wayne", DFVAR_TO_COMPARE, ignore.case=T))
  results_list$`Central Kansas--Hutchinson City` %>% print(n=nrow(.)) %>% filter(grepl("hutch", DFVAR_TO_COMPARE, ignore.case=T))
  
  results_list_ecolab <- list()
  for(i in 1:nrow(missingmsa)){ # {i<-1}
    row_to_search <- missingmsa[i, ] %>% mutate(state=state_abb, description = gsub('20\\d{2}.*|county|counties', '', city, ignore.case=T) %>% trimws_()) %>% 
      mutate_if(is.factorchar, function(v) replace_na(v, "o") %>% gsub("^$", "o", .))
    cbsas_df_state <- cbsas_df %>% filter(tolower(state)==tolower(row_to_search$state)) %>% select(matches("_code|DFVAR_TO|simil"), everything()) %>% 
      mutate(description = gsub("counties|county", "", description, ignore.case=T) %>% trimws_()) %>% 
      mutate_if(is.factorchar, function(v) replace_na(v, "o") %>% gsub("^$", "o", .))
    result_indiv <- fuzzy_match_rank(s=row_to_search$description, df=cbsas_df_state, dfvar="description") %>% mutate(input = row_to_search$description) %>% select(matches("_code|DFVAR_TO|simil|input"))
    results_list_ecolab[[row_to_search$description]] = result_indiv
  }
  results_list_ecolab %>% lapply(., function(d) select(d, -matches("input")))
  
  
  puma_msa_ref_idk <- puma_msa_ref %>%
    # full_join(., transmute(glptools::MSA_PUMA, state_fips_code=STATEFIP, msa_code=MSA, puma_code=pad_leading_0s(PUMA, length=5), year=NULL)) %>% distinct() %>%
    bind_rows(., transmute(glptools::MSA_PUMA, state_fips_code=STATEFIP, msa_code=MSA, puma_code=pad_leading_0s(PUMA, length=5), year=NULL, state_abb=recode_state(STATEFIP), state_puma=get_state_puma(state_abb, puma_code))) %>%
    arrange(desc(nchar(puma_name))) %>% filter(!duplicated(state_puma))
  # glptools::MSA_zip
  puma_msa_ref_idk %>% filter(state_puma %in% still_missing)
  # remotes::install_github("greaterlouisvilleproject/glptools")
  # # glptools::MSA_PUMA
  # # puma_msa_ref %>% filter(grepl("MS.*0", state_puma))
  # # puma_dictionary <- tbl(con, "puma_dictionary") %>% select(matches("state|state_puma")) %>% as_tibble() 
  setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma) %>% length()
  setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma)# %>% datapasta::vector_paste()
  setdiff(
    puma_dictionary$state_puma,
    bind_rows(glptools::MSA2012_PUMA, glptools::MSA_PUMA) %>% transmute(., state_fips_code=STATEFIP, msa_code=MSA, puma_code=pad_leading_0s(PUMA, length=5), year=NULL, state_abb=recode_state(STATEFIP), state_puma=get_state_puma(state_abb, puma_code)) %>% .$state_puma %>% unique()
  ) %>% length()
  # rio::import(file="https://usa.ipums.org/usa/resources/volii/MSA2013_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('population')) %>% mutate_all(tolower) %>% mutate(stab = recode_state(state_fips_code)) %>% mutate()
  
  puma_msa_ref <- rio::import(file="https://usa.ipums.org/usa/resources/volii/MSA2013_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('population')) %>% mutate_all(tolower) %>% 
    mutate(state_abb = statetoabb(state_name), state_puma = get_state_puma(state_abb, puma_code)) 
  # glptools::MSA2012_PUMA
  puma_crosswalk <- rio::import(file="https://usa.ipums.org/usa/resources/volii/PUMA2000_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('pop|land|gisjoin|cpuma00|geoid')) %>% 
    mutate(stab00 = recode_state(state00), stab10 = recode_state(state10),
           state_puma00 = get_state_puma(stab00, puma00), state_puma10 = get_state_puma(stab10, puma10)) #%>% 
  # filter(stab00 != stab10) %>% print(n=nrow(.))
  # filter_at(vars(matches('state_puma')), any_vars(grepl("MS.*006", .)))
  found_missing_msa_state_pumas <- bind_rows(
    puma_crosswalk %>% filter_at(vars(matches('state_puma')), any_vars(. %in% setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma))) %>% left_join(., mutate(puma_msa_ref, state_puma00=state_puma)) %>% filter(!is.na(msa_code)),
    puma_crosswalk %>% filter_at(vars(matches('state_puma')), any_vars(. %in% setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma))) %>% left_join(., mutate(puma_msa_ref, state_puma10=state_puma)) %>% filter(!is.na(msa_code))
  ) %>% distinct() %>%
    select(matches("state_puma|msa_code")) %>%
    gather(., "twastwas", "state_puma", matches("state_puma")) %>% select(-one_of('twastwas')) %>% distinct() %>%
    filter(state_puma %in% setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma)  ) %>%
    arrange(state_puma) %>% 
    mutate(
      puma_code =  substr(state_puma, 4, 20),
      state_abb = substr(state_puma, 1, 2),
      state_name = abbtostate(state_abb),
      state_fips_code = recode_state(state_name, to_fips=T))
  
  found_missing_msa_state_pumas %>%
    group_by(state_puma) %>%
    summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup()
  # filter_all(any_vars(. %in%  setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma) ))
  # puma_crosswalk %>% filter(state_puma10 %in% setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma)) %>% left_join(., mutate(puma_msa_ref, state_puma00=state_puma)) %>% filter(!is.na(msa_code))
  # puma_crosswalk %>% filter(state_puma00 %in% setdiff(puma_dictionary$state_puma, puma_msa_ref$state_puma)) %>% left_join(., mutate(puma_msa_ref, state_puma10=state_puma)) %>% filter(!is.na(msa_code))
  # found_missing_msa_state_pumas$state_puma %>% datapasta::vector_paste()
  
  puma_msa_ref <-  bind_rows(puma_msa_ref, found_missing_msa_state_pumas)
  
}


