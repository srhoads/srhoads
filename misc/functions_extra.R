library(srhoads); pkg('tidyverse', 'magrittr')
if(!"geocorr" %in% installed.packages()){devtools::install_github("jjchern/geocorr")}
if(!"usa" %in% installed.packages()){install.packages("usa")}
if(!"rio" %in% installed.packages()){install.packages("rio")}
# if(!"tigris" %in% installed.packages()){install.packages("tigris")}
# if(!"zipcodeR" %in% installed.packages()){install.packages("zipcodeR")}
if(!"zipcodeR" %in% installed.packages()){devtools::install_github("gavinrozzi/zipcodeR")}
# try({source("~/srhoads/misc/functions_extra.R")}, silent=T)
# con <- RPostgres::dbConnect(RPostgres::Postgres(), user = 'diversityplanner@diversity-planning-data',password = 'jacksonlewisdatascience1!',dbname = 'postgres',host = 'diversity-planning-data.postgres.database.azure.com',port = 5432,sslmode = 'require')
#===========================DATA==================================


if(FUNCTIONS_DISPARATEIMPACT<-T){
  bitest <- function(workforce,protectedclass,availability, pvalue=NA, output=c("all", "pvalue", "zscore")[1]) {
    result <- tryCatch({
      if(is.nanull(pvalue)){
        res <- binom.test(protectedclass,workforce,availability)
        pvalue <- res$p.value
      } else {
        res <- list()
        res$p.value <- pvalue
      }
      zscore <- (sign(protectedclass-availability*workforce)*qnorm(pvalue/2))*-1
      zscore
      if(output=="all"){
        res$zscore <- zscore
        res$formatted <- paste0("Z=", round(zscore, 3), "; ", "p=", round(as.numeric(res$p.value),3)) %>% ifelse(res$p.value<.05, paste0(., "*"), .)
        return(res)
      } else if(output=="pvalue"){
        return(pvalue)
      } else if(output=="zscore"){
        return(zscore)
      }
    }, error=function(e){NA})
    result
  }
  
  chisq=function(sel1,sel2,pool1,pool2){
    zscore <- ((sel1)/(pool1) - (sel2)/(pool2))/sqrt(((sel1+sel2)/(pool1+pool2))*(1-((sel1+sel2)/(pool1+pool2)))*((1/pool1)+(1/pool2)))
    sig <- if(abs(zscore)>1.95996398454005){T}else{F}
    obs <- data.frame(notsel=c(pool1-sel1, pool2-sel2), sel=c(sel1, sel2))
    (csqt <- chisq.test(obs))
    list(zscore=zscore, sig=sig, p.value=csqt$p.value)
  } #Chi-Square Function
  
  fourfifths_ir <- function(sel_l, sel_r, total_l, total_r, calculate_ir_adjusted=F, output=c("all")){
    # rowdata <- {x_dia_df %>% filter(.[[1]]==analysis, decisional_unit==row$decisional_unit)}
    # left <- rowdata[[2]]
    # rght <- rowdata[[3]]
    # sely_l <- left %>% gsub("\\/.*", "", .) %>% readr::parse_number()
    # sely_r <- rght %>% gsub("\\/.*", "", .) %>% readr::parse_number()
    # total_l <- left %>% gsub(".*\\/", "", .) %>% gsub("\\\n.*", "", .) %>% readr::parse_number()
    # total_r <- rght %>% gsub(".*\\/", "", .) %>% gsub("\\\n.*", "", .) %>% readr::parse_number()
    # sr_l <- rowdata[[2]] %>% gsub(".*\\\n|\\%", "", .) %>% readr::parse_number() %>% {./100}
    # sr_r <- rowdata[[3]] %>% gsub(".*\\\n|\\%", "", .) %>% readr::parse_number() %>% {./100}
    # ir_r <- sr_r/sr_l
    sr_l <- sel_l/total_l
    sr_r <- sel_r/total_r
    ir_l <- sr_l/sr_r # SiG <.80
    ir_r <- sr_r/sr_l # SiG <.80
    sig <- ifelse((ir_l<.8|ir_r<.8)&(ir_l+ir_r!=Inf), T, F)
    if(is.na(ir_l)|is.na(ir_r)|ir_l==Inf|ir_r==Inf){
      formatted <- NA
    } else {
      formatted <- paste0( paste0("Left: ", round(ir_l, 3)) %>% ifelse(ir_l<.80, paste0(., "*"), .), "; ", paste0("Right: ", round(ir_r, 3)) %>% ifelse(ir_r<.80, paste0(., "*"), .)) %>% trimws_()#paste0("Left: ", round(ir_l, 3) %>% ifelse(ir_l<.80, paste0("Flag ", .), .), "; Right:", round(ir_r, 3))
    }
    
    # if(calculate_ir_adjusted){
    ir_l_adj <- ((sel_l+1) / total_l) / ((sel_r-1) / total_r)
    ir_r_adj <- ((sel_r+1) / total_r) / ((sel_l-1) / total_l)
    sig_adj <- ifelse(ir_l_adj>1.0|ir_r_adj>1.0, T, F)
    # }
    # ir_adj <- ((sel_l + 1) /total_l) / ((sel_r + 1) /total_r) # SIG > 1.0
    list(ir_l=ir_l, ir_r=ir_r, sig=sig, ir_l_adj=ir_l_adj, ir_r_adj=ir_r_adj, sig_adj=sig_adj, formatted=formatted)
  }
  
  
  if(LOAD_RIF_DISPARATE_IMPACT_ANALYSIS_FUNCTIONS<-T){
    ##########################################################################################################################################################
    # The "RIFanalaysis" function Prepares a RIF Analysis By Decisional Unit on Specified Demographic Characteristics
    #It is set up to prepare analyses and report specific results based on agreed upon methodology
    #Results are sent to "RIF Results.csv" in your working directory.  When the function is complete open this file and run the corresponding Excel Macro
    require("eeptools")
    ##########################################################################################################################################################
    RIFanalysis <- function(RIFdata=NA, dec_unit=NA, demographics=NA, age_form="AGE",RIFdate=NA,overall="FALSE", positive.selections=NA){
      
      if(!is.data.frame(RIFdata)&length(RIFdata)==1){
        RIFdata <- read.table(RIFdata, header=TRUE,check.names=FALSE,sep=",",quote="\"") #Read data
      }
      if("AGE" %in% demographics){
        if(age_form=="AGE" && "AGE" %in% colnames(RIFdata)){RIFdata <- RIFdata}else{
          age <- data.frame(eeptools::age_calc(as.Date(RIFdata$DOB, "%m/%d/%Y"), enddate = as.Date(rep(RIFdate,nrow(RIFdata)),"%m/%d/%Y"), units = "years", precise = TRUE))
          colnames(age)[1] <- "AGE"
          RIFdata <- cbind(RIFdata,age)}} #Either use Age as is, or calculate age off of DOB based off of age_form entry
      
      
      split_RIF <- split(RIFdata, RIFdata[[dec_unit]]) #Creates a unique data set for each decisional unit 
      
      stat_sig_flags <- matrix(c("Decisional Unit","Analysis","Better Performing Group"),nrow=1) #Headers for Flag Summary
      write.table(as.data.frame(stat_sig_flags),file="RIF Summary.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
      
      overall_pool <- nrow(RIFdata)  #Pool of employees in workforce
      
      if("SUB-RACE" %in% demographics){
        #Overall Race Pools
        overall_white_pool <- sum(RIFdata$RACE==1, na.rm=TRUE); overall_black_pool <- sum(RIFdata$RACE==2, na.rm=TRUE); overall_hispanic_pool <- sum(RIFdata$RACE==3, na.rm=TRUE);
        overall_asian_pool <- sum(RIFdata$RACE==4, na.rm=TRUE); overall_natam_pool <- sum(RIFdata$RACE==5, na.rm=TRUE)
        overall_nathaw_pool <- sum(RIFdata$RACE==6, na.rm=TRUE); overall_twoplus_pool <- sum(RIFdata$RACE==7, na.rm=TRUE)
        o_pools=c(overall_white_pool,overall_black_pool,overall_hispanic_pool,overall_asian_pool,overall_natam_pool,overall_nathaw_pool,overall_twoplus_pool)
      }else{o_pools=c(0,0,0,0,0,0,0)}
      
      #{data=RIFdata;decisional_unit="Overall"}
      if(overall=="TRUE"){RIF_by_group(data=RIFdata, demographics=demographics, decisional_unit="Overall", o_pools=o_pools, overall_pool=overall_pool, positive.selections=positive.selections)}  #If "overall" is set to true, Run analysis on entire workforce
      
      #{i=1; data=split_RIF[[i]];decisional_unit=names(split_RIF)[i]}
      for(i in 1:length(split_RIF)){RIF_by_group(data=split_RIF[[i]], demographics=demographics, decisional_unit=names(split_RIF)[i], 
                                                 positive.selections=positive.selections, o_pools=o_pools, overall_pool=overall_pool)} #Analyses by Decisional Unit
      
      cat("RIF Analysis Complete","\n")
    }  #End RIF Analysis
    ################################################################################################################
    ################################################################################################################
    
    ################################################################################################################
    #The RIF_by_group function prepares all specified analyses within a decisional unit
    ################################################################################################################
    RIF_by_group=function(data=NA, demographics=NA, decisional_unit=NA, o_pools=NA, overall_pool=NA, positive.selections=NA){
      
      #Selections, Pools, and Rates for Gender
      if("GENDER" %in% demographics){
        female_pool <- sum(data$GENDER=="F", na.rm=TRUE);male_pool <- sum(data$GENDER=="M", na.rm=TRUE) 
        female_sel <- sum(data$GENDER=="F" & data$SELECTION=="Y",na.rm=TRUE); male_sel <- sum(data$GENDER=="M" & data$SELECTION=="Y",na.rm=TRUE)
        if(female_pool==0){female_rate <- 0}else{female_rate <- (female_sel)/(female_pool)}
        if(male_pool==0){male_rate <- 0}else{male_rate <- (male_sel)/(male_pool)}
      }
      
      #Selections, Pools, and Rates for Overall Race
      if("OVERALLRACE" %in% demographics){
        min_pool <- sum(data$OVERALLRACE=="MINORITY", na.rm=TRUE); nonminority_pool <- sum(data$OVERALLRACE=="NON-MINORITY", na.rm=TRUE)
        min_sel <- sum(data$OVERALLRACE=="MINORITY" & data$SELECTION=="Y",na.rm=TRUE); nonminority_sel <- sum(data$OVERALLRACE=="NON-MINORITY" & data$SELECTION=="Y",na.rm=TRUE) 
        if(nonminority_pool==0){nonminority_rate <- 0}else{nonminority_rate <- (nonminority_sel)/(nonminority_pool)}
        if(min_pool==0){min_rate <- 0}else{min_rate <- (min_sel)/(min_pool)}
      }
      
      #Selections, Pools, and Rates for Sub-Races
      if("SUB-RACE" %in% demographics){
        
        overall_white_pool <- o_pools[1]; overall_black_pool <- o_pools[2]; overall_hispanic_pool <- o_pools[3]; overall_asian_pool <- o_pools[4]
        overall_natam_pool <- o_pools[5]; overall_nathaw_pool <- o_pools[6]; overall_twoplus_pool <- o_pools[7]
        
        white_pool <- sum(data$RACE==1,na.rm=TRUE); black_pool <- sum(data$RACE==2, na.rm=TRUE); hispanic_pool <- sum(data$RACE==3, na.rm=TRUE); 
        asian_pool <- sum(data$RACE==4, na.rm=TRUE)
        natam_pool <- sum(data$RACE==5, na.rm=TRUE); nathaw_pool <- sum(data$RACE==6, na.rm=TRUE); twoplus_pool <- sum(data$RACE==7, na.rm=TRUE)
        white_sel <- sum(data$RACE==1 & data$SELECTION=="Y",na.rm=TRUE); 
        black_sel <- sum(data$RACE==2 & data$SELECTION=="Y",na.rm=TRUE); hispanic_sel <- sum(data$RACE==3 & data$SELECTION=="Y",na.rm=TRUE); asian_sel <- sum(data$RACE==4 & data$SELECTION=="Y",na.rm=TRUE)
        natam_sel <- sum(data$RACE==5 & data$SELECTION=="Y",na.rm=TRUE); nathaw_sel <- sum(data$RACE==6 & data$SELECTION=="Y",na.rm=TRUE); twoplus_sel <- sum(data$RACE==7 & data$SELECTION=="Y",na.rm=TRUE) 
        
        if(white_pool==0){white_rate <- 0}else{white_rate <- (white_sel)/(white_pool)}
        if(black_pool==0){black_rate <- 0}else{black_rate <- (black_sel)/(black_pool)}
        if(hispanic_pool==0){hispanic_rate <- 0}else{hispanic_rate <- (hispanic_sel)/(hispanic_pool)}
        if(asian_pool==0){asian_rate <- 0}else{asian_rate <- (asian_sel)/(asian_pool)}
        if(natam_pool==0){natam_rate <- 0}else{natam_rate <- (natam_sel)/(natam_pool)}
        if(nathaw_pool==0){nathaw_rate <- 0}else{nathaw_rate <- (nathaw_sel)/(nathaw_pool)}
        if(twoplus_pool==0){twoplus_rate <- 0}else{twoplus_rate <- (twoplus_sel)/(twoplus_pool)}
      }
      
      #Selections, Pools, and Rates for Age Groups
      if("AGE" %in% demographics){  
        plus40_pool <- sum(data$AGE>=40,na.rm = T); plus45_pool <- sum(data$AGE>=45,na.rm = T); plus50_pool <- sum(data$AGE>=50,na.rm = T) 
        plus55_pool <- sum(data$AGE>=55,na.rm = T); plus60_pool <- sum(data$AGE>=60,na.rm = T); plus65_pool <- sum(data$AGE>=65,na.rm = T)
        plus70_pool <- sum(data$AGE>=70,na.rm = T); plus75_pool <- sum(data$AGE>=75,na.rm = T); plus80_pool <- sum(data$AGE>=80,na.rm = T)
        under40_pool <- sum(data$AGE<40,na.rm = T); under45_pool <- sum(data$AGE<45,na.rm = T); under50_pool <- sum(data$AGE<50,na.rm = T) 
        under55_pool <- sum(data$AGE<55,na.rm = T); under60_pool <- sum(data$AGE<60,na.rm = T); under65_pool <- sum(data$AGE<65,na.rm = T)
        under70_pool <- sum(data$AGE<70,na.rm = T); under75_pool <- sum(data$AGE<75,na.rm = T); under80_pool <- sum(data$AGE<80,na.rm = T)
        
        plus40_sel <- sum(data$AGE>=40 & data$SELECTION=="Y",na.rm=TRUE); plus45_sel <- sum(data$AGE>=45 & data$SELECTION=="Y",na.rm=TRUE); plus50_sel <- sum(data$AGE>=50 & data$SELECTION=="Y",na.rm=TRUE) 
        plus55_sel <- sum(data$AGE>=55 & data$SELECTION=="Y",na.rm=TRUE); plus60_sel <- sum(data$AGE>=60 & data$SELECTION=="Y",na.rm=TRUE); plus65_sel <- sum(data$AGE>=65 & data$SELECTION=="Y",na.rm=TRUE)
        plus70_sel <- sum(data$AGE>=70 & data$SELECTION=="Y",na.rm=TRUE); plus75_sel <- sum(data$AGE>=75 & data$SELECTION=="Y",na.rm=TRUE); plus80_sel <- sum(data$AGE>=80 & data$SELECTION=="Y",na.rm=TRUE)
        under40_sel <- sum(data$AGE<40 & data$SELECTION=="Y",na.rm=TRUE); under45_sel <- sum(data$AGE<45 & data$SELECTION=="Y",na.rm=TRUE); under50_sel <- sum(data$AGE<50 & data$SELECTION=="Y",na.rm=TRUE) 
        under55_sel <- sum(data$AGE<55 & data$SELECTION=="Y",na.rm=TRUE); under60_sel <- sum(data$AGE<60 & data$SELECTION=="Y",na.rm=TRUE); under65_sel <- sum(data$AGE<65 & data$SELECTION=="Y",na.rm=TRUE)
        under70_sel <- sum(data$AGE<70 & data$SELECTION=="Y",na.rm=TRUE); under75_sel <- sum(data$AGE<75 & data$SELECTION=="Y",na.rm=TRUE); under80_sel <- sum(data$AGE<80 & data$SELECTION=="Y",na.rm=TRUE)
        
        if(plus40_pool==0){plus40_rate <- 0}else{plus40_rate <- (plus40_sel)/(plus40_pool)}
        if(plus45_pool==0){plus45_rate <- 0}else{plus45_rate <- (plus45_sel)/(plus45_pool)}
        if(plus50_pool==0){plus50_rate <- 0}else{plus50_rate <- (plus50_sel)/(plus50_pool)}
        if(plus55_pool==0){plus55_rate <- 0}else{plus55_rate <- (plus55_sel)/(plus55_pool)}
        if(plus60_pool==0){plus60_rate <- 0}else{plus60_rate <- (plus60_sel)/(plus60_pool)}
        if(plus65_pool==0){plus65_rate <- 0}else{plus65_rate <- (plus65_sel)/(plus65_pool)}
        if(plus70_pool==0){plus70_rate <- 0}else{plus70_rate <- (plus70_sel)/(plus70_pool)}
        if(plus75_pool==0){plus75_rate <- 0}else{plus75_rate <- (plus75_sel)/(plus75_pool)}
        if(plus80_pool==0){plus80_rate <- 0}else{plus80_rate <- (plus80_sel)/(plus80_pool)}
        
        if(under40_pool==0){under40_rate <- 0}else{under40_rate <- (under40_sel)/(under40_pool)}
        if(under45_pool==0){under45_rate <- 0}else{under45_rate <- (under45_sel)/(under45_pool)}
        if(under50_pool==0){under50_rate <- 0}else{under50_rate <- (under50_sel)/(under50_pool)}
        if(under55_pool==0){under55_rate <- 0}else{under55_rate <- (under55_sel)/(under55_pool)}
        if(under60_pool==0){under60_rate <- 0}else{under60_rate <- (under60_sel)/(under60_pool)}
        if(under65_pool==0){under65_rate <- 0}else{under65_rate <- (under65_sel)/(under65_pool)}
        if(under70_pool==0){under70_rate <- 0}else{under70_rate <- (under70_sel)/(under70_pool)}
        if(under75_pool==0){under75_rate <- 0}else{under75_rate <- (under75_sel)/(under75_pool)}
        if(under80_pool==0){under80_rate <- 0}else{under80_rate <- (under80_sel)/(under80_pool)}
      }
      
      
      #Decisitional Unit, Analysis Header, and Blank Row
      HeaderColnames <- c("Analysis","Selection Rate for Group on Left","Selection Rate for Group on Right","Better Performing Group","Chi-Square","Fisher's Exact","Exact Binomial","4/5ths Impact Ratios") %>% matrix(.,ncol=length(.),byrow=T)
      HeaderDecUnit <- paste("Decisional Unit: ",decisional_unit,collapse=NULL,sep="") %>% .[1:length(HeaderColnames)] %>% tidyr::replace_na("") %>% matrix(., ncol=length(.), byrow=T)
      # Header <- matrix(c(paste("Decisional Unit:  ",decisional_unit,collapse=NULL,sep=""),"","","","","","Analysis","Selection Rate for Group on Left","Selection Rate for Group on Right","Better Performing Group","Chi-Square","Fisher's Exact","Exact Binomial","4/5ths Impact Ratios"),ncol=6,byrow=T)
      # break_row <- matrix(c("","","","","",""),ncol=6,byrow=T)
      # break_row <- matrix("",ncol=length(Header[2,]),byrow=T)
      break_row <- matrix("",ncol=length(HeaderColnames),byrow=T)
      write.table(HeaderDecUnit,file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
      write.table(HeaderColnames,file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
      # write.table(Header,file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
      write.table(break_row,file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
      
      #Analysis Function Runs the demographic-specific analyses
      Analysis=function(left_group=NA,left_pool=NA,left_sel=NA,left_rate=NA,right_group=NA,right_pool=NA,right_sel=NA,right_rate=NA,sig_only="NO"){
        # sel1=36; sel2=180; pool1=107; pool2=304
        # obs <- data.frame(notsel=c(pool1-sel1, pool2-sel2), sel=c(sel1, sel2)) %>% {row.names(.)<-c("left", "right"); .}
        # (csqt <- chisq.test(obs))
        # csqt$stdres
        # chisq(sel1, sel2, pool1, pool2)
        
        rif_matrix <- matrix(c(right_sel,left_sel,right_pool-right_sel,left_pool-left_sel),nrow=2,byrow=T)
        if(right_rate == 1 && left_rate == 1){ #both 100% termination rates
          rif_chi <- list(zscore=0, sig=F, p.value=NA); rif_fish <- list(zscore=0, sig=F, p.value=NA)
        }else{
          if(right_sel == 0 && left_sel == 0){ #No terminations
            rif_chi <- list(zscore=0, sig=F, p.value=NA); rif_fish <- list(zscore=0, sig=F, p.value=NA)
          }else{
            if(right_pool == 0 || left_pool == 0){ #One group has no observations
              rif_chi <- list(zscore=0, sig=F, p.value=NA); rif_fish <- list(zscore=0, sig=F, p.value=NA)
            }else{
              rif_chi <- chisq(left_sel,right_sel,left_pool,right_pool)
              # rif_fish <- if(right_rate>left_rate){qnorm(fisher.test(rif_matrix)$p.value/2)}else{-qnorm(fisher.test(rif_matrix)$p.value/2)}
              rif_fish <- fisher.test(rif_matrix)
              rif_fish$zscore <- if(right_rate>left_rate){qnorm(rif_fish$p.value/2)}else{-qnorm(rif_fish$p.value/2)}
              rif_fish$sig <- if(abs(rif_fish$zscore)>1.95996398454005){T}else{F}
            }
          }
        }
        rif_binom <- bitest(workforce=left_sel+right_sel, protectedclass=left_sel, availability={left_pool/(left_pool+right_pool)}, output="all")
        if(all(is.na(rif_binom))){rif_binom <- list(zscore=as.numeric(NA), p.value=as.numeric(NA))}
        # rif_binom <- list(p.value=.04, zscore=1.99)
        rif_45ths <- fourfifths_ir(sel_l=left_sel, sel_r=right_sel, total_l=left_pool, total_r=right_pool) ##HERE
        
        
        ###########################
        if(positive.selections=="TRUE"){
          if(rif_chi$zscore> 1.95996398454005){impacted_group<-right_group}else{if(rif_chi$zscore< (-1.95996398454005)){impacted_group<-left_group}else{impacted_group<-"--"}}
          rif_output <- matrix(c(paste(left_group," v. ",right_group,collapse=NULL,sep=""),paste(left_sel,"/",left_pool,"break",floor(left_rate*100+0.5),"%",collapse=NULL,sep=""),paste(right_sel,"/",right_pool,"break",floor(right_rate*100+0.5),"%",collapse=NULL,sep=""),impacted_group,-rif_chi$zscore,-rif_fish$zscore),nrow=1,byrow=T)
          
          if(sig_only=="YES"){  #If it's an age analysis other than 40+ and we only want to report significant results 
            if(abs(rif_chi$zscore) > 1.95996398454005){write.table(matrix(c(decisional_unit,rif_output[1],impacted_group),nrow=1),file="RIF Summary.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
              write.table(rif_output,file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)}}
          
          if(sig_only=="NO"){ #If it's a Gender, 40+, Overall Race, or Sub-Race analysis  
            write.table(rif_output,file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
            if(abs(rif_chi$zscore) > 1.95996398454005){write.table(matrix(c(decisional_unit,rif_output[1],impacted_group),nrow=1),file="RIF Summary.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)}
          }
          ### FILL IN OTHER METRICS HERE
          
        }else{
          sig_chi <- if(abs(rif_chi$zscore)>1.95996398454005){T}else{F}
          sig_fish <- if(abs(rif_fish$zscore)>1.95996398454005){T}else{F}
          # sig_binom <- if(as.numeric(rif_binom$p.value)<.05){T}else{F}
          sig_binom <- ifelse(as.numeric(rif_binom$p.value)<.05, T, F) #%>% tidyr::replace_na(F)
          sig_45ths <- rif_45ths$sig
          sig_any <- any(na.omit(c(sig_chi, sig_fish, sig_binom)))#, sig_45ths)))
          sig_any_any <- any(na.omit(c(sig_chi, sig_fish, sig_binom, sig_45ths)))
          if(sig_any_any){
            # if(sign(rif_chi$zscore)!=sign(rif_fish$zscore))
            ZSCORE_SIGNS <- data.frame(sign_chi=sign(rif_chi$zscore), sign_fish=sign(rif_fish$zscore), sign_binom=sign(rif_binom$zscore))
            if(length(unique(na.omit(unlist(ZSCORE_SIGNS))))>1){
              ZSCORE_SIGNS_DF <- ZSCORE_SIGNS %>% mutate(decisional_unit=decisional_unit, zscore_chi=rif_chi$zscore, zscore_fish=rif_fish$zscore, zscore_binom=rif_binom$zscore, pvalue_chi=rif_chi$p.value, pvalue_fish=rif_fish$p.value, pvalue_binom=rif_binom$p.value, ir45ths_left=rif_45ths$ir_l, ir45ths_right=rif_45ths$ir_r, ir45ths_left_adj=rif_45ths$ir_l_adj, ir45ths_right_adj=rif_45ths$ir_r_adj, left_group=left_group,left_pool=left_pool,left_sel=left_sel,left_rate=left_rate,right_group=right_group,right_pool=right_pool,right_sel=right_sel,right_rate=right_rate)
              cat("ZSCORE_SIGNS_DF:\n")
              print(ZSCORE_SIGNS_DF)
              # write(line,file="ZSCORE_SIGNS.txt",append=TRUE)
              # write.table(ZSCORE_SIGNS_DF, file="ZSCORE_SIGNS.csv", append=TRUE,qmethod="double",sep=",",row.names=F)
            }
            if(rif_chi$zscore>0){impacted_group<-left_group}else if(rif_chi$zscore<0|rif_fish$zscore<0|rif_45ths$sig==T){impacted_group<-right_group}else{impacted_group<-"--"}
          } else {
            impacted_group<-"--"
          }
          # if(rif_chi$zscore> 1.95996398454005){impacted_group=left_group}else{if(rif_chi$zscore< -1.95996398454005){impacted_group=right_group}else{impacted_group="--"}}
          rif_output <- matrix(c(paste(left_group," v. ",right_group,collapse=NULL,sep=""),paste(left_sel,"/",left_pool,"break",floor(left_rate*100+0.5),"%",collapse=NULL,sep=""),paste(right_sel,"/",right_pool,"break",floor(right_rate*100+0.5),"%",collapse=NULL,sep=""),impacted_group,rif_chi$zscore,rif_fish$zscore, rif_binom$formatted,rif_45ths$formatted),nrow=1,byrow=T)
          
          if(sig_only=="YES"){  #If it's an age analysis other than 40+ and we only want to report significant results 
            if(sig_any){
              write.table(matrix(c(decisional_unit,rif_output[1],impacted_group),nrow=1),file="RIF Summary.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
              write.table(rif_output,file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
            }
          }
          
          if(sig_only=="NO"){ #If it's a Gender, 40+, Overall Race, or Sub-Race analysis  
            write.table(rif_output,file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
            # if(abs(rif_chi$zscore) > 1.95996398454005){write.table(matrix(c(decisional_unit,rif_output[1],impacted_group),nrow=1),file="RIF Summary.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)}
            if(T){
              write.table({matrix(c(decisional_unit,rif_output[1],impacted_group),nrow=1)},file="RIF Summary.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE)
            }
          }
          
          STATISTICAL_DETAILS <- tibble(decisional_unit=decisional_unit,left_group=left_group,right_group=right_group, better_scoring_group=impacted_group, zscore_chi=rif_chi$zscore, zscore_fish=rif_fish$zscore, zscore_binom=rif_binom$zscore, pvalue_chi=rif_chi$p.value, pvalue_fish=rif_fish$p.value, pvalue_binom=rif_binom$p.value, ir45ths_left=rif_45ths$ir_l, ir45ths_right=rif_45ths$ir_r, ir45ths_left_adj=rif_45ths$ir_l_adj, ir45ths_right_adj=rif_45ths$ir_r_adj,left_pool=left_pool,left_sel=left_sel,left_rate=left_rate,right_pool=right_pool,right_sel=right_sel,right_rate=right_rate) %>%
            mutate_all(function(v){as.character(v) %>% recode_na("")})# %>% replace_na("-") %>% recode(., "NaN"="-")})
          
          if(!file.exists("statistical_details.csv")){colnames_statistical_details=T} else {colnames_statistical_details=F}
          write.table(STATISTICAL_DETAILS, file="statistical_details.csv", append=TRUE,qmethod="double",sep=",",row.names=F, col.names=colnames_statistical_details)
          
        }  
      }
      
      #Age Analyses
      if("AGE" %in% demographics){
        Analysis(left_group="40+",left_pool=plus40_pool,left_sel=plus40_sel,left_rate=plus40_rate,right_group="Under 40",right_pool=under40_pool,right_sel=under40_sel,right_rate=under40_rate,sig_only="NO")
        Analysis(left_group="45+",left_pool=plus45_pool,left_sel=plus45_sel,left_rate=plus45_rate,right_group="Under 45",right_pool=under45_pool,right_sel=under45_sel,right_rate=under45_rate,sig_only="YES")
        Analysis(left_group="50+",left_pool=plus50_pool,left_sel=plus50_sel,left_rate=plus50_rate,right_group="Under 50",right_pool=under50_pool,right_sel=under50_sel,right_rate=under50_rate,sig_only="YES")
        Analysis(left_group="55+",left_pool=plus55_pool,left_sel=plus55_sel,left_rate=plus55_rate,right_group="Under 55",right_pool=under55_pool,right_sel=under55_sel,right_rate=under55_rate,sig_only="YES")
        Analysis(left_group="60+",left_pool=plus60_pool,left_sel=plus60_sel,left_rate=plus60_rate,right_group="Under 60",right_pool=under60_pool,right_sel=under60_sel,right_rate=under60_rate,sig_only="YES")
        Analysis(left_group="65+",left_pool=plus65_pool,left_sel=plus65_sel,left_rate=plus65_rate,right_group="Under 65",right_pool=under65_pool,right_sel=under65_sel,right_rate=under65_rate,sig_only="YES")
        Analysis(left_group="70+",left_pool=plus70_pool,left_sel=plus70_sel,left_rate=plus70_rate,right_group="Under 70",right_pool=under70_pool,right_sel=under70_sel,right_rate=under70_rate,sig_only="YES")
        Analysis(left_group="75+",left_pool=plus75_pool,left_sel=plus75_sel,left_rate=plus75_rate,right_group="Under 75",right_pool=under75_pool,right_sel=under75_sel,right_rate=under75_rate,sig_only="YES")
        Analysis(left_group="80+",left_pool=plus80_pool,left_sel=plus80_sel,left_rate=plus80_rate,right_group="Under 80",right_pool=under80_pool,right_sel=under80_sel,right_rate=under80_rate,sig_only="YES")
      }
      
      #Gender Analysis
      if("GENDER" %in% demographics){Analysis(left_group="Female",left_pool=female_pool,left_sel=female_sel,left_rate=female_rate,right_group="Male",right_pool=male_pool,right_sel=male_sel,right_rate=male_rate,sig_only="NO")}
      
      #Overall Analysis
      if("OVERALLRACE" %in% demographics){Analysis(left_group="Minority",left_pool=min_pool,left_sel=min_sel,left_rate=min_rate,right_group="White",right_pool=nonminority_pool,right_sel=nonminority_sel,right_rate=nonminority_rate,sig_only="NO")} #{left_group="Minority";left_pool=min_pool;left_sel=min_sel;left_rate=min_rate;right_group="White";right_pool=nonminority_pool;right_sel=nonminority_sel;right_rate=nonminority_rate;sig_only="NO"}
      
      #Sub-Race Analyses
      if("SUB-RACE" %in% demographics){
        
        #White as Default
        if(overall_white_pool/overall_pool >= 0.02 & overall_black_pool/overall_pool >= 0.02 & white_pool >= 2 & black_pool >= 2){
          Analysis(left_group="Black",left_pool=black_pool,left_sel=black_sel,left_rate=black_rate,right_group="White",right_pool=white_pool,right_sel=white_sel,right_rate=white_rate,sig_only="NO")}
        
        if(overall_white_pool/overall_pool >= 0.02 & overall_hispanic_pool/overall_pool >= 0.02 & white_pool >= 2 & hispanic_pool >= 2){  
          Analysis(left_group="Hispanic",left_pool=hispanic_pool,left_sel=hispanic_sel,left_rate=hispanic_rate,right_group="White",right_pool=white_pool,right_sel=white_sel,right_rate=white_rate,sig_only="NO")} 
        
        if(overall_white_pool/overall_pool >= 0.02 & overall_asian_pool/overall_pool >= 0.02 & white_pool >= 2 & asian_pool >= 2){  
          Analysis(left_group="Asian",left_pool=asian_pool,left_sel=asian_sel,left_rate=asian_rate,right_group="White",right_pool=white_pool,right_sel=white_sel,right_rate=white_rate,sig_only="NO")}
        
        if(overall_white_pool/overall_pool >= 0.02 & overall_natam_pool/overall_pool >= 0.02 & white_pool >= 2 & natam_pool >= 2){
          Analysis(left_group="American Indian or Alaskan Native",left_pool=natam_pool,left_sel=natam_sel,left_rate=natam_rate,right_group="White",right_pool=white_pool,right_sel=white_sel,right_rate=white_rate,sig_only="NO")}
        
        if(overall_white_pool/overall_pool >= 0.02 & overall_nathaw_pool/overall_pool >= 0.02 & white_pool >= 2 & nathaw_pool >= 2){  
          Analysis(left_group="Native Hawaiian/Other Pacific Islander",left_pool=nathaw_pool,left_sel=nathaw_sel,left_rate=nathaw_rate,right_group="White",right_pool=white_pool,right_sel=white_sel,right_rate=white_rate,sig_only="NO")}
        
        if(overall_white_pool/overall_pool >= 0.02 & overall_twoplus_pool/overall_pool >= 0.02 & white_pool >= 2 & twoplus_pool >= 2){
          Analysis(left_group="Two or More Races",left_pool=twoplus_pool,left_sel=twoplus_sel,left_rate=twoplus_rate,right_group="White",right_pool=white_pool,right_sel=white_sel,right_rate=white_rate,sig_only="NO")}
        
        #Black as Default
        if(overall_black_pool/overall_pool >= 0.02 & overall_hispanic_pool/overall_pool >= 0.02 & black_pool >= 2 & hispanic_pool >= 2){  
          Analysis(left_group="Hispanic",left_pool=hispanic_pool,left_sel=hispanic_sel,left_rate=hispanic_rate,right_group="Black",right_pool=black_pool,right_sel=black_sel,right_rate=black_rate,sig_only="NO")} #{left_group="Hispanic";left_pool=hispanic_pool;left_sel=hispanic_sel;left_rate=hispanic_rate;right_group="Black";right_pool=black_pool;right_sel=black_sel;right_rate=black_rate;sig_only="NO"}
        
        if(overall_black_pool/overall_pool >= 0.02 & overall_asian_pool/overall_pool >= 0.02 & black_pool >= 2 & asian_pool >= 2){
          Analysis(left_group="Asian",left_pool=asian_pool,left_sel=asian_sel,left_rate=asian_rate,right_group="Black",right_pool=black_pool,right_sel=black_sel,right_rate=black_rate,sig_only="NO")}
        
        if(overall_black_pool/overall_pool >= 0.02 & overall_natam_pool/overall_pool >= 0.02 & black_pool >= 2 & natam_pool >= 2){  
          Analysis(left_group="American Indian or Alaskan Native",left_pool=natam_pool,left_sel=natam_sel,left_rate=natam_rate,right_group="Black",right_pool=black_pool,right_sel=black_sel,right_rate=black_rate,sig_only="NO")}
        
        if(overall_black_pool/overall_pool >= 0.02 & overall_nathaw_pool/overall_pool >= 0.02 & black_pool >= 2 & nathaw_pool >= 2){  
          Analysis(left_group="Native Hawaiian/Other Pacific Islander",left_pool=nathaw_pool,left_sel=nathaw_sel,left_rate=nathaw_rate,right_group="Black",right_pool=black_pool,right_sel=black_sel,right_rate=black_rate,sig_only="NO")}
        
        if(overall_black_pool/overall_pool >= 0.02 & overall_twoplus_pool/overall_pool >= 0.02 & black_pool >= 2 & twoplus_pool >= 2){  
          Analysis(left_group="Two or More Races",left_pool=twoplus_pool,left_sel=twoplus_sel,left_rate=twoplus_rate,right_group="Black",right_pool=black_pool,right_sel=black_sel,right_rate=black_rate,sig_only="NO")}
        
        
        #Hispanic as Default
        if(overall_hispanic_pool/overall_pool >= 0.02 & overall_asian_pool/overall_pool >= 0.02 & hispanic_pool >= 2 & asian_pool >= 2){
          Analysis(left_group="Asian",left_pool=asian_pool,left_sel=asian_sel,left_rate=asian_rate,right_group="Hispanic",right_pool=hispanic_pool,right_sel=hispanic_sel,right_rate=hispanic_rate,sig_only="NO")}
        
        if(overall_hispanic_pool/overall_pool >= 0.02 & overall_natam_pool/overall_pool >= 0.02 & hispanic_pool >= 2 & natam_pool >= 2){
          Analysis(left_group="American Indian or Alaskan Native",left_pool=natam_pool,left_sel=natam_sel,left_rate=natam_rate,right_group="Hispanic",right_pool=hispanic_pool,right_sel=hispanic_sel,right_rate=hispanic_rate,sig_only="NO")}
        
        if(overall_hispanic_pool/overall_pool >= 0.02 & overall_nathaw_pool/overall_pool >= 0.02 & hispanic_pool >= 2 & nathaw_pool >= 2){    
          Analysis(left_group="Native Hawaiian/Other Pacific Islander",left_pool=nathaw_pool,left_sel=nathaw_sel,left_rate=nathaw_rate,right_group="Hispanic",right_pool=hispanic_pool,right_sel=hispanic_sel,right_rate=hispanic_rate,sig_only="NO")}
        
        if(overall_hispanic_pool/overall_pool >= 0.02 & overall_twoplus_pool/overall_pool >= 0.02 & hispanic_pool >= 2 & twoplus_pool >= 2){
          Analysis(left_group="Two or More Races",left_pool=twoplus_pool,left_sel=twoplus_sel,left_rate=twoplus_rate,right_group="Hispanic",right_pool=hispanic_pool,right_sel=hispanic_sel,right_rate=hispanic_rate,sig_only="NO")}
        
        #Asian as Default
        if(overall_asian_pool/overall_pool >= 0.02 & overall_natam_pool/overall_pool >= 0.02 & asian_pool >= 2 & natam_pool >= 2){
          Analysis(left_group="American Indian or Alaskan Native",left_pool=natam_pool,left_sel=natam_sel,left_rate=natam_rate,right_group="Asian",right_pool=asian_pool,right_sel=asian_sel,right_rate=asian_rate,sig_only="NO")}
        
        if(overall_asian_pool/overall_pool >= 0.02 & overall_nathaw_pool/overall_pool >= 0.02 & asian_pool >= 2 & nathaw_pool >= 2){
          Analysis(left_group="Native Hawaiian/Other Pacific Islander",left_pool=nathaw_pool,left_sel=nathaw_sel,left_rate=nathaw_rate,right_group="Asian",right_pool=asian_pool,right_sel=asian_sel,right_rate=asian_rate,sig_only="NO")}
        
        if(overall_asian_pool/overall_pool >= 0.02 & overall_twoplus_pool/overall_pool >= 0.02 & asian_pool >= 2 & twoplus_pool >= 2){
          Analysis(left_group="Two or More Races",left_pool=twoplus_pool,left_sel=twoplus_sel,left_rate=twoplus_rate,right_group="Asian",right_pool=asian_pool,right_sel=asian_sel,right_rate=asian_rate,sig_only="NO")}
        
        #Native American as Default
        if(overall_natam_pool/overall_pool >= 0.02 & overall_nathaw_pool/overall_pool >= 0.02 & natam_pool >= 2 & nathaw_pool > 2){
          Analysis(left_group="Native Hawaiian/Other Pacific Islander",left_pool=nathaw_pool,left_sel=nathaw_sel,left_rate=nathaw_rate,right_group="American Indian or Alaskan Native",right_pool=natam_pool,right_sel=natam_sel,right_rate=natam_rate,sig_only="NO")}
        
        if(overall_natam_pool/overall_pool >= 0.02 & overall_twoplus_pool/overall_pool >= 0.02 & natam_pool >= 2 & twoplus_pool >= 2){
          Analysis(left_group="Two or More Races",left_pool=twoplus_pool,left_sel=twoplus_sel,left_rate=twoplus_rate,right_group="American Indian or Alaskan Native",right_pool=natam_pool,right_sel=natam_sel,right_rate=natam_rate,sig_only="NO")}
        
        #NHPI as Default
        if(overall_nathaw_pool/overall_pool >= 0.02 & overall_twoplus_pool/overall_pool >= 0.02 & nathaw_pool >= 2 & twoplus_pool >= 2){
          Analysis(left_group="Two or More Races",left_pool=twoplus_pool,left_sel=twoplus_sel,left_rate=twoplus_rate,right_group="Native Hawaiian/Other Pacific Islander",right_pool=nathaw_pool,right_sel=nathaw_sel,right_rate=nathaw_rate,sig_only="NO")}
        
      }
      
      write.table(rbind(break_row,break_row,break_row),file="RIF Results.csv",row.names=FALSE,col.names=FALSE,qmethod="double",sep=",",append=TRUE) #Three row break between each decisional unit
      cat("Decisional Unit:",decisional_unit,"Has Been Analyzed","\n")
    } #End RIF_by_group Function
    
    
    
    writexl_open_formatted_warn <- function(x=NULL, filename=NULL, open_file=T, maxcolwidth=20, colwidthplus=0, freeze_after_col=1, clean_colnames=T, autofilter=F){
      # library(openxlsx)
      if(!exists("loadWorkbook")){
        load_unload_openxlsx <- T
        pkg('openxlsx')
      } else {
        load_unload_openxlsx <- F
      }
      if(is.nanull(filename)){
        filename=tempfile(fileext = ".xlsx")
      }
      if(!file.exists(filename)|is.data.frame(x)|is.list(x)){
        df <- if(clean_colnames){x %>% setNames(names(.) %>% gsub("_", " ", .))} else {x}
        writexl::write_xlsx(df, filename)
      }
      sheetnames <- readxl::excel_sheets(filename)
      wb = #openxlsx::
        loadWorkbook(filename)
      for (sheetname in sheetnames){
        wbdf = if(!is.data.frame(x)){readxl::read_excel(filename, sheet=sheetname)} else {x}
        maxrownum <- nrow(wbdf)+1
        # activeSheet(wb) <- sheetname
        if(is.numeric(freeze_after_col)){
          freeze_before_colnum <- freeze_after_col + 1
        } else if(lookslike_number(freeze_after_col)){
          freeze_before_colnum <- as.numeric(freeze_after_col) + 1
        } else {
          freeze_before_colnum <- grep(freeze_after_col, names(wbdf)) %>% max() %>% {.+1}
        }
        LabelStyle <- #openxlsx::
          createStyle(halign = "center", border = c("bottom"), borderStyle = "thin", textDecoration = "bold", 
                      wrapText=T, valign="center"# fgFill = "#2020FF", fontColour = "white"
          )
        if(autofilter){# openxlsx::
          addFilter(wb, sheet=sheetname, row=1, cols=1:ncol(wbdf))
        }
        # openxlsx::
        addStyle(wb, sheet=sheetname, style=LabelStyle, rows=1, cols=1:ncol(wbdf))
        
        conditionalFormatting(wb, sheetname, cols=1:ncol(wbdf), rows=2:maxrownum, type="contains", rule='YES', style={createStyle(fontColour="#ff4942", textDecoration="bold", wrapText=T, valign="center")}) # "Cleared with Fisher's Exact" column formatting
        
        # freezePane(wb, 1, firstRow=T, firstCol=T)
        tryCatch({#openxlsx::
          freezePane(wb, sheet=sheetname, firstActiveRow=2, firstActiveCol=freeze_before_colnum)}, error=function(e){#openxlsx::
            freezePane(wb, sheet=sheetname, firstActiveRow=2, firstActiveCol=1)})
        
        width_vec1 <- apply(wbdf, 2, function(x){max(nchar(as.character(x))+1+colwidthplus, na.rm = TRUE)})
        width_vec2 <- names(wbdf) %>% sapply(., function(x){
          xw <- strsplit(x, split=" |[[:space:]]|-|\\.") %>%unlist() %>% trimws_() %>% as.list() %>% setNames(names(.)<-.) %>% nchar() %>% max()
          # xw <- map_chr(strsplit(x, " |[[:space:]]|-|\\."), ~ .[which.max(nchar(.))])
          xw+1+colwidthplus
        })
        width_vec <- tibble(width_vec1, width_vec2) %>% rowwise() %>% mutate(width_vec = max(width_vec1, width_vec2)) %>% .$width_vec %>% sapply(., function(n){min(maxcolwidth, n, na.rm=T)})
        # openxlsx::
        setColWidths(wb, sheet=sheetname, cols=1:ncol(wbdf), widths=width_vec)
        
      }
      # openxlsx::
      saveWorkbook(wb, filename, overwrite=TRUE)
      if(open_file){system_open(filename)}
      if(load_unload_openxlsx){unload_pkg("openxlsx")}
      # wbdf
      x
    }
    
    writexl_open_formatted_dia <- function(x=NULL, filename=NULL, open_file=T, maxcolwidth=20, colwidthplus=0, freeze_after_col=1, clean_colnames=T, autofilter=F, autofilter_row_start=1){ #{open_file=T; maxcolwidth=20; colwidthplus=0; freeze_after_col=1; clean_colnames=T; autofilter=T; autofilter_row_start=3}
      # # for(i in c(5, 6)){
      # #     x$rif_results[[i]] %<>% as.numeric()
      # # }
      # # dresults <- lod$rif_results
      # # dsummary <- lod$rif_summary
      # filename=paste0("results/rif_results_formatted-", sysdate(), ".xlsx")
      # clean_colnames=T
      # open_file=T
      # freeze_after_col=1
      # autofilter=F
      # colwidthplus=0
      # maxcolwidth=20
      
      if(!exists("loadWorkbook")){
        load_unload_openxlsx <- T
        pkg('openxlsx')
      } else {
        load_unload_openxlsx <- F
      }
      if(is.nanull(filename)){
        filename=tempfile(fileext = ".xlsx")
      }
      if(!file.exists(filename)|is.data.frame(x)|is.list(x)){
        df <- if(clean_colnames){x %>% setNames(names(.) %>% gsub("_", " ", .))} else {x}
        writexl::write_xlsx(df, filename)
      }
      sheetnames <- readxl::excel_sheets(filename)
      wb = #openxlsx::
        loadWorkbook(filename)
      for (sheetname in sheetnames){ #{sheetname=sheetnames[1]}
        wbdf = if(!is.data.frame(x)){readxl::read_excel(filename, sheet=sheetname)} else {x}
        # activeSheet(wb) <- sheetname
        if(is.numeric(freeze_after_col)){
          freeze_before_colnum <- freeze_after_col + 1
        } else if(lookslike_number(freeze_after_col)){
          freeze_before_colnum <- as.numeric(freeze_after_col) + 1
        } else {
          freeze_before_colnum <- grep(freeze_after_col, names(wbdf)) %>% max() %>% {.+1}
        }
        
        if(DEFINE_STYLES<-T){
          LabelStyle <- #openxlsx::
            createStyle(halign = "center", border=c("bottom"), borderStyle="thick", textDecoration="bold", wrapText=T, valign="center", fgFill="#3b1466", fontColour="#ff4942", fontSize="15")
          SubLabelStyle <- createStyle(halign="center", border=c("bottom"), borderStyle="thin", textDecoration="bold", wrapText=T, valign="center", fontColour="#3b1466", fontSize="11")
          ValueStyle <- createStyle(halign="center", border=c("bottom"), borderStyle="dotted", borderColour="#f5f5f5", wrapText=T, valign="center")
          flagStyle <- createStyle(fontColour="#9C0006", bgFill="yellow") #f7f76f is light yellow for significant flags
        }
        
        if(STYLE_RESULTS_SUMMARY<-T){
          
          maxrownum <- nrow(wbdf)+1
          if(grepl("result", sheetname, ignore.case=T)){
            
            if(STYLE_ANALYSIS_TYPE_ROW_HEADERS<-T){
              addStyle(wb, sheet=sheetname, style={createStyle(fontColour="#707070", textDecoration="bold", border=c("right", "top", "bottom"), borderStyle="dotted", borderColour="#f5f5f5", valign="center", halign="center", wrapText=T)}, rows=1:maxrownum, cols=1)
            }
            
            for(colnum in 1:ncol(wbdf)){
              
              if(STYLE_REGULAR_VALUES<-T){
                if(colnum!=1){addStyle(wb, sheet=sheetname, style=ValueStyle, rows=1:maxrownum, cols=colnum)}
              }
              for(rownum_df in 1:nrow(wbdf)){
                rownum = rownum_df + 1
                if(colnum %in% c(5, 6, 7)){
                  value <- wbdf[rownum_df, colnum][[1]]
                  value_numeric <- as.numeric(value)
                  if(!is.na(value_numeric)){value <- value_numeric}
                  writeData(wb=wb, sheet=sheetname, x=value, xy = c(colnum,rownum))
                }
              }
            }
            # addStyle(wb, sheet=sheetname, style=ValueStyle, rows=1:nrow(wbdf), cols=3)
            # for(i in c(5, 6)){
            #     # testNumberStyle = createStyle(halign = "center", border = c("bottom"), borderStyle = "thin", textDecoration = "bold", wrapText=T, valign="center")# fgFill = "#2020FF", fontColour = "white"
            #     flagStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
            #     conditionalFormatting(wb, sheetname, cols=5:6, rows = 1:nrow(wbdf)+1, rule = "<-1.95996398454005", style = flagStyle)
            #     addStyle(wb, sheet=sheetname, style=createStyle(numFmt = "0.000"), rows=1:{nrow(wbdf)+1}, cols=i)
            # }
            conditionalFormatting(wb, sheetname, cols=5:6, rows=1:maxrownum, rule='<-1.95996398454005', style=flagStyle)
            conditionalFormatting(wb, sheetname, cols=5:6, rows=1:maxrownum, rule='>1.95996398454005', style=flagStyle)
            conditionalFormatting(wb, sheetname, cols=7, rows=1:maxrownum, rule='AND(G1<.05, G1<>"")', style=flagStyle)
            conditionalFormatting(wb, sheetname, cols=c(7:8), rows=1:maxrownum, type="contains", rule='~*', style={flagStyle}) # "Cleared with Fisher's Exact" column formatting
            # conditionalFormatting(wb, sheetname, cols=4, rows=1:maxrownum, type="notContains", rule='-', style={flagStyle}) # "Cleared with Fisher's Exact" column formatting
            # conditionalFormatting(wb, sheetname, cols=4, rows=1:maxrownum, rule='=""', style=createStyle())
            
            for(colname in c("Analysis", "Selection Rate for Group on Left", "Selection Rate for Group on Right", "Better Performing Group", "Chi-Square", "Fisher's Exact", "Exact Binomial", "4/5ths Impact Ratios")){
              conditionalFormatting(wb, sheetname, cols=1:8, rows=1:maxrownum, rule={paste0('="', colname, '"')}, style={createStyle(fontColour="#707070", bgFill="white", textDecoration="bold", border=c("top", "bottom", "right"), borderStyle="thin")})
            }
            
            if(FORMAT_DECISIONAL_UNIT_HEADERS<-T){
              conditionalFormatting(wb, sheetname, cols=1, rows=2:maxrownum, type="contains", rule='Decisional Unit', style={createStyle(fontColour="#9D89B2", fontSize="14", border=c("top"), borderStyle="thin", textDecoration="bold", wrapText=T, valign="center", fgFill="#f5f5f5")}) # "Cleared with Fisher's Exact" column formatting
            }
            # conditionalFormatting(wb, sheetname, cols=5:6, rows=1:maxrownum, rule="value(E1)<-1.95996398454005", style=flagStyle)
            # conditionalFormatting(wb, sheetname, cols=5:6, rows=1:maxrownum, rule="value(E1)>1.95996398454005", style=flagStyle)
            
            # addStyle(wb, sheet=sheetname, style=SubLabelStyle, rows=2, cols=1:ncol(wbdf))
            
            if(FORMAT_TOP_HEADER<-T){
              # openxlsx::
              addStyle(wb, sheet=sheetname, style=LabelStyle, rows=1, cols=1:ncol(wbdf))
              mergeCells(wb, sheetname, cols=1:8, rows=1)
            }
            
          } else {
            if(autofilter){# openxlsx::
              addFilter(wb, sheet=sheetname, rows=autofilter_row_start, cols=1:ncol(wbdf))
            }
          }
          
          if(grepl("summary", sheetname, ignore.case=T)){
            addStyle(wb, sheet=sheetname, style=SubLabelStyle, rows=1, cols=1:ncol(wbdf))
            for(i in 1:ncol(wbdf)){
              addStyle(wb, sheet=sheetname, style=ValueStyle, rows=2:maxrownum, cols=i)
            }
            conditionalFormatting(wb, sheetname, cols=4:7, rows=1:maxrownum, rule='="No"', style=createStyle(fontColour="red")) # "Cleared with Fisher's Exact" column formatting
            addFilter(wb, sheet=sheetname, row=1, cols=1:ncol(wbdf))
          }
          
        }
        # freezePane(wb, 1, firstRow=T, firstCol=T)
        tryCatch({#openxlsx::
          freezePane(wb, sheet=sheetname, firstActiveRow=2, firstActiveCol=freeze_before_colnum)}, error=function(e){#openxlsx::
            freezePane(wb, sheet=sheetname, firstActiveRow=2, firstActiveCol=1)})
        
        width_vec1 <- apply(wbdf, 2, function(x){max(nchar(as.character(x))+1+colwidthplus, na.rm=TRUE)})
        # width_vec1 <- apply(wbdf, 2, function(x){(({max(nchar(unlist(strsplit(as.character(x), " |[[:space:]]|-|\\\n"))), na.rm=T)})+1+colwidthplus)})
        width_vec2 <- names(wbdf) %>% sapply(., function(x){
          xw <- strsplit(x, split=" |[[:space:]]|-|\\.|\\\n|_") %>% unlist() %>% trimws_() %>% as.list() %>% setNames(names(.)<-.) %>% nchar() %>% max()
          # xw <- map_chr(strsplit(x, " |[[:space:]]|-|\\."), ~ .[which.max(nchar(.))])
          xw+1+colwidthplus
        })
        width_vec <- tibble(width_vec1, width_vec2) %>% rowwise() %>% mutate(width_vec = max(width_vec1, width_vec2)) %>% .$width_vec %>% sapply(., function(n){min(maxcolwidth, n, na.rm=T)})
        # width_vec[1] <- width_vec[1] %>% {. + .*.75}
        # openxlsx::
        if(INCREASE_COL1_WIDTH<-T){
          # cat("\nwidth_vec prior:"); print(width_vec)
          width_vec[1] <- width_vec[1] + (width_vec1[[1]]/4)#(width_vec[1]/2) #+ 9
          # cat("\nwidth_vec after:"); print(width_vec)
        }
        
        setColWidths(wb, sheet=sheetname, cols=1:ncol(wbdf), widths=width_vec)
        
      }
      # openxlsx::
      saveWorkbook(wb, filename, overwrite=TRUE)
      if(open_file){system_open(filename)}
      if(load_unload_openxlsx){unload_pkg("openxlsx")}
      # wbdf
      x
      
    }
    
    
  }
  
}


























if(FUNCTIONS_DIVERSITY<-T){
  
  
  if(LOAD_DATASETS<-T){
    
    # census_geo_reference_files <- "https://www2.census.gov/geo/docs/maps-data/data/rel/"
    # state_place_ref_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/place/tab20_place20_place10_natl.txt"
    # zipcode_to_msa_crosswalk_url <- "https://www.dol.gov/owcp/regs/feeschedule/fee/fee11/fs11_gpci_by_msa-zip.xls"
    # zipcode_to_msa_crosswalk_fewer_url <- "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_cbsa_rel_10.txt"
    # state_place_shapefile_url <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_01_place_500k.zip"
    # state_place_shapefile_urls <- {state_fips=c("01", "02", "03", "04"); paste0("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_", state_fips, "_place_500k.zip")}
    if(!exists("county_set_county_fips_ref")){
      county_set_county_fips_ref <- "https://www2.census.gov/programs-surveys/demo/reference-files/eeo/time-series/eeo-county-sets-2010.xls" %>% rio::import(., skip=3) %>% as_tibble() %>% janitor::clean_names() %>%
        setNames(names(.) %>% recode("x2010_cs_code"="cbsafp", "fips_state_code"="state_fips", "fips_county_code"="county_fips")) %>% drop_rows_all_na() %>%
        fill(., one_of("cbsafp", "x2010_total_cs_population", "x2010_county_set_name", "x1"), .direction="down") %>%
        drop_na(cbsafp) %>%
        mutate(geoid_counties = paste0(state_fips, county_fips))
    }
    if(!exists("state_place_ref")){
      state_place_ref <- readr::read_delim("https://www2.census.gov/geo/docs/maps-data/data/rel2020/place/tab20_place20_place10_natl.txt", delim="|") %>% janitor::clean_names()
    }
    if(!exists("urban_area_crosswalk")){
      urban_area_crosswalk <- readr::read_delim("https://www2.census.gov/geo/docs/maps-data/data/rel/ua_ua00_rel_10.txt", delim=",") %>% janitor::clean_names()
      urban_are_msa_ref <- "https://www2.census.gov/geo/docs/maps-data/data/rel/ua_cbsa_rel_10.txt" %>% readr::read_delim(., delim=",") %>% janitor::clean_names()
      msa_crosswalk <- "https://www2.census.gov/geo/docs/maps-data/data/rel/ua_cbsa_rel_10.txt" %>% readr::read_delim(., delim=",") %>% janitor::clean_names()
    }
    if(!exists("puma_crosswalk")){
      puma_crosswalk <- rio::import(file="https://usa.ipums.org/usa/resources/volii/PUMA2000_PUMA2010_crosswalk.xls", which=1) %>% as_tibble() %>% janitor::clean_names() %>% select(-matches('pop|land|gisjoin|cpuma00|geoid')) %>% 
        mutate(stab00 = recode_state(state00), stab10 = recode_state(state10),
               state_puma00 = get_state_puma(stab00, puma00), state_puma10 = get_state_puma(stab10, puma10)) #%>% 
    }
    
    if(!exists("puma_msa_ref")){
      puma_msa_ref <- readr::read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv") %>% 
        add_row(state="vt", puma_description="SW Vermont-Rutland/Bennington+Addison Counties 2010-2019", msa_description="BENNINGTON, VT (MICRO)", state_puma="VT-00400", state_msa="VT-13540", msa=13540, type="Micro Area") %>%
        mutate(far_away = ifelse(msa=="15540"&grepl("Bennington", puma_description), T, F)) %>%
        select(-one_of("X8", "X9", "notes", "msa")) %>% select(-matches("^\\.\\.\\.\\d$"))
      # readr::read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv") %>% filter_if(is.factorchar, any_vars(grepl("B.*ngton", .))) %>% filter(state=="vt")
    }
    
    
    if(!exists("zip_msa_ref_fewer")){
      # zip_msa_ref_fewer <- readr::read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_cbsa_rel_10.txt") %>% select(-one_of("X8", "X9", "notes", "msa"), -matches("(PCT|POP|LAND|AREA|MHU|PT|MEMI|ZHU)$")) %>% janitor::clean_names() %>% rename(zip=zcta5, msa=cbsa) %>% distinct()
      zip_msa_ref_fewer <- rio::import("https://www.dol.gov/owcp/regs/feeschedule/fee/fee11/fs11_gpci_by_msa-zip.xls", skip=10) %>% janitor::clean_names() %>% select(-one_of("X8", "X9", "notes", "msa"), -matches("(PCT|POP|LAND|AREA|MHU|PT|MEMI|ZHU|gpci_\\d)$")) %>% distinct() %>% as_tibble() %>% select_if(not_all_na) %>% rename(zip=zip_code, msa=msa_no)# %>% rename(zip=zcta5, msa=cbsa)
    }
    
    
    if(!exists("df_1_row_per_msa_multi_puma")){
      df_1_row_per_msa_multi_puma <- puma_msa_ref %>%
        group_by_at(vars(one_of("state_msa"))) %>% 
        summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% unique_sep_sort(., ", ") %>% recode_na('', 'NA')) %>% ungroup()
    }
    
    if(!exists("df_1_row_per_puma_multi_msa")){
      df_1_row_per_puma_multi_msa <- puma_msa_ref %>%
        group_by_at(vars(one_of("state_puma"))) %>% 
        summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% unique_sep_sort(., ", ") %>% recode_na('', 'NA')) %>% ungroup()
    }
    
    if(!exists('zip_code_db')|!exists('zip_puma_ref')){
      # zip_puma_ref <- bind_rows(geocorr::zcta2010_to_puma2012, geocorr::zcta2010_to_puma2000 %>% setNames(gsub('2kName', 'name', names(.)) %>% gsub('2k', '12', .) )) %>% distinct() %>% distinct(puma12, zcta5, .keep_all=T) %>% select(-matches('intpt|pop10|afact')) %>% mutate(zip=zcta5, zipcode=zcta5) %>% mutate_all(function(v) tolower(iconv(enc2utf8(v))))
      zip_puma_ref <- geocorr::zcta2010_to_puma2012 %>% select(-matches('intpt|pop10|afact')) %>%
        add_row(zcta5=c("00968"), puma12=c("00803"), stab=c("pr")) %>% 
        mutate(zip=zcta5, zipcode=zcta5) %>% mutate_all(function(v) tolower(iconv(enc2utf8(v))))
      # writexl_open(zip_code_db, "zip_code_db.xlsx")
      zip_code_db <- (zip_code_db_github <- read_csv('https://raw.githubusercontent.com/DataUSA/datausa-tutorials/master/commuting_viz_tutorial/csv/zip_code_database.csv') %>% 
                        mutate(major_city=primary_city, zipcode=pad_leading_0s(zip), lat=latitude, lng=longitude, common_city_list=acceptable_cities %>% blob::vec_cast.blob() ) %>%
                        select(-one_of(setdiff(names(.), names(zipcodeR::zip_code_db)))) %>% # filter(!zipcode %in% zipcodeR::zip_code_db$zipcode) %>%
                        mutate(post_office_city = major_city )
      ) %>%
        bind_rows(zipcodeR::zip_code_db, .)
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
        zipcodeR::zcta_crosswalk # ZCTA5 TRACT GEOID
        zipcodeR::zip_code_db %>% filter(grepl('20227|20520|20565', zipcode))
        # zipcodeR::search_county("District of Columbia", "DC")
      }
    }
    
  }
  #=========================FUNCTIONS====================================
  
  # unique_sep_sort <- srhoads::unique_sep_sort #function(v, sep = "; "){sapply(v, function(s) strsplit(s, sep) %>% unlist() %>% unique() %>% sort() %>% paste0(., collapse=sep)) %>% as.character()}
  
  get_state_puma <- function(st, puma){
    state_abb <- srhoads::recode_state(st, abb=T)
    paste0(replace_na(state_abb, ''), '-', replace_na(puma, '')) %>% gsub('^-|-$', '', .)
  }
  
  mutate_demographic_pct_cols <- function(d, count_colname_indicator="_n$", group_colname_indicator="^(female|male|amind|amer_ind|asian|black|hisp|nhopi|twoplus|two|white|nonhisp|minority)_", total_colname_indicator="total"){
    DIVERSITY_COUNT_COLNAMES <- d %>% select(matches(count_colname_indicator), -matches(total_colname_indicator)) %>% names()
    for(DIVERSITY_COUNT_COLNAME in DIVERSITY_COUNT_COLNAMES){
      TOTAL_COUNT_COLNAME <- gsub(group_colname_indicator, paste0(total_colname_indicator, "_"), DIVERSITY_COUNT_COLNAME)
      newcol=gsub(count_colname_indicator, "_pct", DIVERSITY_COUNT_COLNAME)
      d[[newcol]] <- d[[DIVERSITY_COUNT_COLNAME]] / d[[TOTAL_COUNT_COLNAME]]
    }
    d
  }
  
  
  # puma_msa_ref %>% filter(is.na(msa_description)) %>% select(-matches('msa_desc')) %>% left_join()
  
  # puma_msa_ref %>% writexl_open('puma_msa_ref.xlsx')
  "Users/rhoadss/Downloads/PUMA2000_PUMA2010_crosswalk.xlsx" # has manually added msa x puma codes
  
  recode_msa_to_puma <- function(s, return_na_if_no_match=T){
    # if(!exists('puma_msa_ref')){puma_msa_ref <- read_csv("https://raw.githubusercontent.com/srhoads/census/main/puma_msa_dictionary.csv")}
    v_ <- s %>% strsplit(., ', ') %>% unlist()
    to <- c(df_1_row_per_msa_multi_puma$state_puma, df_1_row_per_msa_multi_puma$state_puma)
    from <- c(df_1_row_per_msa_multi_puma$state_msa, df_1_row_per_msa_multi_puma$msa)
    result <- (to[match(v_, from)]) %>% paste0(., collapse=", ")
    if(return_na_if_no_match){
      return(result)
    } else {
      return(ifelse(is.na(result), v_, result))
    }
  }
  
  recode_puma_to_msa <- function(s, return_na_if_no_match=T, keep_far_away=T){
    df_1_row_per_puma_multi_msa_X <- if(keep_far_away){df_1_row_per_puma_multi_msa}else{df_1_row_per_puma_multi_msa %>% filter(far_away==F)}
    # df_1_row_per_puma_multi_msa <- puma_msa_ref %>%
    #   group_by(state_puma) %>% 
    #   summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup()
    
    # print(s)
    v_ <- s %>% unique() %>% paste0(., collapse=", ") %>% recode_na(., "NA", "") %>% strsplit(., ', ') %>% unlist()
    # v_ <- s %>% strsplit(., ', ') %>% unlist()
    to <- df_1_row_per_puma_multi_msa_X$state_msa
    from <- df_1_row_per_puma_multi_msa_X$state_puma
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
  #   df_1_row_per_puma_multi_msa <- puma_msa_ref %>%
  #     group_by(state_puma) %>% 
  #     summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup()
  #   
  #   v_ <- s %>% strsplit(., ', ') %>% unlist()
  #   to <- df_1_row_per_puma_multi_msa$state_msa
  #   from <- df_1_row_per_puma_multi_msa$state_puma
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
  
  if(!exists("df_zipcode_puma_ref")){
    df_zipcode_puma_ref <- full_join(
      geocorr::zcta2010_to_puma2012 %>% mutate(state_puma = get_state_puma(stab, puma12)) %>% select(matches("zcta|puma"), -matches('name')),
      geocorr::zcta2010_to_puma2000 %>% mutate(state_puma = get_state_puma(stab, puma2k)) %>% select(matches("zcta|puma"), -matches('name')),
      by="zcta5",
      suffix=c("_12", "_2k")
    ) %>%
      add_row(zcta5 = c("80201", "48901", "59620", "57854", "33101", "73019", "91388", "94013", "94101", "95705", "00969"), state_puma_12=c("CO-00812", "MI-01802", "MT-00300", "ND-00100", "FL-08611", "OK-00900", "CA-03722", "CA-08102", "CA-07503", "CA-06707", "PR-00969")) %>%
      group_by(state_puma_12) %>% fill(., matches("puma|zcta"), .direction="downup") %>% ungroup() %>%
      mutate(state_puma = ifelse(state_puma_12==state_puma_2k, state_puma_12, ifelse(is.na(state_puma_2k)&!is.na(state_puma_12), state_puma_12, paste0(state_puma_12, ", ", state_puma_2k))), state_puma=ifelse(!is.na(state_puma), state_puma, state_puma_12)) %>% distinct() %>%
      {
        df_zipcode_puma_ref <- .
        d_addl <- rio::import("https://udsmapper.org/wp-content/uploads/2020/09/Zip_to_zcta_crosswalk_2020.xlsx") %>% janitor::clean_names() %>% as_tibble() %>% # %>% filter(zip_join_type=="Spatial join to ZCTA")
          filter(!zip_code %in% c(df_zipcode_puma_ref$zcta5)) %>%
          select(zcta5=zcta, matches("type|name|state|zip|zcta")) %>%
          # filter(!zcta5 %in% c(df_zipcode_puma_ref$zcta5)) %>%
          left_join(., mutate(df_zipcode_puma_ref, state=substr(state_puma, 1, 2)), by=c("zcta5", "state"), suffix=c("", "_y")) %>%
          mutate(zcta5 = zip_code) %>% 
          select(one_of(names(df_zipcode_puma_ref))) %>%
          distinct() %>%
          drop_na()
        bind_rows(df_zipcode_puma_ref, d_addl) %>% distinct() %>%
          mutate_all(., function(v) recode_na(v, "", "NA", "na", "99999")) %>%
          group_by(state_puma_12) %>% fill(., matches("puma|zcta"), .direction="downup") %>% ungroup() %>% distinct()
      }
    
    if(F){
      # df_zipcode_puma_ref %>% filter_all(any_vars(grepl("CO-00812", .))) %>% print(n=nrow(.))
      # df_zipcode_puma_ref %>% filter_all(any_vars(grepl("80201|48901|59620", .))) %>% print(n=nrow(.))
      # df_zipcode_puma_ref %>% filter_all(any_vars(grepl("58854", .))) %>% print(n=nrow(.))
      df_zipcode_puma_ref %>% filter_at(vars(matches("zcta|zip|post")), any_vars(grepl("85412|86949|75531|92669|95705|00807|34757|32010|83678", .))) %>% print(n=nrow(.))
      df_zipcode_puma_ref %>% filter_at(vars(matches("zcta|zip|post")), any_vars(grepl("33101|34757|65045|73019|91388|94013|94101|95705", .))) %>% print(n=nrow(.))
      # df_zipcode_puma_ref %>% filter_all(any_vars(grepl("58109|75261|20310|82071", .))) %>% print(n=nrow(.))
      # d_addl %>% filter_all(any_vars(grepl("58109", .))) %>% print(n=nrow(.))
      # df_zipcode_puma_ref %>% filter(is.na(puma12))
    }
    
  }
  
  if(!exists("df_1_row_per_zipcode")){
    # df_1_row_per_zipcode <- df_zipcode_puma_ref %>% group_by(zcta5) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% unique_sep_sort(., ", ") %>% recode_na('')) %>% ungroup()
    df_1_row_per_zipcode <- df_zipcode_puma_ref %>% group_by(zcta5) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup() %>% mutate_all(function(v) unique_sep_sort(v, ", "))
    # df_1_row_per_zipcode %>% filter_all(any_vars(grepl("58109|75261|20310|82071", .))) %>% print(n=nrow(.))
  }
  # puma_ref_with_2k_pumas <- left_join(df_1_row_per_puma_multi_msa, df_zipcode_puma_ref %>% mutate(state_puma=state_puma_12, zcta5=NULL, puma2k=NULL, puma12=NULL) %>% distinct()) %>% distinct()
  
  recode_zipcode_to_puma <- function(s="20175, 20176, 20177, 20178", include_2000_pumas=T, return_na_if_no_match=T, collapse=", "){
    v_ <- s %>% strsplit(., ', ') %>% unlist()
    if(include_2000_pumas){
      df_1_row_per_zipcode
    } else {
      # df_1_row_per_zipcode <- geocorr::zcta2010_to_puma2012 %>% mutate(state_puma = get_state_puma(stab, puma12)) %>% select(matches("zcta|puma"), -matches('name')) %>% group_by(zcta5) %>% summarize_all(., function(v) paste0(sort(unique(v)), collapse=', ') %>% recode_na('')) %>% ungroup()
      df_1_row_per_zipcode <- df_1_row_per_zipcode %>% mutate(state_puma = state_puma_12)
    }
    to <- df_1_row_per_zipcode %>% .$state_puma
    from <- df_1_row_per_zipcode$zcta5
    result <- (to[match(v_, from)]) %>% unique() %>% na.omit()
    if(return_na_if_no_match){
      return(result %>% paste0(., collapse=", "))
    } else {
      return(ifelse(is.na(result), v_, result) %>% paste0(., collapse=collapse))
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
  
  recode_zipcode_to_state <- function(zipcode=c("36101")){ #zipcodes=c("36101", "60007")
    tryCatch({zipcodeR::reverse_zipcode(zipcode) %>% .$state %>% unique() %>% paste0(., collapse="; ")}, error=function(e) NA)
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
        pkg('noncensus', url='https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz'); # install.packages('https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz', repos=NULL)
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
          result_ <- res %>% drop_na(properties_postcode) %>% filter(!duplicated(properties_postcode)) %>% slice(1:3) %>% mutate(zipcode = na_if_(as.character(srhoads::zipcode5(properties_postcode)))) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('', 'NA') %>% gsub('c\\(\\\"|\\\"|\\)|\\(', "", .) %>% unique_sep_sort(., ", ")
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
          result_ <- res %>% drop_na(properties_postcode) %>% filter(!duplicated(properties_postcode)) %>% slice(1) %>% mutate(zipcode = na_if_(as.character(srhoads::zipcode5(properties_postcode)))) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('', 'NA') %>% gsub('c\\(\\\"|\\\"|\\)|\\(', "", .) %>% unique_sep_sort(., ", ")
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
          pkg("noncensus", url="https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz'); # install.packages('https://cran.r-project.org/src/contrib/Archive/noncensus/noncensus_0.1.tar.gz", repos=NULL); data("zip_codes")
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
          result_ <- res %>% drop_na(properties_postcode) %>% filter(!duplicated(properties_postcode)) %>% slice(1:3) %>% mutate(zipcode = na_if_(as.character(srhoads::zipcode5(properties_postcode)))) %>% drop_na(zipcode) %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('', 'NA') %>% gsub('c\\(\\\"|\\\"|\\)|\\(', "", .) %>% unique_sep_sort(., ", ")
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
      
      result <- unique_sep_sort(result)
      
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
  
  
  
  
  # city="tuscaloosa"; state_abb="al"
  geocode_city <- function(city="Orange County", state_abb=NA, country_code="US", return_which=c("latlonstate", "latlon", "county", "everything", "state|city|postcode|country|name|county|street|coordinates")[1], return_n_results=1, verbose=F){
    sapply(1:length(city), function(i){ # {i<-1} #{city="94087"; state_abb="ca"}
      
      CityName <- gsub(" ", "%20", city[i]) %>% gsub("\\.", "", .)
      StateAbb <- state_abb[i]
      CountryCode <- country_code[i]
      
      if(!is.nanull(StateAbb)){
        StateName <- state_abb[i] %>% abbtostate() %>% gsub(" ", "%20", .) %>% gsub("\\.", "", .)
        URL <- paste0("http://photon.komoot.io/api/?q=", CityName, "?state=", StateName)
      } else {
        URL <- paste0("http://photon.komoot.io/api/?q=", CityName)
      }
      
      if(grepl("\\d{5}", substr(CityName, 1, 5))){
        URL <- paste0(URL, "?postcode=", CityName)
      } #else {
      # URL <- paste0(URL, "?city=", CityName)
      # }
      
      result <- tryCatch({
        (res <- jsonlite::fromJSON(rawToChar(httr::GET(URL)$content))$features %>% .[c('properties', 'geometry')] %>% unlist(., recursive=F) %>% as_tibble() %>% janitor::clean_names() %>% select(matches('city|state|zip|post'), everything(), -matches("properties_extent$")))
        if(nrow(res)>0){
          result_ <- res %>% drop_na(matches("geometry_coordinates")) %>% rowwise() %>% mutate_at(vars(one_of("geometry_coordinates")), function(v) as.character(paste0(unlist(paste0(v, collapse=", ")), collapse="; "))) %>% ungroup()
        } else {
          result_ <- tibble(properties_state=NA, properties_city=NA, properties_postcode=NA, properties_country=NA, properties_countrycode=NA, properties_name=NA, properties_county=NA, properties_street=NA, geometry_coordinates=NA)
        }
        
        if(!is.nanull(StateAbb)){
          result_ <- result_ %>% # filter_at(vars(one_of("properties_state")), grepl(paste0(abbtostate(state_abb),"|^",state_abb, "$"), ., ignore.case=T)) %>% 
            filter_at(vars(matches("properties_state")), any_vars(grepl(paste0(abbtostate(StateAbb),"|^",StateAbb, "$"), ., ignore.case=T)))
        }
        
        if(!is.nanull(CountryCode)){
          result_ <- result_ %>% filter_at(vars(matches("properties_country")), any_vars(. %in% CountryCode))
        }
        
        result_ %<>%
          select(matches(paste0(unique(c(return_which, "state|city|postcode|countrycode|name|county|street|coordinates")), collapse="|"))) %>% 
          group_by_at(vars(one_of("properties_city", "properties_state"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="down") %>% 
          group_by_at(vars(one_of("properties_name", "properties_state"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="down") %>%
          group_by_at(vars(one_of("properties_county", "properties_state"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="down") %>% 
          ungroup() %>% distinct()
        
        result_1 <- result_ %>% slice(1) %>% mutate_at(vars(one_of("geometry_coordinates")), function(v) as.character(paste0(unlist(v), collapse=", ")))
        if(verbose) {cat(URL, ":\n"); print(result_1 %>% data.frame() %>% setNames(gsub("properties_", "", names(.))))}
        
        if(return_which=="latlon"){
          final_result <- paste0(unlist(result_1$geometry_coordinates), collapse=", ")
        } else if(return_which=="county"){
          final_result <- result_ %>% drop_na(properties_county) %>% select(-matches("osm|country|district|street|type")) %>% 
            slice(1) %>%
            .$properties_county
        } else if(return_which=="everything"){
          final_result_a <- result_ %>% select(-matches("osm|district|type|country$")) %>% distinct()# %>% 
          # group_by_at(vars(one_of("properties_name"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="downup") %>% 
          # group_by_at(vars(one_of("properties_city"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="downup") %>% 
          # group_by_at(vars(one_of("properties_county"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="downup") %>% ungroup() %>% distinct()
          final_result <- final_result_a %>% slice(1:return_n_results) %>%
            group_by() %>%
            summarize_all(., function(v) paste0(v, collapse='; ')) %>% ungroup() %>%
            # mutate_all(., function(v) paste0(names(.), "::", paste0(v, collapse="; ")) %>% paste0(., collapse="; ")) %>%
            unlist() %>%
            paste0(names(.), "::", .) %>%
            paste0(., collapse=";\n;")
          # slice(1) %>%
          # .$properties_county
        } else if(return_which=="lonlatstate"){
          result_1 <- unite(result_1, "resultsvar", matches("properties_state|geometry_coordinates"), sep="; ")
          final_result <- paste0(unlist(result_1$resultsvar), collapse=", ")
        } else {
          final_result_a <- result_ %>% select(matches(return_which)) %>% distinct() %>% 
            filter_at(vars(matches(return_which)), any_vars(!is.na(.))) %>%
            mutate_at(vars(matches("properties_state")), function(v) srhoads::state2abb_or_abb2state(v, abb=T))
          
          # USA # https://www.infoplease.com/us/geography/extreme-points-united-states-50-states
          # MAX LON: -66 w
          # MIN LON: 172.27 e
          # 172.27 <= USA_LON <= -66.57
          # 18.55 <= USA_LAT <= 71.387
          
          if(grepl("zip|post", return_which)){
            city <- gsub("\\.", "", city) %>% gsub("^New York City$", "New York", ., ignore.case=T)
            final_results_best <- tryCatch({
              zipcodeR::search_city(tools::toTitleCase(city), state_abb) %>% select(matches(paste0(return_which, "|zipcode$|(lat|lng)$"))) %>% mutate_all(as.character) %>% distinct() %>%
                drop_na() %>%
                setNames(recode(names(.),
                                "major_city"="properties_city",
                                "zipcode"="properties_postcode",
                                "state"="properties_state",
                                "county"="properties_county"#,
                                # ""=""
                )) %>%
                # mutate(properties_countrycode = ifelse(as.numeric(lng)>=-172.27 & as.numeric(lng)<=-66.57 & as.numeric(lat)>=18.55 & as.numeric(lat)<=71.387, "US", NA),
                #        properties_name = final_result_decent[['properties_name']][1],
                #        properties_street = final_result_decent[['properties_street']][1]#,
                #        # properties_state = abbtostate(properties_state)
                #        ) %>% 
                unite(., "geometry_coordinates", c(matches("lng"), matches("lat")), sep=", ", na.rm=T) %>%
                select(matches("properties_|geometry_")) %>%
                # select(matches("properties_postcode$")) %>% drop_na() %>%
                mutate_at(vars(one_of('properties_postcode')), function(v) pad_leading_0s(v, 5)) # %>% .$zipcode %>% unique() %>% sort() %>%  paste0(., collapse=", ") %>% recode_na('')
            }, error=function(e) {NULL}) # summarize(state_puma = paste0(sort(unique(state_puma)), collapse=", "))#group_by(primary_city, state) %>% 
            
            if(!is.null(final_results_best)){
              final_result_a <- full_join(final_results_best, final_result_a, by="properties_postcode", suffix=c("", "_y")) %>%
                {
                  x <- .
                  x <- tryCatch({ mutate(x, geometry_coordinates = ifelse(!is.na(geometry_coordinates), geometry_coordinates, geometry_coordinates_y))}, error=function(e) x)
                  x <- tryCatch({ mutate(x,  properties_county = ifelse(!is.na(properties_county), properties_county, properties_county_y))}, error=function(e) x)
                  x <- tryCatch({ mutate(x, properties_state = ifelse(!is.na(properties_state), properties_state, properties_state_y))}, error=function(e) x)
                  x <- tryCatch({ mutate(x, properties_city = ifelse(!is.na(properties_city), properties_city, properties_city_y))}, error=function(e) x)
                  x
                } %>%
                select(-matches("_y$")) %>%
                distinct_at(vars(matches("city|state|county|postcode")), .keep_all=T)
              # unite(., "properties_county", matches("county"), sep="; ", na.rm=T) %>%
              # unite(., "properties_city", matches("city"), sep="; ", na.rm=T) %>% 
              # unite(., "properties_state", matches("state"), sep="; ", na.rm=T) %>% 
              # mutate_at(vars(matches("properties_(city|county|state)")), function(v)  unique_sep_sort(v, "; ")) %>%
              # mutate(properties_county = unique_sep_sort(properties_county, "; ")) %>%
              # distinct()
            }
          }
          # group_by_at(vars(one_of("properties_name"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="downup") %>% 
          # group_by_at(vars(one_of("properties_city"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="downup") %>% 
          # group_by_at(vars(one_of("properties_county"))) %>% fill(., matches("state|city|postcode|country|name|county|street|coordinates"), .direction="downup") %>% 
          # ungroup() %>% distinct()
          # # final_result <- final_result_a %>% slice(1:return_n_results) %>%
          # #   group_by() %>%
          # #   summarize_all(., function(v) paste0(v, collapse='; ')) %>% ungroup() %>%
          # #   unlist() %>%
          # #   paste0(names(.), "::", .) %>%
          # #   paste0(., collapse=";\n;")
          final_result <- final_result_a %>% slice(1:return_n_results) %>%
            jsonlite::toJSON()
        }
        
        # print(final_result)
        
        final_result %>% gsub("character\\(0\\)", "", .)  %>% recode_na("", "NA", "character(0)")
      },
      error=function(e){
        # NA
        structure("[]", class = "json")
      })
      return(result)
    })
    # }
  }
  
  
  recode_city_to_county <- function(city="Orange County", state_abb=NA, country_code="US", return_which="county") {
    result <- geocode_city(city=city, state_abb=state_abb, country_code=country_code, return_which=return_which)
    # result <- ifelse(return_which=="county"&result=="[]"&tolower(city)=="washington"&tolower(state_abb)=="dc", "Washington County", result)
    result
  }
  
  
  recode_county_name_to_fips_code <- function(county_name="Monterey", state="CA"){
    state_county <- paste0(state, ", ", county_name) %>% gsub(" County$", "", .)
    tryCatch({
      rerddap::fipscounty(state_county)
    }, error=function(e){
      NA
    })
  }
  
  
  
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
  
  
}
