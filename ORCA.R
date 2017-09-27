library(dplyr)
library(magrittr)
#This program does two things: create a table for MC and C scroe, and then merge that with complete table

#1. Read 8 csv files into a dataframe in a list and create a column named "cat", indicating category of the obs,
#   eg. M_Contacts

#2. Rename 'A' and 'B' to 'M' or 'C' basing on which comes first 

#3. Merge all them together

#4. split variables and recode -99 as NA

#5. create 4 variables, 
#   miss_MC: missness of MC. 0: not missing, 1: missing
#   miss_C: missing of closed. 0: not missing, 1: missing
#   miss_pk: missing of pkscore 0: not missing, 1: missing
#6. create 9 variables: (web in MC is missing)

#   Total_Lscore_MC: total L score in MC
#   Total_Escore_MC: total E score in MC
#   Total_Sscore_MC: total S score in MC
#   Total_Cscore_MC: total C score in MC
#   Total_Lscore_C: total L score in C
#   Total_Escore_C: total E score in C
#   Total_Sscore_C: total S score in C
#   Total_Cscore_C: total C score in C
#   Total_Webscore_C: total Web score in C

#7. Read complete table and merge them tgt, crate a new variable miss_dmon
#   miss_dmon: 0: 0 of state ID, Gender, isELL, ISAH, School FRP is missing
#              1: 1 of state ID, Gender, isELL, ISAH, School FRP is missing
#              2: 2 of state ID, Gender, isELL, ISAH, School FRP is missing
#              3: 3 of state ID, Gender, isELL, ISAH, School FRP is missing
#              4: 4 of state ID, Gender, isELL, ISAH, School FRP is missing
#              5: 5 of state ID, Gender, isELL, ISAH, School FRP is missing
#   key: take M or C first. M: take M first, C: take C first
#   Topic_M:subject for M
#   Topic_C: subject for C

#8. reorder data based on 


################
################
################
####Start program






#1. Read 8 csv files into a dataframe in a list and create a column named "cat", indicating category of the obs,

setwd('//kcfsn02/Users/wei_yi/Desktop/nell work/ORCA/NEW ORCA/LESC data csv')

#replace section A,B with M, C depending on which exam comes first
substrRight_replace <- function(x,i){
  substr(x[substr(x, nchar(x), nchar(x))=='A'], nchar(x[substr(x, nchar(x), nchar(x))=='A']),
         nchar(x[substr(x, nchar(x), nchar(x))=='A'])) = substr(temp[i],1,1)
  substr(x[substr(x, nchar(x), nchar(x))=='B'], nchar(x[substr(x, nchar(x), nchar(x))=='B']),
         nchar(x[substr(x, nchar(x), nchar(x))=='B'])) = name[name!=substr(temp[i],1,1)]
  return (x)
}

temp <- list.files(pattern='*.csv') #read all csv files



#2. Rename 'A' and 'B' to 'M' or 'C' basing on which comes first 

name <- c('M', 'C')


df = c(rep(data.frame(), length(temp))) # list of empty dataframe
for (i in 1:length(temp)) { df[[i]] <- read.csv(temp[i]) }
for (i in 1:length(temp)) { 
  df[[i]]$cat <- rep(gsub(".csv","",temp[i]), dim(df[[i]])[1]) 
  names(df[[i]]) <- substrRight_replace(names(df[[i]]), i)
  df[[i]]$cat
}

#3. Merge all them together


df2 <- df[[1]]
for (i in 2:length(temp)) { 
  df2 <- rbind(df2,df[[i]])
}


#4. split variables and recode -99 as NA

pkc <- names(df2)[9:19] #"PK1SC"  "PK2SC"  "PK3SC"  "PK4SC"  "PK5SC"  "PK6SC"  "PK7SC"  "PK8SC"  "PK9SC"  "PK10SC" "PKTSC"
lc <- names(df2)[c(20:22,25)] #"LOC1SC" "LOC2SC" "LOC3SC" "LOC4SC"
ec <- names(df2)[26:29] # "EVA1SC" "EVA2SC" "EVA3SC" "EVA4SC"
sc <- names(df2)[c(30,31,34,35)] #"SYN1SC" "SYN2SC" "SYN3SC" "SYN4SC"
cc <- names(df2)[36:40] #"COM1SC"   "COM2SC"   "COM3SC"   "COM4SiC"  "COM4SiiC"
webc <- names(df2)[c(32,33)] #"WEBSITE2SC" "WEBSITE4SC"
between <- 35

pkm <- names(df2)[(9+between):(19+between)] #"PK1SM"  "PK2SM"  "PK3SM"  "PK4SM"  "PK5SM"  "PK6SM"  "PK7SM"  "PK8SM"  "PK9SM"  "PK10SM" "PKTSM" 
lm <- names(df2)[(20+between):(25+between)] #"LOC1SM"    "LOC2SM"    "LOC3SM"    "LOC4MC1SM" "LOC4MC2SM" "LOC4SM" 
em <- names(df2)[(26+between):(29+between)] #"EVA1SM" "EVA2SM" "EVA3SM" "EVA4SM"
sm <- names(df2)[c(30+between,31+between,34+between,35+between)] #"SYN1SM" "SYN2SM" "SYN3SM" "SYN4SM"
cm <- names(df2)[c((36+between):(38+between),(40+between))] #"COM1SM"   "COM2SM"   "COM3SM"   "COM4SiiM"

df2[df2==-99 ] <- NA #recode -99 to NA


#5. create 5 variables, 
#   miss_MC: missness of MC. 0: not missing, 1: missing
#   miss_C: missing of closed. 0: not missing, 1: missing
#   miss_pk: missing of pkscore 0: not missing, 1: missing

missness_MC <- is.na(df2[ ,c(pkc,lc, ec,sc,cc) ])
missness_pk <- is.na(df2[ ,c(pkc,pkm) ])
missness_C <- is.na(df2[ ,c(pkm,lm, em,sm,cm) ])

df2 <- df2 %>% mutate(miss_MC = ifelse(apply(missness_MC, 1, sum)==0, 0, 1),
                      miss_C = ifelse(apply(missness_C, 1, sum)==0, 0, 1),
                      miss_pk = ifelse(apply(missness_pk, 1, sum)==0, 0, 1)
                      )





#6. create 9 variables: (web in MC is missing)
#   Total_Lscore_MC: total L score in MC
#   Total_Escore_MC: total E score in MC
#   Total_Sscore_MC: total S score in MC
#   Total_Cscore_MC: total C score in MC
#   Total_Lscore_C: total L score in C
#   Total_Escore_C: total E score in C
#   Total_Sscore_C: total S score in C
#   Total_Cscore_C: total C score in C
#   Total_Webscore_C: total Web score in C

df2 <- df2 %>% mutate(Total_Lscore_MC = apply(df2[ ,c(lm)], 1, sum),
                      Total_Escore_MC = apply(df2[ ,c(em)], 1, sum),
                      Total_Sscore_MC = apply(df2[ ,c(sm)], 1, sum),
                      Total_Cscore_MC = apply(df2[ ,c(cm)], 1, sum),
                      Total_Lscore_C = apply(df2[ ,c(lc)], 1, sum),
                      Total_Escore_C = apply(df2[ ,c(ec)], 1, sum),
                      Total_Sscore_C = apply(df2[ ,c(sc)], 1, sum),
                      Total_Cscore_C = apply(df2[ ,c(cc)], 1, sum),
                      Total_Webscore_C = apply(df2[ ,c(webc)], 1, sum)
                      )




#7. Read complete table and merge them tgt, crate a new variable miss_dmon
#   miss_dmon: 0: 0 of state ID, Gender, isELL, ISAH, School FRP is missing
#              1: 1 of state ID, Gender, isELL, ISAH, School FRP is missing
#              2: 2 of state ID, Gender, isELL, ISAH, School FRP is missing
#              3: 3 of state ID, Gender, isELL, ISAH, School FRP is missing
#              4: 4 of state ID, Gender, isELL, ISAH, School FRP is missing
#              5: 5 of state ID, Gender, isELL, ISAH, School FRP is missing
#   key: take M or C first. M: take M first, C: take C first
#   Topic_M:subject for M
#   Topic_C: subject for C

com_time <- read.csv('//kcfsn02/Users/wei_yi/Desktop/nell work/ORCA/NEW ORCA/Complete_student_demoscore.csv')

#merge complete file and score file
df3 <- merge(df2, com_time, by = c('STUDENTID','LESCPAIR'), all.y = TRUE)

demon <- c("STATEID", "Gender", "IsELL", "LSAH", "SchoolFRPL")
missness_dom <- is.na(df3[ ,demon])
miss_num <- apply(missness_dom, 1, sum)
df3 <- df3 %>% mutate(miss_dmon = ifelse(miss_num==0, 0, 
                                         ifelse(miss_num==1, 1, ifelse(miss_num==2, 2,
                                                                       ifelse(miss_num==3, 3, ifelse(miss_num==4,4, 5))
                                                                       ))),
                      key = substr(cat,1,1),
                      Topic_M = ifelse(key == 'M', substr(LESCPAIR,1,1), substr(LESCPAIR,2,2)),
                      Topic_C = ifelse(key == 'C', substr(LESCPAIR,1,1), substr(LESCPAIR,2,2))
                      )

#8. reorder data based on 
df4 <- df3 %>% select (miss_MC, miss_C, miss_pk, miss_dmon, Gender, STUDENTID, STATEID,
                       SCHOOLID, Gender, IsELL, LSAH, SchoolFRPL, SchoolDRG, StudToCompRatio, PercCompInet,
                       LESCPAIR, key, PKTSM, Topic_M, Total_Lscore_MC, Total_Escore_MC, 
                       Total_Sscore_MC, Total_Cscore_MC, mc_Total_L, mc_Total_S, mc_Total_E, 
                       PKTSC, Topic_C, Total_Lscore_C, Total_Escore_C, 
                       Total_Sscore_C, Total_Cscore_C, close_Total_L, close_Total_S, close_Total_E)


write.csv(head(df4), '//kcfsn02/Users/wei_yi/Desktop/nell work/ORCA/NEW ORCA/complete_score.csv')
#seelcted variables

