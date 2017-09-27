#install.packages('readxl')
#install.packages('sqldf')

library(dplyr)
library(magrittr)
library(readxl)
library(sqldf)

setwd('D:/ORCA/Frank_2014/Timestamps')
temp <- list.files(pattern='*.xlsx') #read all csv files



#2. Rename 'A' and 'B' to 'M' or 'C' basing on which comes first 

name <- c('M', 'C')

substrRight_replace <- function(x,i){
  substr(x[substr(x, nchar(x), nchar(x))=='A'], nchar(x[substr(x, nchar(x), nchar(x))=='A']),
         nchar(x[substr(x, nchar(x), nchar(x))=='A'])) = substr(temp[i],12,12)
  substr(x[substr(x, nchar(x), nchar(x))=='B'], nchar(x[substr(x, nchar(x), nchar(x))=='B']),
         nchar(x[substr(x, nchar(x), nchar(x))=='B'])) = name[name!=substr(temp[i],12,12)]
  return (x)
}

#choose smallest positive difference
diff_smallest_p <- function(object, pop){
  dif <- pop - object
  dif_smallest_p <- min(dif[dif>0])
  #dif_smallest_p <- ifelse(!if(any(is.na(pop))), min(dif[dif>0]), NA)
  return (dif_smallest_p)
}

substrRight <- function(x,i){
  return (substr(x, nchar(x)-i+1,nchar(x)))
}



df = c(rep(data.frame(), length(temp))) # list of empty dataframe
for (i in 1:length(temp)) { df[[i]] <- as.data.frame( read_excel(temp[i])) }
for (i in 1:length(temp)) { 
  df[[i]]$cat <- rep(gsub(".xlsx","",temp[i]), dim(df[[i]])[1]) 
  names(df[[i]]) <- substrRight_replace(names(df[[i]]), i)
}



#3. Merge all them together


df2 <- df[[1]]
for (i in 2:length(temp)) { 
  df2 <- rbind(df2,df[[i]])
}

df2[,'TIME_STARTM'] = 0  #set variable TIME_STARTM to 0

C_name <- names(df2)[5:17]
M_name <- names(df2)[20:32]
com_name <- c(C_name, M_name)

# if one of web, E,S, L is NA, set all of them to NA
for (i in 1:dim(df2)[1]) {if (any(is.na(df2[i,c(C_name, M_name)]))) df2[i,c(C_name, M_name)] = NA} 




for (i in 1:dim(df2)[1]){
  for (j in c(5:17, 20:32)){
    if (!is.na(df2[ i, j]) & nchar(df2[ i, j])>15) {
      df2[ i,j] = substrRight(df2[ i, j],10)
      df2[ i, j] = 60 * 60 * as.numeric(substr(df2[ i, j],1,2)) +
      60 * as.numeric(substr(df2[ i, j],4,5)) + as.numeric(substr(df2[ i, j],7,10))
    }
    if (!is.na(df2[ i, j]) & nchar(df2[ i, j])== 7) df2[ i, j] = 60 * as.numeric(substr(df2[ i, j], 1,2)) +
        as.numeric(substr(df2[ i, j], 4,7))
  }
}

#using sapply to simplify code
#convert time to numeric
df2[,com_name] <- sapply(df2[,com_name], as.numeric)

df3 <- df2

new <- {}
for (i in 1:dim(df2)[1]) {
     for (j in c(6:17)) new = c(new, diff_smallest_p(df2[i,c(5:17)],df2[i,j]))
     for (j in c(21:32)) new = c(new, diff_smallest_p(df2[i,c(20:32)],df2[i,j]))
}

df3[,c(6:17,21:32)] <- matrix(new,nrow = dim(df2)[1],byrow = T)


df3 <- df3 %>% mutate(close_Total_L = TIME_LOC1RC+ TIME_LOC2RC + TIME_LOC3RC,
                      close_Total_S = TIME_SYN1RC+ TIME_SYN2RC + TIME_SYN3RC + TIME_SYN4RC,
                      close_Total_E = TIME_EVA1RC+ TIME_EVA2RC + TIME_EVA3RC + TIME_EVA4RC, 
                      mc_Total_L = TIME_LOC1RM+ TIME_LOC2RM + TIME_LOC3RM,
                      mc_Total_S = TIME_SYN1RM+ TIME_SYN2RM + TIME_SYN3RM + TIME_SYN4RM,
                      mc_Total_E = TIME_EVA1RM+ TIME_EVA2RM + TIME_EVA3RM + TIME_EVA4RM,
                      key = substr())

Timestamp_all <- df3
write.csv(df3, 'D:/ORCA/timestamp.csv',na='')