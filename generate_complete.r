dat <- read.csv('D:/ORCA/master.csv')
head(dat)

total_scor_name <- c('TOTAL_LSCORE_MC', 'TOTAL_ESCORE_MC', 'TOTAL_SSCORE_MC', 'TOTAL_CSCORE_MC', 'TOTAL_LSCORE_C', 'TOTAL_ESCORE_C',
                     'TOTAL_SSCORE_C', 'TOTAL_CSCORE_C')
total_time_name <- c('MC_TOTAL_L', 'MC_TOTAL_S', 'MC_TOTAL_E', 'CLOSE_TOTAL_L', 'CLOSE_TOTAL_S', 'CLOSE_TOTAL_E')


#complete in MC and Closed 
complete <- dat [apply(is.na(dat [, c(total_scor_name, total_time_name)]),1, sum) ==0,]
complete[complete==-99]  <- NA
write.csv(complete, 'D:/ORCA/complete.csv', na='')
