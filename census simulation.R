#==============================================================================================
#==============================================================================================
#This study creates population based on 2012 census. The proportion of crop farm, livestock 
#farm, and crop and livestock farm in the population amounts to that in 2012 census. 
#Same works for farms by value of sales, data source: 
#https://www.agcensus.usda.gov/Publications/2012/Full_Report/Volume_1,_Chapter_1_US/st99_1_001_001.pdf
#https://www.agcensus.usda.gov/Publications/2012/Full_Report/Volume_1,_Chapter_1_US/st99_1_051_052.pdf
#https://www.agcensus.usda.gov/Publications/2012/Full_Report/Volume_1,_Chapter_1_US/st99_1_064_064.pdf
library(dplyr)
library(magrittr)
#Procedures
#
#1.Generate population of 100,000 farms
#2.Randomly assign farms to be crops, livestock, or a mix of crops&livestock using proportions from 2012 census
#3. with each farm type, generate a size variable based on the proportions within each size categoty in 2012
#   census, select size at random within the selected category
#4. create the census sample by selecting each farm with PPS, sample size with 90,000
#5. crate the census sample by selecting each farm with PPS, sample size 30,000
#6. determine the overlap in step 4 and 5
#7. estimate the population using cap-recap model
#   a) ignoring type and size
#   b) ignoring type but not size
#   c) ignoring size but not type
#   d) considering both size and type
#8. repeat simulation for a 1000 times
#9. report mean, median, min, max, sd of 1000

num_total <- 2109303 #number of total farms
num_crop <- 1551654 #number of crop farms
num_livestock <- 913246 + 63246 + 198272 #number of livestock farms
num_croplive <- num_livestock + num_crop - num_total ##number of livestock crop farms overlap
num_crop_only <-  num_crop - num_croplive #number of only crop farms 
num_livestock_only <- num_livestock - num_croplive #number of only livestock  farms

num_less9 <- 223634#number of  farms size less than 9 acre
num_10to49 <- 589549 #number of farms size 10 to 49 acre
num_50to179 <- 634047 #number of farms size 50 to 179 acre
num_180to499 <- 346038 #number of farms size 180 to 499 acre
num_500to999 <- 142555 #number of farms size 500 to 999 acre
num_1000to1999 <- 91273 #number of farms size 1000 to 1999 acre
num_more2000 <- 82207#number of farms size more than 2000 acre


#check if sum of number of farms in different values amounts to that of total farms







#create population
set.seed(1000)

n <- 100000 #total number of farms
n_census <- 90000 #number of farms in census
n_JAS <- 30000 #number of farms in JAS
size_name <- c(num_less9, num_10to49, num_50to179, num_180to499, num_500to999, num_1000to1999,
               num_more2000)

#Encoding in farm type: farm_type
#  crop farm: 1
#  livestock farm: 2
#  Both: 3
#Encoding in farm value: farm_size
#  num_less9:    1
#  num_10to49:   2
#  num_50to179:     3
#  num_180to499:   4
#  num_500to999:   5
#  num_1000to1999:   6
#  num_more2000: 7

#farm number: farm_num 
#  from 1 to 100,000


#A table is crated to correspond value category to values
size_less9 <- 1:9
size_10to49 <- 10:49
size_50to179<- 50:179
size_180to499<- 180:499
size_500to999<- 500:999
size_1000to1999<- 1000:1999
size_more2000<- 2000:4000
value_size <- list(size_less9, size_10to49, size_50to179, size_180to499, size_500to999, size_1000to1999, size_more2000)










########STart of the program########################################
########################################################################
########################################################################
########################################################################

#result summary function
result_summary = function(obj){
  return (c(mean(obj), median(obj), min(obj), max(obj), sd(obj)))
}


#Create a dataframe farm_orig consisting of farm type and farm number






farm_num <- 1:n

farm_type <- rep(0, n)

farm_orig <- data.frame(farm_num)  #create population


##Randomly select farm to be in different types. The proportion mataches farm type proportion in 2012 census
#farm_orig[sample(n, ceiling(num_crop_only/num_total * n)),'farm_type'] <- 1
#1: crop_only
#2: livestock_only
#3: both crop_only and livestock_only

#random assign units to be in different types
c1 <- sample (1:n, ceiling(num_crop_only/num_total * n)) #crop 
c2 <- sample(setdiff(1:n, c1),ceiling(num_livestock_only/num_total *n)) #livestock
c3 <- setdiff(1:n, c(c1,c2)) #both crop and livestock

farm_orig<- farm_orig %>% mutate(farm_type = case_when(
  farm_num %in% c1 ~ 1,
  farm_num %in% c2 ~ 2,
  TRUE ~3
))


#Generate value categories based on value categories proportion for each farm type, i.e. crop farm, livestock farm
#and crop and livestock farm. Then merge farm_orig and value categories.



value_size_n <- length(value_size)

farm_size_crop <- sample(c(1:value_size_n), size=sum(farm_orig$farm_type == 1), replace = TRUE,
                               prob=size_name/num_total)
farm_size_livestock <- sample(c(1:value_size_n), size=sum(farm_orig$farm_type == 2), replace = TRUE,
                                    prob=size_name/num_total)
farm_size_croplive <- sample(c(1:value_size_n), size=sum(farm_orig$farm_type == 3), replace = TRUE,
                                   prob=size_name/num_total)
farm_size_cat <- c(farm_size_crop, farm_size_livestock, farm_size_croplive)

farm <- data.frame(farm_orig,farm_size_cat)

#select a value randomly from that category and replace the category with that value
for (i in 1:n){
  farm$farm_size[i] <-sample(value_size[[farm$farm_size_cat[i]]],1)
}

#farm <- farm %>% mutate(farm_type = sample(value_size[[farm$farm_size_cat[i]]],1))
#farm <- farm [order(farm$farm_num), ]
##============================================================================================
##============================================================================================
##============================================================================================
#Ignoring difference
#sc <- spark_connect(master = "local")
#using PPS to select census sample based on theor size for 1000 times
ptm <- proc.time()
overlap = {}
JAS_farm_total <- {}
census_farm_total <- {}
sim <- 1000
for (i in 1:sim){
  census_farm <- sample_n(farm,  n_census, replace = FALSE, weight = as.numeric(farm$farm_size))#select census sample
  JAS_farm <- sample_n(farm,  n_JAS, replace = FALSE, weight = as.numeric(farm$farm_size))#select JAS sample
  JAS_farm_total <- append(JAS_farm_total, list(JAS_farm))
  census_farm_total <- append(census_farm_total, list(census_farm))
  overlap_farm <- merge(census_farm, JAS_farm) #check overlap
  overlap <- append(overlap, list(overlap_farm))
}
#k <- k[[-1]]
proc.time() - ptm
#spark_disconnect(sc)

#1. Ignoring type&size difference
#calculate size 
size = 0
for (i in 1:1000) size =c(size, nrow(overlap[[i]])) 
size <- size[-1]


total_num_a <- 1 / (sum(size)/sim/n_JAS) * n_census
summary_a <- result_summary(total_num_a)

#2. Ignoring type but not size

size_overlap <- {}
size_census <- {}
size_jas <- {}

for (i in 1:1000) {
  size_overlap <- append(size_overlap, list(overlap[[i]] %>% count(farm_size_cat)  %>% select(n)))
  size_census <- append(size_census, list(census_farm_total[[i]] %>% count(farm_size_cat)  %>% select(n)))
  size_jas <- append(size_jas, list(JAS_farm_total[[i]] %>% count(farm_size_cat)  %>% select(n)))
}

total <- c()
for (i in 1:1000){
  total <- c(total, size_overlap[[i]]*size_census[[i]] / size_jas[[i]])
}

total_num_b <- unlist(lapply(total, sum,2))
summary_b <- result_summary(total_num_b)


#3. Ignoring size but not type


size_overlap <- {}
size_census <- {}
size_jas <- {}

for (i in 1:1000) {
  size_overlap <- append(size_overlap, list(overlap[[i]] %>% count(farm_type)  %>% select(n)))
  size_census <- append(size_census, list(census_farm_total[[i]] %>% count(farm_type)  %>% select(n)))
  size_jas <- append(size_jas, list(JAS_farm_total[[i]] %>% count(farm_type)  %>% select(n)))
}

total <- c()
for (i in 1:1000){
  total <- c(total, size_overlap[[i]]*size_census[[i]] / size_jas[[i]])
}

total_num_c <- unlist(lapply(total, sum,2))
summary_c <- result_summary(total_num_c)


#4. consider both type & size



size_overlap <- {}
size_census <- {}
size_jas <- {}

for (i in 1:1000) {
  size_overlap <- append(size_overlap, list(data.frame(overlap[[i]] %>% count(farm_type,farm_size_cat))  %>% select(n)))
  size_census <- append(size_census, list(data.frame(census_farm_total[[i]] %>% count(farm_type,farm_size_cat))  %>% select(n)))
  size_jas <- append(size_jas, list(data.frame(JAS_farm_total[[i]] %>% count(farm_type,farm_size_cat))  %>% select(n)))
}

total <- c()
for (i in 1:1000){
  total <- c(total, size_overlap[[i]]*size_census[[i]] / size_jas[[i]])
}

total_num_d <- unlist(lapply(total, sum,2))
summary_d <- result_summary(total_num_d)


summary <- rbind(summary_a, summary_b, summary_c, summary_d)