library(tidyr)
library()
fire_data <- read.csv("fire_counts_2017.csv", stringsAsFactors = FALSE)

#extract street number
street_split <- as.data.frame(stringr::str_split_fixed(fire_data$Address, " ", n = 2))
street_split$V1 <- as.numeric(street_split$V1)
colnames(street_split) <- c("street_num", "address")

#upgrade street level to block level
street_split$block <- floor(street_split$street_num/100)*100

#regulate expression of address
street_split$address <- gsub("\\s?&.*$","",street_split$address)
street_split$address <- gsub("AVE$","AV",street_split$address)
street_split$address <- gsub("GRN$","GREEN",street_split$address)
street_split$address <- gsub(" rear.*$","",street_split$address)
street_split$address <- gsub(" to .*$","",street_split$address)
street_split$address <- gsub(" #.*$","",street_split$address)

#put s/e/n/w to end of the address
street_split$address <-tolower(street_split$address)
South<-unlist(gregexpr("^s ",street_split$address))
South<-as.numeric(gsub(-1,0,South))
street_split$address <-gsub("^s ","",street_split$address)
street_split$address[South==TRUE]<-paste(street_split$address[South==TRUE],"s")

East <-unlist(gregexpr("^e ",street_split$address))
East <-as.numeric(gsub(-1,0,East))
street_split$address <-gsub("^e ","",street_split$address)
street_split$address[East==TRUE]<-paste(street_split$address[East==TRUE],"e")

North <-unlist(gregexpr("^n ",street_split$address))
North <-as.numeric(gsub(-1,0,North))
street_split$address <-gsub("^n ","",street_split$address)
street_split$address[North==TRUE]<-paste(street_split$address[North==TRUE],"n")

West <-unlist(gregexpr("^e ",street_split$address))
West <-as.numeric(gsub(-1,0,West))
street_split$address <-gsub("^w ","",street_split$address)
street_split$address[West==TRUE]<-paste(street_split$address[West==TRUE],"w")

#cancantenate block address
street_split$block <- paste(as.character(street_split$block), "block")
street_split$address <- paste(street_split$block, street_split$address, sep = " ")

#renew address in fire data
fire_data$Address <- street_split$address

#count frequency of each block
count_num <- aggregate(Fire.alarm ~ Address, fire_data,FUN = "sum")

write.csv(count_num, "firedata.csv")