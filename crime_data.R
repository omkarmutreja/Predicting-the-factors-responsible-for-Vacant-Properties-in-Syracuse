library(data.table)

crime_table <- read.csv("crime2017.csv", stringsAsFactors = FALSE)

colnames(crime_table[1]) <- c("Crime_address")

crime_table$police_call_block <- gsub("\xff","",crime_table$police_call_block)
crime_table$police_call_block <- gsub(" Syracuse$", "", crime_table$police_call_block)
crime_table$police_call_block <- gsub(" @ ", " ", crime_table$police_call_block)
crime_table$police_call_block <- gsub("^To Eb I 690 Ramp$", " ", crime_table$police_call_block)
#delete destiny results
# crime_data <- police_call_syr[!grepl("Unknown", police_call_syr$Crime_address),]
# crime_data <- crime.data[!grepl("Destiny Usa", crime.data$Crime_address),]

crime_table$police_call_block <- tolower(crime_table$police_call_block)

#regulate the n/w/e/n
#10==False
South<-unlist(gregexpr(" s ",crime_table$police_call_block))
South<-as.numeric(South)
crime_table$police_call_block <-gsub(" s "," ",crime_table$police_call_block)
crime_table$police_call_block[South>0]<-paste(crime_table$police_call_block[South>0],"s")

North <-unlist(gregexpr(" n ",crime_table$police_call_block))
North <-as.numeric(North)
crime_table$police_call_block<-gsub(" n "," ",crime_table$police_call_block)
crime_table$police_call_block[North>0]<-paste(crime_table$police_call_block[North>0],"n")

West <-unlist(gregexpr(" w ",crime_table$police_call_block))
West <-as.numeric(West)
crime_table$police_call_block <-gsub(" w "," ",crime_table$police_call_block)
crime_table$police_call_block[West>0]<-paste(crime_table[West>0],"w")

East <-unlist(gregexpr(" e ",crime_table$police_call_block))
East <-as.numeric(East)
crime_table$police_call_block <-gsub(" e "," ",crime_table$police_call_block)
crime_table$police_call_block[East>0]<-paste(crime_table$police_call_block[East>0],"e")

#merge fire_data and crime_data
fire_crime <-merge(count_num, crime_table, by="Address")

colnames(crime_table)[colnames(crime_table)=="police_call_block"] <- "Address"
fire_crime <-merge(count_num, crime_table, by="Address")
fire_crime <- data.table(fire_crime)
fire_crime[lapply()]

fire_crime$Fire.alarm[is.na(fire_crime$Fire.alarm)] <- 0
fire_crime$Aggravated_assault[is.na(fire_crime$Aggravated_assault)] <- 0
fire_crime$Arson[is.na(fire_crime$Arson)] <- 0
fire_crime$Burglary[is.na(fire_crime$Burglary)] <- 0
fire_crime$Larceny[is.na(fire_crime$Larceny)] <- 0
fire_crime$Robbery[is.na(fire_crime$Robbery)] <- 0
fire_crime$vehicle_theft[is.na(fire_crime$vehicle_theft)] <- 0

write.csv(crime_table, "crimedata.csv")


