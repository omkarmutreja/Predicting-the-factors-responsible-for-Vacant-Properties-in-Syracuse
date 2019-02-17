LatLong <- read.csv('LatLong.csv')
str(LatLong)

mainbackup <- LatLong
str(mainbackup)
View(mainbackup)
mainbackup$target <- 'NA'

mainbackup <- mainbackup[,-c(4,7)]
mainbackup_train <- mainbackup[1:2007,2:3]
mainbackup_test <- mainbackup[1:2373,4:5]  
mainbackup_train_target <- mainbackup[1:2007,1]  
mainbackup_test_target <- mainbackup[1:2373,6] 

sqrt(2007)  

library(class) #for knn
m1 <- knn(train=mainbackup_train,test=mainbackup_test,cl=mainbackup_train_target,k=48)

mainbackup$target <- m1

write.csv(mainbackup,'TargetExcel.csv')








