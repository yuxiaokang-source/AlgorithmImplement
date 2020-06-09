# rm(list=ls())
# setwd("C:\\Users\\29774\\Desktop\\6月9日\\irls-in-machine-learning-master\\")
# X =read.table("input.dat",header=F)
# Y =read.table("output.dat",header=F)
# 
# data=cbind(X,Y)
# colnames(data)=c("x1","x2","y")
# model <- glm(formula = y~.,
#            family = binomial(link = "logit"), 
#            data = data)
# 
# print(model$coefficients)
rm(list=ls())
candidates =data.frame(gmat=c(780,750,690,710,680,730,690,720,740,690,610,690,710,680,770,610,580,650,540,590,620,600,550,550,570,670,660,580,650,660,640,620,660,660,680,650,670,580,590,690),
                       gpa= c(4,3.9,3.3,3.7,3.9,3.7,2.3,3.3,3.3,1.7,2.7,3.7,3.7,3.3,3.3,3,2.7,3.7,2.7,2.3,3.3,2,2.3,2.7,3,3.3,3.7,2.3,3.7,3.3,3,2.7,4,3.3,3.3,2.3,2.7,3.3,1.7,3.7),
                       work_experience=c(3,4,3,5,4,6,1,4,5,1,3,5,6,4,3,1,4,6,2,3,2,1,4,1,2,6,4,2,6,5,1,2,4,6,5,1,2,1,4,5),
                       admitted= c(1,1,0,1,0,1,0,1,1,0,0,1,1,0,1,0,0,1,0,0,1,0,0,0,0,1,1,0,1,1,0,0,1,1,1,0,0,0,0,1))


model <- glm(formula = admitted~.,
             family = binomial(link = "logit"), 
             data = candidates)

print(model$coefficients)
