rm(list=ls())
# #model <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
# 
# data(airquality)
# ozone <- subset(na.omit(airquality), 
#                 select = c("Ozone", "Solar.R", "Wind", "Temp"))
# set.seed(123)
# N.train <- ceiling(0.7 * nrow(ozone))
# N.test <- nrow(ozone) - N.train
# trainset <- sample(seq_len(nrow(ozone)), N.train)
# testset <- setdiff(seq_len(nrow(ozone)), trainset)
# 
# model.pois <- glm(Ozone ~ Solar.R + Temp + Wind, data = ozone, 
#                   family = "poisson", subset = trainset)
# summary(model.pois)
# 
# expected <- ozone$Ozone[trainset]
# g <- family(model.pois)$linkfun # log function
# g.inv <- family(model.pois)$linkinv # exp function
# estimates.log <- model.pois$linear.predictors # estimates on log scale
# estimates <- fitted(model.pois) # estimates on response scale (exponentiated)
# print(all.equal(g.inv(estimates.log), estimates))

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
devi=function(y,mu)
{
   res=0.0
   for(i in c(1:length(y)))
   {
      if(y[i]==0)
      {
        res=res+log(1/(1-mu[i]))
      }
      else
      {
        res=res+log(1/mu[i])
      }
   }
   return(2*res)
}

#可以计算pearson统计量
 pearson_stat=function(y,mu){
   res=0.0
   for(i in c(1:length(y))){
      fenzi=(y[i]-mu[i])^2
      fenmu=mu[i]*(1-mu[i])
      res=res+fenzi/fenmu
   }
   return(res)
 }

mu=model$fitted.values
y=candidates$admitted
print(devi(y,mu))
print(model$deviance)

print(pearson_stat(y,mu))
print(sum(residuals(model, type = "pearson")^2))

null_model=glm(formula = admitted~1,
               family = binomial(link = "logit"), 
               data = candidates)
print(null_model$deviance) #对应方差分析的各个数值




