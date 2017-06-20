library(zoo)
library(plyr)
library(MASS)
library(leaps)
library(dummies)
library(zoo)
library(plyr)
library(MASS)
library(leaps)
library(mlogit)
install.packages("cluster")

library(cluster)
import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE,strip.white=TRUE))
}

write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)
}


###########################--------Neural net predictions-----##########

install.packages("nnet")
library(nnet)

##pre-processing of data####

filtered_data =data.frame(import.csv('Property Score_v3.csv'))
fdata=filtered_data[,c("price","sqft","bedrooms","bathrooms","parking",
                       "heating","basement","attic","Age_in_Years","price_per_sqft",
                       "Seller_Rating","property_score"
)]
fdata=fdata[complete.cases(fdata),]

#####----nnet multinomial prediction---###
get_pred_nnet <- function(train,test){
  model= multinom(property_score~bedrooms+bathrooms+parking+
                    heating+basement+attic+Age_in_Years+price_per_sqft+
                    Seller_Rating,data=fdata,MaxNWts=6700)
  prediction=factor(predict(model,test))
  return(prediction)
}

#########---n fold cross validation-----#########
####----Training the model and checking accuracy------#####
do_cv=function(df,num_folds)
{
  df=df[sample(nrow(df)),]
  folds=cut(seq(1,nrow(df)),breaks=num_folds,labels=FALSE)
  output_new=data.frame()
  
  for(i in 1:num_folds){
    print("entering loop")
    index <- which(folds==i,arr.ind=TRUE)
    test <- df[index, ]
    print(dim(test))
    train <- df[-index, ]
    print(dim(train))
    output=get_pred_nnet(train,test)
    output=factor(output)
    print(typeof(output))
    output_new=rbind(output_new,factor(output))
    print(output_new)
  }
  return(output_new)
}

###########----cv_function call-------###########
cv=do_cv(fdata,10,test)

######----classification error-----##########
classification_error=table(class_predictions,fdata$property_score)
print(classification_error)

####################-----predictions of new samples-----######


############################################
###-----CSS code for new data point----####
###########################################

property_score=function(datapoint)
{
  pr_score=get_pred_nnet(fdata,datapoint)
  datapoint=cbind(datapoint,pr_score)
  fdata=rbind(fdata,datapoint)
  sorted_fdata=fdata[order(-property_score),]
  return(sorted_fdata)
}




#######################----Clustering---######################
####################################
mydata <-import.csv('With_SR.csv')
mydata$zip<- NULL
mydata$property.age<- NULL
mydata[is.na(mydata)]<- -1
return.data<- na.locf(mydata)
return.data <- coredata(return.data) #data




###############------PCA-------#######

obj=prcomp(mydata, retx = TRUE, center = TRUE, scale = FALSE)
screeplot(obj, type = 'lines')
obj$sdev

plot(cumsum(obj$sdev^2/sum(obj$sdev^2)),xlab='Number of variables',ylab = 'Magnitude')
topN=obj$sdev[1:10]
biplot(obj)


##########################################


##K-means
#jmydata=mydata[,-1]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#mydatanew=data.frame(mydata$ld_diff_lv,mydata$accum_subs,mydata$pages_per_visit)
fit <- kmeans(mydata, 8,nstart = 20) # 8 cluster solution
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
s1=aggregate(mydata,by=list(fit$cluster),FUN=mean)
write.csv(s1,"allclusters.csv")
summarize(s1)
# append cluster assignment

mydata <- data.frame(mydata, fit$cluster)

#aggregate(mydata$fit.cluster ~ ., mydata, function(x) length(unique(x)))
table(mydata$fit.cluster)


cluster1= subset(mydata,mydata$fit.cluster==1)
cluster2= subset(mydata,mydata$fit.cluster==2)
cluster3= subset(mydata,mydata$fit.cluster==3)
cluster4= subset(mydata,mydata$fit.cluster==4)
write.csv(cluster1,"Cluster1.csv")
write.csv(cluster2,"Cluster2.csv")
write.csv(cluster3,"Cluster3.csv")
write.csv(cluster4,"Cluster4.csv")










