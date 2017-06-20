##Seller side code
library(zoo)
library(plyr)
library(MASS)
library(leaps)
#library(dummies)
#library(mlogit)
#library(cluster)
library(nnet)

import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE,strip.white=TRUE))
}

write.csv <- function(ob,filename){
  write.table(ob, filename, quote=FALSE, sep=",", row.names=FALSE)
}

pr_score=0
get_pred_nnet <- function(train,test){
  model= multinom(property_score~bedrooms+bathrooms+parking+
                    heating+basement+attic+age_in_years+price_per_sqft+
                    seller_rating,data=train,MaxNWts=6700)
  prediction=predict(model,test)
  return(prediction)
}

property_score=function(datapoint, fdata,or_df)
{
  pr_score=get_pred_nnet(fdata,datapoint)
  datapoint=cbind(or_df,property_score=as.integer(as.numeric(pr_score)))
  #datapoint=cbind(orig_df.new.prop,property_score=4)
  updated_df=data.frame(import.csv('~/Seller_PropScore/data/Property Score_v4.csv'))
  #fdata=rbind(fdata,datapoint)
  updated_df$property_score=as.integer(as.numeric(updated_df$property_score))
  updated_df=rbind(updated_df,datapoint)
  sorted_fdata=updated_df[order(-updated_df$property_score),]
  return(sorted_fdata)
}

produce_result <- function(df.new.prop,orig_df.new.prop){
  #ppsf = df.new.prop$price/df.new.prop$sqft
  output=df.new.prop[,c("bedrooms","bathrooms","parking",
                        "heating","basement","attic","age_in_years","price_per_sqft",
                        "seller_rating")]
  #query <- data.frame(df.new.prop$bedrooms, df.new.prop$bathrooms,df.new.prop$parking, df.new.prop$heating, 
   #                     df.new.prop$basement,df.new.prop$attic, df.new.prop$age,ppsf,5)
  #query=df.new.prop[,c("bedrooms","bathrooms","parking",
   #                      "heating","basement","attic","age_in_years","price_per_sqft",
    #                     "seller_rating","property_score")]
  filtered_data =data.frame(import.csv('~/Seller_PropScore/data/Property Score_v4.csv'))
  filtered_data$price = gsub(",","",filtered_data$price)
  filtered_data$price = gsub("\\$","",filtered_data$price)
  filtered_data$price_per_sqft = gsub(",","",filtered_data$price_per_sqft)
  filtered_data$price_per_sqft = gsub("\\$","",filtered_data$price_per_sqft)
  filtered_data$sqft = gsub(",","",filtered_data$sqft)
  filtered_data=filtered_data[complete.cases(filtered_data),]
  filtered_data[is.na(filtered_data)]=0
  filtered_data$price = as.numeric(filtered_data$price)
  filtered_data$sqft = as.numeric(filtered_data$sqft)
  filtered_data$price_per_sqft = as.numeric(filtered_data$price_per_sqft)
  
  
  fdata=filtered_data[,c("bedrooms","bathrooms","parking",
                         "heating","basement","attic","age_in_years","price_per_sqft",
                         "seller_rating","property_score"
                      )]
  fdata=fdata[complete.cases(fdata),]
  or=data.frame(orig_df.new.prop)
  sorted_fdata = property_score(output, fdata,or)
  return(sorted_fdata)
}
