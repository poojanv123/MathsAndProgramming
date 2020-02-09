#Objective: Find the optimum number of beds required for hospital

#Logic for assigning bed:
#1. Everytime a new customer comes, check if its intime is after out-time of
# all the previous patients
  #If yes, then check if the bed is available
    #If yes, assign that bed, if no, check the next previous patient until an available bed is found
  #If in-time is before out-time of previous patient, assign the next available bed
#rm(list=ls())


library(lubridate)
library(truncnorm)
library(ggplot2)
library(dplyr)


daytime <- seq(from=as.POSIXct("2020-01-05 00:00", tz="UTC"), 
               to=as.POSIXct("2020-01-11 23:00", tz="UTC"), by="hour")

n_pcs = 10
n_run = 100
df2 = list()
df3 = list()
df3 <- data.frame(matrix(ncol = 8, nrow = 0)) #Created dataframe of results

for(j in 1:n_pcs)
{
  df2 <- data.frame(matrix(ncol = 8, nrow = 0)) #Created dataframe of results
  for(l in 1:n_run)
  {
df <- data.frame(matrix(ncol = 8, nrow = 0)) #Created dataframe of results
no_of_users=1
while(no_of_users==1){ no_of_users= rpois(1,20)} #Poisson distribution of patients with mean 10
diff_bn_user_intime = round(rtruncnorm(n=no_of_users, a=0, b=15, mean=10, sd=5),2)
users_stay_norm = round(rnorm(no_of_users, 36,8),1) #Normal distribution of patient intime
df[1,1:4] <- c(1,1,sample(daytime[1:5],2), round(users_stay_norm,1)[1])
df[1,5] <- df[1,3] + df[1,4]*3600
last_user_intime = df[1,3]
df[1,6] = l
df[1,7] = j
df[1,8] = 0
pcs_occupied = 1
  for(k in seq(2,no_of_users))
  {
    
    df[k,1] = k #Patient number
    df[k,8] = round(diff_bn_user_intime,1)[k]
    df[k,3] = last_user_intime + df[k,8]*3600  #Patient intime
    df[k,6] = l
    df[k,7] = j
    if(pcs_occupied <= j)
    {
      df[k,4] = round(users_stay_norm[k],1) #Patient stay
      df[k,5] = df[k,3] + df[k,4]*3600 #Patient outime
      if(k==2 & df[k,3] >= df[1,5]) 
      {
        df[k,2] = 1
      }
      else if(k==2 & df[k,3] < df[1,5] & pcs_occupied < j)
      {
        df[k,2] = 2
        pcs_occupied = pcs_occupied + 1
      }
      else if(k==2 & df[k,3] < df[1,5] & pcs_occupied == j)
      {
        df[k,2] = 0
        df[k,4] = 0
        df[k,5] = 0
      }
      else
      { 
         for(i in seq(1,k-1))
        {
          if(df[k,3] >= df[i,5] & !(df[i,2] %in% df[(i+1):(k-1),2]))
          {
            df[k,2] <- df[i,2]
            break
          }
         }
        if(is.na(df[k,2]) & pcs_occupied < j )
        {
          df[k,2] = pcs_occupied+1
          pcs_occupied = pcs_occupied + 1
        }
        else if(is.na(df[k,2]) & pcs_occupied == j)
        {
          df[k,2] = 0
          df[k,4] = 0
          df[k,5] = 0
        }
      }
    last_user_intime = df[k,3]
    }
    else
    {
      break
    }
  }
df2 = rbind.data.frame(df2,df)
  }
  df3 = rbind.data.frame(df3,df2)
}

df3[,3] = as.POSIXct(df3[,3], origin = "1970-01-01 00:00",  tz="UTC")
df3[,5] = as.POSIXct(df3[,5], origin = "1970-01-01 00:00",  tz="UTC")
df3[,9] = day(df3[,3])
df3 = df3[which(df3[,9] < 12),]
colnames(df3) <- c("user_no", "bed_no", "intime", "stay", "outtime","run", "number_of_beds", "diff_bet_patient_intime","day")
colnames(df2) <- c("user_no", "bed_no", "intime", "stay", "outtime","run", "number_of_beds", "diff_bet_patient_intime")

write.csv(df3, "E:/Study/Maths and programming/pset3_output_final_v6.csv")
