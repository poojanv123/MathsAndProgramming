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

 out5<- aggregate(df3$stay,by= list(df3$run, df3$number_of_beds,df3$bed_no),mean )
 out6 <- aggregate(out5$x, by = list(out5$Group.2, out5$Group.3),mean)
 plot(out5[1,], xlab = "Day of the Week", ylab = "Number of Patients")
 axis(side=1, at=seq(1,10, by=1))
 axis(side=2, at=seq(5, 30, by=10))
 par(mfrow=c(3,3))
plot.new() 
 for(i in 1:10)
 {
   plot(out6[which(out6$Group.1 == i),2:3])
 }
df3 <- read.csv("E:/Study/Maths and programming/pset3_output_final_v6.csv")
df_test <- subset(df3, bed_no != 0)
out1 <- table(df_test$run, df_test$number_of_beds)
put1<- colMeans(out1)
out2 <- table(df3$run, df3$number_of_beds)
put2<- colMeans(out2)
colnames(df3)
p_with_bed <- df3 %>% group_by(number_of_beds,run) %>% summarise(count1 = sum(bed_no != 0))
out1 <- tapply(p_with_bed$count1, p_with_bed$number_of_beds, mean)
pat_count <- df3 %>% group_by(number_of_beds,run) %>% summarise(count2 = sum(user_no>0))
out2 <- tapply(pat_count$count2, pat_count$number_of_beds, mean)
out <-rbind(put1,put2)
rownames(out) = c("Average Patients with bed", "Total Average Patients")
par(mfrow=c(3, 3), mar=c(5, 5, 4, 2) + 0.1, xpd = TRUE)
barplot(out,ylim = c(0,20), beside = TRUE, main = "Average Number of Patients with Bed", xlab = "No. of Maximum Beds", ylab = "No. of Patients", legend = rownames(out), args.legend = list(x = "bottomleft", bty = "n", inset=c(-0.1, -0.5)))

pat_count <- df3 %>% group_by(day,run) %>% summarise(count2 = sum(user_no>0))
out2 <- tapply(pat_count$count2, pat_count$number_of_beds, mean)

pat_count <- df3 %>% group_by(df3$number_of_beds,df3$run) %>% summarise(count2 = sum(user_no>0))
out3 <- tapply(pat_count$count2,df3$ pat_count$number_of_beds, mean)
plot(out3,main = "Average number of Patients Per Day", xlab = "Day of the week", ylab = "Average number of Patients")
lines(out3)
df3[which(df3$stay == 0),5] = NA
out4 <- tapply(df3$stay, df3$number_of_beds)
hist(df3$stay, xlab = "Length of Stay (in hours)", main = "Length of Stay Distribution")
df4 = df[which(df3$user_no > 1),]
head(df4)
mean(df3$stay, na.rm = T)
hist(df3$diff_bet_patient_intime, xlab = "Difference between in-time of two patients (in hours)", main = "Difference between in-time of Patients Distribution")
out3 <- tapply(df3$stay, df3$bed_no, df3$number_of_beds, mean)
out5 <- aggregate(df3$stay,by= list(df3$bed_no, df3$number_of_beds), mean)

out7 <- aggregate(df3$stay,by= list(df3$run, df3$number_of_beds, df3$bed_no), mean, na.rm = T)
out7 <- out7[which(out7$Group.3 > 0),]
out8 <- aggregate(out7$x,by= list(out7$Group.1, out7$Group.3), mean)
ggplot(out8, aes(x =out8$x, y = out8$Group.2, color= out8$Group.1))
write.csv(out7,"test.csv")
out5 <- table(df3$run, df3$day)
ggplot(out8, aes(x=Group.2, y=x, color=Group.1)) + 
  labs(title = "Small World plot", y ="", color = "") + 
  geom_point() + scale_x_log10() + theme(plot.title = element_text(hjust = 0.5))

plot(out5[1,], ylim = c(0,40), xlab = "Day of the Week", ylab = "Number of Patients")
axis(side=1, at=seq(1,10, by=1))
axis(side=2, at=seq(5, 30, by=10))
for(i in 1:100)
{
lines(out5[i,])
}
out6 =data.frame(out5)