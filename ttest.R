setwd('E:/dropbox/Dropbox/projects/phd/codes/matlab/fm_fi/t test comparison')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/matlab/fm_fi/t test comparison')
#library("formattable")


# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

# data<-read.csv("result_comparison_temporary.csv", header = TRUE)
# ttest_output<-rbind()
# for (i in 1:nrow(data)){
#   rttest<-cbind()
#   acc<-max(data[i,1:4])
#   pos<-which.max(data[i,1:4])
#   stdv<-data[i,pos+4]
#   for (j in 1:4){
#     rttest<-cbind(rttest,t.test2( acc, data[i,j], stdv, data[i,j+4], 100, 100))
#   }
#   ttest_output<-rbind(ttest_output,rttest)
# }

t.test2( 97.15, 96.47, 1.24, 1.91, 100, 100)

file_names_real <- c("acc_adaboost_real_csv.csv", "acc_bagging_real_csv.csv", "acc_DeFIMKL_real_csv.csv", "acc_DFRAV_real_csv.csv",
                     "acc_majSVM_real_csv.csv", "acc_random_forest_real_csv.csv")
file_names_standard <- c("acc_adaboost_standard_csv.csv", "acc_bagging_standard_csv.csv", "acc_DeFIMKL_standard_csv.csv", "acc_DFRAV_standard_csv.csv",
                         "acc_majSVM_standard_csv.csv", "acc_random_forest_standard_csv.csv", "acc_DeFIMKL_adaboost_40_standard_csv.csv", 
                         "acc_DeFIMKL_adaboost_100_standard_csv.csv", "acc_DeFIMKL_adaboost_200_standard_csv.csv")

acc_real <- cbind()
acc_stdv_real <- cbind()
time_real <- cbind()
for (i in 1:length(file_names_real)){
  data <- read.csv(file_names_real[i],header = TRUE)
  acc_real <- cbind(acc_real,data[c(1:4),2])
  acc_stdv_real <- cbind(acc_stdv_real,data[c(1:4),3])
  time_real <- cbind(time_real,data[c(1:4),10])
}

acc_standard <- cbind()
acc_stdv_standard <- cbind()
time_standard <- cbind()
for (i in 1:length(file_names_standard)){
  data <- read.csv(file_names_standard[i],header = TRUE)
  acc_standard <- cbind(acc_standard,data[c(1:14),2])
  acc_stdv_standard <- cbind(acc_stdv_standard,data[c(1:14),3])
  time_standard <- cbind(time_standard,data[c(1:14),10])
}

acc_data <- acc_real*100
acc_stdv_data<-acc_stdv_real*100
acc_data <- acc_standard*100
acc_stdv_data<-acc_stdv_standard*100
pvalue <- rbind()
for (i in 1:nrow(acc_data)){
  temp_acc_max <- max(acc_data[i,])
  temp_which_acc_max <- which.max(acc_data[i,])
  temp_stdv_max <- acc_stdv_data[i, temp_which_acc_max]
  temp_pvalue <- cbind()
  for (j in 1:ncol(acc_data)){
    temp <- t.test2(acc_data[i,j], temp_acc_max, acc_stdv_data[i,j], temp_stdv_max, 100, 100)
    temp_pvalue <- cbind(temp_pvalue,temp[4])
  }
  pvalue <- rbind(pvalue, temp_pvalue)
}

pp<-matrix(0,nrow(pvalue),ncol(pvalue))
for (i in 1:nrow(pvalue)){
  for (j in 1:ncol(pvalue)){
    if (pvalue[i,j]>0.05)
      pp[i,j]<-1
  }
}

#write.csv(pvalue,"pvalues.csv")
#write.csv(acc_data,"acc_data.csv")
print_data<-matrix(0,nrow(acc_data),ncol(acc_stdv_data))
for (i in 1:nrow(acc_data)){
  for (j in 1:ncol(acc_data)){
    print_data[i,j]<-paste(round(acc_data[i,j],2),'(', round(acc_stdv_data[i,j],2), ')', sep = " ", collapse = NULL)
  }
}

#write.csv(print_data,"accuracy_comparison_real.csv")

