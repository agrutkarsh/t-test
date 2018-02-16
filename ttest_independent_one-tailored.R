setwd('E:/dropbox/Dropbox/projects/phd/codes/matlab/fm_fi/t test comparison')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/matlab/fm_fi/t test comparison')

data<-read.csv("t-test_sample_data.csv",header = FALSE)

ttest_result<-t.test(data[,1],data[,2],paired = FALSE,alternative = "greater")

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

