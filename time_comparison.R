setwd('E:/dropbox/Dropbox/projects/phd/codes/matlab/fm_fi/t test comparison')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/matlab/fm_fi/t test comparison')

#file_names_real <- c("acc_adaboost_real_csv.csv", "acc_bagging_real_csv.csv", "acc_DeFIMKL_real_csv.csv", "acc_DFRAV_real_csv.csv",
#                     "acc_majSVM_real_csv.csv", "acc_random_forest_real_csv.csv")
#file_names_standard <- c("acc_adaboost_standard_csv.csv", "acc_bagging_standard_csv.csv", "acc_DeFIMKL_standard_csv.csv", "acc_DFRAV_standard_csv.csv",
#                         "acc_majSVM_standard_csv.csv", "acc_random_forest_standard_csv.csv", "acc_DeFIMKL_adaboost_40_standard_csv.csv", 
#                         "acc_DeFIMKL_adaboost_100_standard_csv.csv", "acc_DeFIMKL_adaboost_200_standard_csv.csv")

file_names_standard <- c("acc_DeFIMKL_standard_csv.csv", "acc_DFRAV_standard_csv.csv", "acc_majSVM_standard_csv.csv", 
                         "acc_adaboost_standard_csv.csv", "acc_bagging_standard_csv.csv", 
                         "acc_random_forest_standard_csv.csv")

#time_real <- cbind()
#for (i in 1:length(file_names_real)){
#  data <- read.csv(file_names_real[i],header = TRUE)
#  time_real <- cbind(time_real,data[c(1:4),10])
#}

time_standard <- cbind()
for (i in 1:length(file_names_standard)){
  data <- read.csv(file_names_standard[i],header = TRUE)
  time_standard <- cbind(time_standard,data[c(1:14),10])
}

## new analysis of time ##
time_standard<-time_standard*1000
time_standard<-round(time_standard,2)
speedup<-round(time_standard/time_standard[,1],2)

time_data<-time_standard
#time_data<-time_real
time_comp<-matrix(0,nrow(time_data),ncol(time_data))
defimkl<-time_data[,1]
for (i in 1:nrow(time_data)){
  for (j in 1:ncol(time_data)){
    time_comp[i,j]<-paste(time_data[i,j],'(',speedup[i,j],')', sep = " ", collapse = NULL)
  }
}

temp<-as.data.frame(c(366,178,214,208,351,267,345,569,306,768,690,462,6435,2310))
temp1<-as.data.frame(c(2.57,5.57,5.72,2.68,2.65,2.76,2.46,2.07,2.87,1.74,1.78,2.01,1.58,1.64))
temp1<-as.data.frame(defimkl/time_data[,5])
temp2<-as.data.frame(c(temp1,temp))
#write.csv(time_comp,"time_comparison_standard.csv")
