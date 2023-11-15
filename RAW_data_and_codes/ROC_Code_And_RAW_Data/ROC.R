# This script reproduces the ROC curves in the paper titled “Comprehensive Analysis of Splicing Factors to Construct
# Prognosis Prediction Classifier in Prostate Cancer” by He Zhang. This script was executed on a MacOS 14.1.1 system using R 4.2.2.
# Detailed comments are embedded within the script for guidance.

# First, load the required R packages. 
# Input file format: Columns include id (sample identifier), time (follow-up time or event occurrence time),
# survivalstate (disease progression status: 0 for stable disease, 1 for biochemical recurrence), 
# and riskScore (calculated risk score from the classifier).


#install.packages("survival")
#install.packages("survminer")
#install.packages("survivalROC")

library("survival")
library("survminer")
library("survivalROC")


# ---------------------------For Figure 4E
# Read in risk score data for Figure 4E analysis.
data_5E=read.table("Fig5E_training.txt",header=T,sep="\t",check.names=F,row.names=1)    
# The “survivalROC” package is used for ROC curve analysis with the Kaplan-Meier (KM) method.
# The following code block sets up and plots the ROC curve.
input_data<-data_5E
roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore, predict.time =3,  method="KM")
# Customize plot parameters for the ROC curve.
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)

####---------------------------For Figure 5F
####Similar steps are repeated for other figures (5F, 6A, 6B, 6C, 4F-Left, 4F-Right) with appropriate data files and parameters.

data_5E=read.table("Fig5E_testing .txt",header=T,sep="\t",check.names=F,row.names=1)    ####Read the calculated riskscore data.
input_data<-data_5E

# The “survivalROC” package is used for ROC curve analysis with the Kaplan-Meier (KM) method.
roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore, predict.time =3,  method="KM")
# Customize plot parameters for the ROC curve.

par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)


#---------------------------For Figure 6A


data_6A=read.table("Fig6A.txt",header=T,sep="\t",check.names=F,row.names=1)    ####Read the calculated riskscore data.
input_data<-data_6A
roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore, predict.time =2,  method="KM")
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)








#---------------------------For Figure 6B

data_6B=read.table("Fig6B.txt",header=T,sep="\t",check.names=F,row.names=1)    ####Read the calculated riskscore data.
input_data<-data_6B
roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore, predict.time =2,  method="KM")
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)

#---------------------------For Figure 6C



data_6C=read.table("Fig6C.txt",header=T,sep="\t",check.names=F,row.names=1)    ####Read the calculated riskscore data.
input_data<-data_6C
roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore, predict.time =6,  method="KM")
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)



#---------------------------For Figure 4F-Left

data_4FL=read.table("Fig4FL.txt",header=T,sep="\t",check.names=F,row.names=1)    ####Read the calculated riskscore data.
input_data<-data_4FL
roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore, predict.time =3,  method="KM")
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)

#---------------------------For Figure 4F-Right
data_4FR=read.table("Fig4FR.txt",header=T,sep="\t",check.names=F,row.names=1)    ####Read the calculated riskscore data.
input_data<-data_4FR
roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore, predict.time =3,  method="KM")
par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)


################# The following code for smoothing the ROC curve by using the "NNE" method as the manual of “survivalROC” package mentioned.
################# Take the figure5E data as an example.



data_5E=read.table("Fig5E_training.txt",header=T,sep="\t",check.names=F,row.names=1)    ####Read the calculated riskscore data.

#### We used R package “survivalROC” to analyze the survival-related ROC curve, using the "NNE" method
####If we use the "KNN" method, a smooth parameter must given, as the package's “example” mentioned, span = 0.25*nobs^(-0.20) 
####The “nobs” means the total cases of the data 
nobs <- NROW(data_5E)
span = 0.25*nobs^(-0.20)
##here the span=0.08299457
####
input_data<-data_5E

roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore,
                       predict.time =3,
                       method="NNE",
                       span = span
)

#### Parameters for generating the survival-related ROC curve


par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)




#### We can see the curve became smooth, but the calculated AUC=0.779(KNN) differ from AUC=0.848 (KM).
#### Indeed, the span can be any value, and each of them will give a different AUC, which makes the results highly adjustable and difficult to reproduce
#### for example
#### span = 1.0
span = 1.0

roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore,
                       predict.time =3,
                       method="NNE",
                       span = span
)

#### Parameters for generating the survival-related ROC curve


par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)




#### span = 0.5
span = 0.5

roc_data = survivalROC(Stime=input_data$time, status=input_data$survivalstate, marker = input_data$riskScore,
                       predict.time =3,
                       method="NNE",
                       span = span
)

#### Parameters for generating the survival-related ROC curve


par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
roc=roc_data
plot(roc$FP, roc$TP, type="l", xlim=c(0,1), ylim=c(0,1),col='red', 
     xlab="False positive rate", ylab="True positive rate",
     main=paste("ROC curve (", "AUC = ",round(roc$AUC,3),")"),
     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
abline(0,1)


#### Considering this, we used the less adjustable “KM" method to generate the ROC plot, although it is given a rocky plot, we believe it is more accurate and reproducible. 




#########Pleas not RUN ###########
##The sessionInfo outputs
R version 4.2.2 (2022-10-31)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS 14.1.1

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib

locale:
  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
  [1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
  [1] survivalROC_1.0.3.1 survminer_0.4.9     ggpubr_0.5.0        glmnet_4.1-6       
[5] Matrix_1.5-1        caret_6.0-93        lattice_0.20-45     ggplot2_3.4.0      
[9] survival_3.4-0     

loaded via a namespace (and not attached):
  [1] Rcpp_1.0.10          lubridate_1.9.2      tidyr_1.2.1         
[4] listenv_0.9.0        zoo_1.8-11           class_7.3-20        
[7] assertthat_0.2.1     digest_0.6.31        ipred_0.9-13        
[10] foreach_1.5.2        utf8_1.2.2           parallelly_1.34.0   
[13] R6_2.5.1             plyr_1.8.8           backports_1.4.1     
[16] hardhat_1.2.0        stats4_4.2.2         pillar_1.8.1        
[19] rlang_1.0.6          rstudioapi_0.14      data.table_1.14.6   
[22] car_3.1-1            rpart_4.1.19         splines_4.2.2       
[25] gower_1.0.1          stringr_1.5.0        munsell_0.5.0       
[28] broom_1.0.2          xfun_0.37            compiler_4.2.2      
[31] pkgconfig_2.0.3      shape_1.4.6          globals_0.16.2      
[34] nnet_7.3-18          tidyselect_1.2.0     km.ci_0.5-6         
[37] gridExtra_2.3        tibble_3.1.8         prodlim_2019.11.13  
[40] codetools_0.2-18     fansi_1.0.4          future_1.31.0       
[43] dplyr_1.0.10         withr_2.5.0          MASS_7.3-58.1       
[46] recipes_1.0.4        ModelMetrics_1.2.2.2 grid_4.2.2          
[49] xtable_1.8-4         nlme_3.1-160         gtable_0.3.1        
[52] lifecycle_1.0.3      DBI_1.1.3            KMsurv_0.1-5        
[55] magrittr_2.0.3       pROC_1.18.0          scales_1.2.1        
[58] future.apply_1.10.0  cli_3.6.0            stringi_1.7.12      
[61] carData_3.0-5        ggsignif_0.6.4       reshape2_1.4.4      
[64] timeDate_4022.108    survMisc_0.5.6       generics_0.1.3      
[67] vctrs_0.5.1          lava_1.7.1           iterators_1.0.14    
[70] tools_4.2.2          glue_1.6.2           purrr_1.0.1         
[73] abind_1.4-5          parallel_4.2.2       timechange_0.2.0    
[76] colorspace_2.0-3     rstatix_0.7.1        knitr_1.42   



