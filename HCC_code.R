#*********************************************************************************#
#********************* Machine Learning  *****************************#
#********************* by Ruthba hashim  ********************************#
#*********************************************************************************#

#*********************loading data******************************
#setting the work directory and loading the data into a dataframe from the csv file
#As seen in the csv file, there is some missing data denoted by ? which is read as NA while
#loading the csv file
setwd("C:/Users/reply/Rcode")
data1 <-read.csv("HCC_Survival.csv",header = TRUE,na.strings = c("?"))
head(data1) # we can check the file and observe that ? issue solved

#*************** Data visualization ***************************
library(ggplot2)
#plotting graphs accordig to the basic understanding that smoking,age,gender etc can effect 
# the HCC occurence. it does not specify any correlation

#Gender 
count1 <- table(data1$Gender,data1$Class..1.Year.Survival.)
barplot(count1,main = "Survival w.r.t Gender",col=c("pink","light blue"),
        ylab = "No.of Customers",xlab = "Survival",xlim=c(0,4),
        legend= c("Female","Male"))

plot.new()
#Symptom
count2 <- table(data1$Symptoms,data1$Class..1.Year.Survival.)
barplot(count2,main = "Survival w.r.t Symptoms",col=c("yellow","light green"),
        ylab = "No.of Customers",xlab = "Survival",xlim=c(0,5),
        legend=c("Symptom-No","Symptom-Yes"))

#Alcohol
count3 <- table(data1$Alcohol,data1$Class..1.Year.Survival.)
barplot(count3,main = "Survival w.r.t Alcohol",col=c("aquamarine","chocolate"),
        ylab = "No.of Customers",xlab = "Survival",xlim=c(0,5),
        legend=c("Alcohol-No","Alcohol-Yes"))

#Endemic Countries
count4 <- table(data1$Endemic.Countries,data1$Class..1.Year.Survival.)
barplot(count4,main = "Survival w.r.t Endemic Countries",col=c("cyan","pink"),
        ylab = "No.of Customers",xlab = "Survival",xlim=c(0,5),
        legend=c("Ende.Cntry - No","Ende.Cntry - Yes"))

#Smoking
count5<- table(data1$Smoking,data1$Class..1.Year.Survival.)
barplot(count5,main = "Survival w.r.t Smoking",col=c("#CCFF66","#FF3399"),
        ylab = "No.of Customers",xlab = "Survival",xlim=c(0,5),
        legend=c("Smoking - No","Smoking - Yes"))

#Diabetes
count6<- table(data1$Diabetes,data1$Class..1.Year.Survival.)
barplot(count6,main = "Survival w.r.t Diabetes",col=c("#CC00FF","#66CC66"),
        ylab = "No.of Customers",xlab = "Survival",xlim=c(0,5),
        legend=c("Diabetes - No","Diabetes - Yes"))

#Obesity
count7<- table(data1$Obesity,data1$Class..1.Year.Survival.)
barplot(count7,main = "Survival w.r.t Obesity",col=c("#FFCCFF","#FF33FF"),
        ylab = "No.of Customers",xlab = "Survival",xlim=c(0,5),
        legend=c("Obesity - No","Obesity - Yes"))


#Age at diagnosis
Survived<- factor(data1$Class..1.Year.Survival.)
ggplot(data1,aes(x=Survived, y=Age.at.diagnosis,fill=Survived)) +
  geom_boxplot(colour=c("#CC99CC","#3300FF"),outlier.color = "red")+
  ggtitle("Survival w.r.t  Age at Diagnosis")

mean = mean(data1$Age.at.diagnosis, na.rm = T)
var = var(data1$Age.at.diagnosis, na.rm = T)
sd = sd(data1$Age.at.diagnosis, na.rm = T)
hist(data1$Age.at.diagnosis, col = "#3399FF", border="white",freq = FALSE,main=NULL)
curve(dnorm(x, mean = mean, sd = sd), add = T,col = "red", lwd = 1)
  

#Grams per Day
ggplot(data1,aes(x=Survived, y=Grams...day,fill=Survived)) +
  geom_boxplot(colour=c("#CC99CC","#3300FF"),outlier.color = "red")+
  ggtitle("Survival w.r.t  Grams per Day")

mean = mean(data1$Grams...day, na.rm = T)
var = var(data1$Grams...day, na.rm = T)
sd = sd(data1$Grams...day, na.rm = T)
hist(data1$Grams...day, col = "#3399FF", border="white",freq = FALSE,main=NULL)
curve(dnorm(x, mean = mean, sd = sd), add = T,col = "red", lwd = 1)

#Packs per Year
ggplot(data1,aes(x=Survived, y=Packs...year,fill=Survived)) +
  geom_boxplot(colour=c("#CC99CC","#3300FF"),outlier.color = "red")+
  ggtitle("Survival w.r.t  Packs per Year")

mean = mean(data1$Packs...year, na.rm = T)
var = var(data1$Packs...year, na.rm = T)
sd = sd(data1$Packs...year, na.rm = T)
hist(data1$Packs...year, col = "#3399FF", border="white",freq = FALSE,
     main=NULL,xlim = c(0,150))
curve(dnorm(x, mean = mean, sd = sd), add = T,col = "red", lwd = 1)


#*************** Data Exploration ***************************

#Check the data types of the variables
str(data1)

data1$Gender<-as.factor(data1$Gender)
data1$Symptoms<-as.factor(data1$Symptoms)
data1$Alcohol<-as.factor(data1$Alcohol)
data1$HBsAg<-as.factor(data1$HBsAg)
data1$HBeAg<-as.factor(data1$HBeAg)
data1$HBcAb<-as.factor(data1$HBcAb)
data1$HBVAb<-as.factor(data1$HBVAb)
data1$Cirrhosis<-as.factor(data1$Cirrhosis)
data1$Endemic.Countries<-as.factor(data1$Endemic.Countries)
data1$Smoking<-as.factor(data1$Smoking)
data1$Diabetes<-as.factor(data1$Diabetes)
data1$Obesity<-as.factor(data1$Obesity)
data1$Hemochromatosis<-as.factor(data1$Hemochromatosis)
data1$AHT<-as.factor(data1$AHT)
data1$CRI<-as.factor(data1$CRI)
data1$HIV<-as.factor(data1$HIV)
data1$NASH<-as.factor(data1$NASH)
data1$Esophageal.varices<-as.factor(data1$Esophageal.varices)
data1$Splenomegaly<-as.factor(data1$Splenomegaly)
data1$Portal.hypertension<-as.factor(data1$Portal.hypertension)
data1$Portal.vein.thrombosis<-as.factor(data1$Portal.vein.thrombosis)
data1$Liver.metastasis<-as.factor(data1$Liver.metastasis)
data1$Radiological.hallmark<-as.factor(data1$Radiological.hallmark)
data1$Class..1.Year.Survival.<- as.factor(data1$Class..1.Year.Survival.)
data1$Age.at.diagnosis<-as.numeric(data1$Age.at.diagnosis)
data1$Grams...day<-as.numeric(data1$Grams...day)
data1$Performance.status<-as.numeric(data1$Performance.status)
data1$Encefalopathy <-as.numeric(data1$Encefalopathy )
data1$Ascites<-as.numeric(data1$Ascites)
data1$ALT<-as.numeric(data1$ALT)
data1$AST<-as.numeric(data1$AST)
data1$Number.of.nodules<-as.numeric(data1$Number.of.nodules)

#summarize data
summary(data1)
#according to data summary,there seem to be lot of missing data hence we would first check for 
#missing data

#visualizing missing data
library(VIM)

mice_plot <- aggr(data1, col=c('blue','yellow'),
                  numbers = TRUE, sortVars = TRUE,
                  labels=names(data1), cex.axis=.4,
                  gap=1)

#missing data count
 
miss_data = colSums(is.na(data1))
miss_data




#*************** Data Preprocessing ***************************
#Imputing Missing values

#imputing missing values by KNN, this method is taken for imputation inorder to find the missing 
#value by comparing values near to this. KNN is a known method for imputing values in medical stream predictions

a<- kNN(data1[,-50],k=5)
summary(a)

#creating complete data (original data plus imputed data)
data2<- a[,c(1:49)]
data2$Survival = data1$Class..1.Year.Survival.

#check if values have been imputed
miss_data = colSums(is.na(data2))
miss_data

#Scaling the data

# As seen some attributes like AFP,Leukocytes have large range of values hence we need to scalr the data
#inorder to get relative values. Since data has both binary as well as continuous data, we would be 
#performing scaling on numeric and integer data and leaving the binary as it is

for (colName in names(data2)) {
  
  # Check if the column contains numeric data.
  if(class(data2[,colName]) == 'integer' | class(data2[,colName]) == 'numeric') {
    
    # Scale this column (scale() function applies z-scaling).
    data2[,colName] = scale(data2[,colName])
    
  }
}


#************** adding code for PCA *****************************
#install.packages("stats")
library(stats)

#filtering numeric and integer data
data3 <- Filter(is.numeric,data2)
sample_pca <- prcomp(data3, center = TRUE, scale = TRUE) 
summary(sample_pca)

#**************Creating machine learning model and testing the model*********************
#Creating training and testing data
library(caTools)
set.seed(11)
#creating split
split1<- sample.split(data2,SplitRatio = .70)
#training set
train <- subset(data2,split1==TRUE)
head(train)
#testing set
test <- subset(data2,split1==FALSE)
head(test)


#As the target variable is binomial we can use SVM technique for modelling

#creating classifier 
#install.packages("e1071")
library(e1071)
classifier_svm = svm(formula = Survival ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the test set results using svm -linear

test_data_svm = predict(classifier_svm, newdata = test[-50])

#creating Confusion Matrix for svm -linear

conf_matrix_svm = table(test[,50], test_data_svm)
conf_matrix_svm

#Accuracy for SVM -linear
accuracy_svm<-(sum(diag(conf_matrix_svm)))/sum(conf_matrix_svm)
accuracy_svm
#rounding to 3 digits
accuracy_svm <- round(accuracy_svm,3)
accuracy_svm

#Creating model using SVM- Radial basis function 

classifier_svmR = svm(formula = Survival ~ .,
                     data = train,
                     type = 'C-classification',
                     kernel = 'radial')

# Predicting the test set results for radial basis function on SVM

test_data_svmR = predict(classifier_svmR, newdata = test[-50])

#Making the confusion Matrix for SVM radial basis 

conf_matrix_svmR = table(test[,50], test_data_svmR)
conf_matrix_svmR

#Accuracy of svm radial basis
accuracy_svmR<-(sum(diag(conf_matrix_svmR)))/sum(conf_matrix_svmR)
accuracy_svmR

#rounding to 3 digits
accuracy_svmR <- round(accuracy_svmR,3)
accuracy_svmR

#Using KNN (K nearest neighbour) for creating the model. For an optimum result, 
#for predicting the result, we would be comparing it with 15 close points
library(class)
test_knn <- knn(train = train[,-50],
                test = test[,-50],
                cl = train[,50], k=15)
test_knn

#Making the confusion Matrix for knn

conf_matrix_knn = table(test[,50],test_knn )
conf_matrix_knn

#Accuracy for knn
accuracy_knn<-(sum(diag(conf_matrix_knn)))/sum(conf_matrix_knn)
accuracy_knn

#rounding to 3 digits
accuracy_knn <- round(accuracy_knn,3)
accuracy_knn
