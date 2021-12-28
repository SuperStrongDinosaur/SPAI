# Importing the dataset

getwd()
setwd("Programing/SPAI")

fin <- read.csv("Future-500-10.csv", na.strings=c(""))
fin2 <- read.csv("Future-500-10.csv", na.strings=c(""))
summary(fin2)

head(fin, 20)

#provides information about the structure of some object
str(fin)

# Makes numeric values out of symbols
fin$Profit <- as.numeric(as.character(fin$Profit))

#presents descriptive statistics for a data set
summary(fin)

#clean data
fin$Expenses <- gsub(" Dollars","",fin$Expenses)
fin$Expenses <- gsub(",","",fin$Expenses)
fin$Expenses <- as.numeric(as.character(fin$Expenses))
head(fin)
str(fin)
fin$Revenue <- gsub("\\$","",fin$Revenue)
fin$Revenue <- gsub(",","",fin$Revenue)
fin$Revenue <- as.numeric(as.character(fin$Revenue))
head(fin)
fin$Growth <- gsub("\\%","",fin$Growth)
fin$Growth <- as.numeric(as.character(fin$Growth))
head(fin, 20)

#string convert to numeric
fin$Expenses <- as.numeric(as.character(fin$Expenses))

#Filtering data using which() 
fin[which(fin$Employees == 45),]

#back up of data set
fin_backup<- fin

#remove data with missing values

#selects entries that contain missing values
fin[!complete.cases(fin),]

# is.na () selects entries that contain missing values for a specific parameter
fin <- fin[!is.na(fin$Industry),]
fin[which(fin$City == "San Francisco"),]

# a sequence of restored objects
rownames(fin) <- 1:nrow(fin)

#Fill data according to real data
fin[is.na(fin$State) &fin$City=="New York", "State"] <- "NY"
fin[is.na(fin$State) &fin$City=="San Francisco", "State"] <- "CA"
fin[is.na(fin$State) &fin$City=="Clarksville", "State"] <- "TN"
fin[is.na(fin$State) &fin$City=="Illinois", "State"] <- "IL"
fin[is.na(fin$State) &fin$City=="Chicago", "State"] <- "IL"
fin[c(82,265),]


# fills the missed values on average using the rm parameter,
#The specified percentage of maximum and minimum values can be rejected
fin$Revenue = ifelse(is.na(fin$Revenue), ave(fin$Revenue, FUN = function(x) mean(x, na.rm = TRUE)), fin$Revenue)

#fill data by median
fin[!complete.cases(fin),]
Med_Empl_Retail <- median(fin[fin$Industry=="Retail", "Employees"], na.rm = TRUE)
fin[fin$Industry=="Retail", "Employees"]<-Med_Empl_Retail

Med_Empl_Retail <- median(fin[fin$Industry=="Software", "Employees"], na.rm = TRUE)
fin[fin$Industry=="Software", "Employees"]<-Med_Empl_Retail

Med_Empl_Retail <- median(fin[fin$Industry=="IT Services", "Employees"], na.rm = TRUE)
fin[fin$Industry=="IT Services", "Employees"]<-Med_Empl_Retail

fin[!complete.cases(fin),]
Med_Empl_Retail <- median(fin[fin$Industry=="Financial Services", "Employees"], na.rm = TRUE)
fin[fin$Industry=="Financial Services", "Employees"]<-Med_Empl_Retail

fin[!complete.cases(fin),]
Med_Rev_contr <- median(fin[fin$Industry=="Construction", "Revenue"], na.rm = TRUE)
fin[fin$Industry=="Construction", "Revenue"]<-Med_Rev_contr

fin[!complete.cases(fin),]
Med_Exp_contr <- median(fin[fin$Industry=="Construction", "Expenses"], na.rm = TRUE)
fin[fin$Industry=="Construction", "Expenses"]<-Med_Exp_contr

fin[!complete.cases(fin),]
Med_Exp_Growth <- median(fin[fin$Industry=="Construction", "Growth"], na.rm = TRUE)
fin[fin$Industry=="Construction", "Growth"]<-Med_Exp_Growth

Med_Exp_Growth <- median(fin[fin$Industry=="Software", "Growth"], na.rm = TRUE)
fin[fin$Industry=="Software", "Growth"]<-Med_Exp_Growth

Med_Exp_Growth <- median(fin[fin$Industry=="Health", "Growth"], na.rm = TRUE)
fin[fin$Industry=="Health", "Growth"]<-Med_Exp_Growth

# filling with derived values
fin[is.na(fin$Profit), "Profit"]<-fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]
fin[is.na(fin$Expenses), "Expenses"]<-fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]

fin[!complete.cases(fin),]
summary(fin)
#scaterplot
library(ggplot2)
graf <- ggplot(data=fin)
graf+ geom_point(aes(x=Revenue, y=Expenses, colour = Industry, size = Profit))

#histogram
hist(fin$Revenue)

#scaterplot with trends
tr<-ggplot(data=fin, aes(x=Revenue, y=Profit, colour = Industry))
tr+ geom_point(aes(x=Revenue, y=Profit, colour = Industry)) + geom_smooth(method = "loess", formula = y ~ x)

#boxplot
st <- ggplot(data=fin, aes(x=Industry, y=Growth, colour = Industry))
st + geom_boxplot(size=1)

boxplot(fin$Revenue, horizontal = TRUE)
boxplot(fin$Revenue~fin$Industry, horizontal = FALSE)

#identification of outliers

fin1 <- fin
boxplot(fin1$Revenue, horizontal = TRUE)
qnt <- quantile(fin1$Revenue, probs=c(.25, .75), na.rm = T)
caps <- quantile(fin1$Revenue, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(fin1$Revenue, na.rm = T)
fin1 <- subset(fin1, Revenue > (qnt[1] - H))
fin1 <- subset(fin1, Revenue < (qnt[2] + H))
#fin1$Revenue[fin1$Revenue < (qnt[1] - H)] <- caps[1]
#fin1$Revenue[fin1$Revenue > (qnt[2] + H)] <- caps[2]
boxplot(fin1$Revenue, horizontal = TRUE)

summary(fin1)
summary(fin)

graf <- ggplot(data=fin1)
graf+ geom_point(aes(x=Revenue, y=Expenses, colour = Industry, size = Profit))

#histogram
hist(fin1$Revenue)

#scaterplot with trends
tr<-ggplot(data=fin1, aes(x=Revenue, y=Profit, colour = Industry))
tr+ geom_point(aes(x=Revenue, y=Profit, colour = Industry)) + geom_smooth(method = "loess", formula = y ~ x)

#boxplot
st <- ggplot(data=fin1, aes(x=Industry, y=Growth, colour = Industry))
st + geom_boxplot(size=1)

boxplot(fin1$Revenue, horizontal = TRUE)
boxplot(fin1$Revenue~fin$Industry, horizontal = FALSE)

#x <- fin$Revenue
#boxplot(x, horizontal = TRUE)
#qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#caps <- quantile(x, probs=c(.05, .95), na.rm = T)
#H <- 1.5 * IQR(x, na.rm = T)
#x[x < (qnt[1] - H)] <- caps[1]
#x[x > (qnt[2] + H)] <- caps[2]
#boxplot(x, horizontal = TRUE)
fin<-fin1
summary(fin)

#normalization mean
Fin_norm <- fin
Fin_norm[8:11]<-scale(fin[8:11])
summary(Fin_norm)
#normalization standard deviation
Fin_norm[8:11]<-scale(fin[8:11], center = FALSE, scale = apply(fin[8:11], 2, sd, na.rm = TRUE))
summary(Fin_norm)
#normalization min-max normalization
Fin_norm[8] = (fin[8]-min(fin[8]))/(max(fin[8])-min(fin[8]))
Fin_norm[9] = (fin[9]-min(fin[9]))/(max(fin[9])-min(fin[9]))
Fin_norm[10] = (fin[10]-min(fin[10]))/(max(fin[10])-min(fin[10]))
Fin_norm[11] = (fin[11]-min(fin[11]))/(max(fin[11])-min(fin[11]))
summary(Fin_norm)

#MDS method
dataset1 <- fin[c(4,5,8,9,10,11,3)]
dataset1[-7]<-scale(dataset1[-7])
#dimension reduction using MDS
dist_MDS=dist(dataset1[-7])
dataset2 = cmdscale(dist_MDS, k=2)
dataset1=cbind(dataset2, dataset1[7])
colnames(dataset1) <- c("DIM1","DIM2","Industry")

#Visualization MDS
graf <- ggplot(data=dataset1)
graf+ geom_point(aes(x=DIM1, y=DIM2, colour = Industry))

# data matrix formation for PCA and MDS dimension reduction
training_set<-fin[c(4,5,8,9,10,11,3)]
training_set[-7]<-scale(training_set[-7])

# Applying PCA
library(caret)
library(e1071)
pca = preProcess(x = training_set[-7], method = 'pca', pcaComp = 2)
training_set = predict(pca, training_set)
#changes column positions in the order specified
training_set = training_set[c(2, 3, 1)]
#Visualize PCA results
graf <- ggplot(data=training_set)
graf+ geom_point(aes(x=PC1, y=PC2, colour = Industry))

#Revenue, Growth, Profit,  Expense, Employees
cor(fin1$Expense, fin1$Employees, method = c("pearson", "kendall", "spearman"))

summary(fin)
