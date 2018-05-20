# Set the working directory
setwd("D:/Sri_DataScience_Practice/Big Mart Sales - Linear Regression Practice")


install.packages("dplyr")
library("dplyr")
# Load the train file

train = read.csv("Train.csv")
test = read.csv("Test.csv")

# Verify the train data set
str(train)
summary(train)

# Item_Fat content column has different descriptions. Regularizing the same
unique(train$Item_Fat_Content)
NROW(train[train$Item_Fat_Content=="low fat"|train$Item_Fat_Content=="Low Fat"|train$Item_Fat_Content=="LF",])

train$Item_Fat_Content=ifelse(train$Item_Fat_Content=="LF"|train$Item_Fat_Content=="low fat"|train$Item_Fat_Content=="Low Fat",1,2)


# Check for any missing values
NROW(train[is.na(train$Item_Identifier),])
NROW(train[is.na(train$Item_Weight),]) # 1463 missing values
NROW(train[is.na(train$Item_Fat_Content),])
NROW(train[is.na(train$Item_Visibility),])
NROW(train[trimws(train$Item_Type)=="",]) # 
NROW(train[is.na(train$Item_MRP),])
NROW(train[is.na(train$Outlet_Identifier),])
NROW(train[is.na(train$Outlet_Establishment_Year),])
NROW(train[trimws(train$Outlet_Size)=="",]) # 2410 rows
NROW(train[trimws(train$Outlet_Location_Type)=="",])


boxplot(train$Item_Weight)
boxplot(train$Item_Visibility)
hist(train$Item_Visibility) # Right skewed data
boxplot(train$Item_MRP)
hist(train$Item_MRP)
boxplot(train$Item_Outlet_Sales)
hist(train$Item_Outlet_Sales) # Right skewed data

# We need to deal with Item weight & Outlet_size (Categorical)


install.packages("DMwR")
library("DMwR")

look_up = unique(train[is.na(train$Item_Weight)==FALSE,colnames(train) %in% c("Item_Identifier","Item_Weight","Item_Fat_Content")])

str(train)
# Use for loop to impute missing values through nearest group
 for (i in 1:NROW(train)){
   if(is.na(train[,"Item_Weight"][i])){
     data_A = train[which(train$Item_Identifier==train$Item_Identifier[i]& train$Item_Fat_Content==train$Item_Fat_Content[i]), names(train) %in% c("Item_Identifier","Item_Fat_Content","Item_Weight")]
     train[i,"Item_Weight"]=median(data_A$Item_Weight, na.rm = TRUE)
     remove(data_A)
   }
   
 }

# Still there are 4 missing values. Replace with Item Type Median

for (i in 1:NROW(train)){
  if(is.na(train[,"Item_Weight"][i])){
    train[i,"Item_Weight"]=median(data_A$Item_Weight, na.rm = TRUE)
    remove(data_A)
  }
  
}

# Replace missing values for Outlet_Size using Outlet_Location_Type, Outlet_Type

# COnvert factor to numerical variables

train$Outlet_Type=ifelse(train$Outlet_Type=="Grocery Store",0,ifelse(train$Outlet_Type=="Supermarket Type1",1,ifelse(train$Outlet_Type=="Supermarket Type2",2,3)))
train$Outlet_Location_Type=ifelse(train$Outlet_Location_Type=="Tier 1",1,ifelse(train$Outlet_Type=="Tier 2"))
