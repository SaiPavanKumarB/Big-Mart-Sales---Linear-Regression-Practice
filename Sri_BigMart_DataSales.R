# Big Mart Sales Prediction

# Setting the work directory

setwd("D:/Sri_DataScience_Practice/Big Mart Sales - Linear Regression Practice")

# Load the datasets

train = read.csv("train.csv")
NROW(train) #8523
View(train)
test = read.csv("test.csv")
NROW(test) #5681

# Load libraries
install.packages("ggplot2")
library("ggplot2")
install.packages("gdata")
library("gdata")
install.packages("plyr")
library("plyr")
install.packages("stringr")
library("stringr")


# Check data structure
str(train)

# Check each column
NROW(unique(train$Item_Identifier)) # 1559 unique products
NROW(which(is.na(train$Item_Identifier))) # 0 missing values
ggplot(data.frame(train$Item_Identifier), aes(x=train$Item_Identifier))+geom_bar(stat = "count")

NROW(which(train$Item_Weight<=0)) # No missing values
NROW(which(is.na(train$Item_Weight))) # 1463 missing values
hist(train$Item_Weight, labels = TRUE)
boxplot(train$Item_Weight, horizontal = TRUE) # No outliers

NROW(unique(train$Item_Fat_Content)) # 5 unique value
NROW(which(is.na(train$Item_Fat_Content))) # No missing values
ggplot(data.frame(train$Item_Fat_Content), aes(x=train$Item_Fat_Content))+geom_bar(stat = "count")

# Only 2 categories broadly. LF, low fat & Low Fat can be merged.
# Similarly reg & Regular can be merged

train$Item_Fat_Content = ifelse(train$Item_Fat_Content=="low fat", "LF",ifelse(train$Item_Fat_Content=="Low Fat","LF",ifelse(train$Item_Fat_Content=="reg","Regular",as.character(train$Item_Fat_Content))))
train$Item_Fat_Content = as.factor(train$Item_Fat_Content)
NROW(unique(train$Item_Fat_Content)) # 5 unique value
ggplot(data.frame(train$Item_Fat_Content),aes(x=train$Item_Fat_Content))+geom_bar(stat = "count")
test$Item_Fat_Content = ifelse(test$Item_Fat_Content=="low fat", "LF",ifelse(test$Item_Fat_Content=="Low Fat","LF",ifelse(test$Item_Fat_Content=="reg","Regular",as.character(test$Item_Fat_Content))))

NROW(which(train$Item_Visibility==0)) 
View(train[train$Item_Visibility==0,])
# 526 items with visibility 0. But this is not possible as the products are sold
# These can be treated as missing values
Outvals= boxplot(train$Item_Visibility, horizontal = TRUE, xlab = "Item Visibility", col = "orange")
NROW(Outvals$out) # 144 outliers 
hist(train$Item_Visibility, xlab = "Visibility",labels = TRUE ) # Right skewed data set

NROW(which(is.na(train$Item_Type)))
ggplot(data.frame(train$Item_Type), aes(x=train$Item_Type))+geom_bar(stat = "count")

 # Replace space with _ in the type

train$Item_Type = str_replace_all(as.character(train$Item_Type)," ","_")
test$Item_Type = str_replace_all(as.character(test$Item_Type)," ","_")
View(train[train$Item_Identifier=="NCN07",])


NROW(which(train$Item_MRP<=0))
hist(train$Item_MRP, labels = TRUE, col = "orange")
boxplot(train$Item_MRP, xlab="MRP",horizontal = TRUE,col = "orange")


NROW(which(is.na(train$Outlet_Identifier)))
NROW(unique(train$Outlet_Identifier))
ggplot(data.frame(train$Outlet_Identifier), aes(x=train$Outlet_Identifier))+geom_bar(stat = "count")

NROW(which(train$Outlet_Establishment_Year<=0))
summary(train$Outlet_Establishment_Year)
boxplot(train$Outlet_Establishment_Year, horizontal = TRUE, col = "green")

NROW(which(is.na(train$Outlet_Size)))
NROW(unique(train$Outlet_Size))
NROW(which(train$Outlet_Size==""))
ggplot(data.frame(train$Outlet_Size), aes(x=train$Outlet_Size))+geom_bar(stat = "count")

NROW(unique(train$Outlet_Location_Type))
ggplot(data.frame(train$Outlet_Location_Type), aes(x=train$Outlet_Location_Type))+geom_bar(stat = "count")

NROW(unique(train$Outlet_Type))
#Replace spaces in description with _
train$Outlet_Type = str_replace(as.character(train$Outlet_Type)," ","_")
test$Outlet_Type = str_replace(as.character(test$Outlet_Type)," ","_")
ggplot(data.frame(train$Outlet_Type),aes(x=train$Outlet_Type))+geom_bar(stat = "count")

NROW(which(train$Item_Outlet_Sales<=0))
Sales_box = boxplot(train$Item_Outlet_Sales, horizontal = TRUE, col = "yellow2",xlab = "Sales", main = "Item Outlet Sales Boxplot")
hist((train$Item_Outlet_Sales), labels = TRUE, main = "Item Outlet Sales Distribution", xlab = "Sales")

# Treat the missing values

# Item Weight
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v, incomparables = TRUE)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

train$Item_Weight_old = train$Item_Weight
train$Item_Weight = train$Item_Weight_old

for(level1 in unique(train$Item_Identifier)){
  cat(level1)
      item_weight_mod=0
      item_weight_mod=getmode(train$Item_Weight[which(train$Item_Identifier==level1)])
      cat(item_weight_mod)
      train$Item_Weight[train$Item_Identifier==level1]=item_weight_mod
}


# Rounding item visibility to 3 digits

train$Item_Visibility = round(train$Item_Visibility,digits = 4)
train$Item_Visibility_upd = train$Item_Visibility
train$Item_Visibility = train$Item_Visibility_upd

for(level1 in unique(train$Item_Identifier)){
  cat(level1)
    Item_Visibility_mod=0
  Item_Visibility_mod=getmode(train$Item_Visibility[which(train$Item_Identifier==level1)])
  cat(Item_Visibility_mod)
  train$Item_Visibility[which((train$Item_Identifier==level1) & (train$Item_Visibility==0))]=round(item_weight_mod,digits = 4)
      }


# Handling outlet size missing values

train$Outlet_Location_Type = str_replace(train$Outlet_Location_Type," ","_")
test$Outlet_Location_Type = str_replace(test$Outlet_Location_Type," ","_")

train$Outlet_Size[which((train$Outlet_Size=="") & (train$Outlet_Type=="Grocery_Store"))] = getmode(train$Outlet_Size[which((train$Outlet_Type=="Grocery_Store") & (train$Outlet_Size!=""))])
train$Outlet_Size[which((train$Outlet_Size=="") & (train$Outlet_Type=="Supermarket_Type1") & (train$Outlet_Location_Type=="Tier_2"))] = getmode(train$Outlet_Size[which((train$Outlet_Type=="Supermarket_Type1") & (train$Outlet_Size!="") & (train$Outlet_Location_Type=="Tier_2"))])
write.csv(train, file = "train_upd.csv")                          


# Create final data set
str(train)

train_new = train[,!colnames(train) %in% c("Item_Identifier","Item_Weight_old","Item_Visibility_upd")]
train_new$Item_Type = as.factor(train_new$Item_Type)
train_new$Outlet_Location_Type = as.factor(train_new$Outlet_Location_Type)
train_new$Outlet_Type = as.factor(train_new$Outlet_Type)
str(train_new)
# Convert categorical variables to dummies
library("fastDummies")
library("psych")
library("dummies")

train_new_dummies = dummies::dummy.data.frame(train_new,names = c("Item_Fat_Content","Item_Type","Outlet_Identifier","Outlet_Size","Outlet_Location_Type","Outlet_Type"))

train_new$Item_Weight[which(is.na(train_new$Item_Weight))] = getmode(train_new$Item_Weight[which(is.na(train_new$Item_Weight)==FALSE)])



bigmart_cor = as.data.frame(cor(train_new_dummies))
View(bigmart_cor)
write.csv(bigmart_cor, file = "correlation.csv")
View(train_new_dummies)

# After verifying correlation, build new data set with important elements
train_final = train_new_dummies[,colnames(train_new_dummies) %in% c("Item_MRP","Outlet_IdentifierOUT027","Outlet_TypeSupermarket_Type3","Outlet_SizeMedium","Outlet_TypeSupermarket_Type1","Outlet_Location_TypeTier_1","Outlet_SizeSmall","Outlet_IdentifierOUT019","Outlet_IdentifierOUT010","Outlet_TypeGrocery_Store","Item_Outlet_Sales")]
train_final2=train_new_dummies[,colnames(train_new_dummies) %in% c("Item_MRP","Outlet_IdentifierOUT027","Outlet_TypeSupermarket_Type1","Outlet_IdentifierOUT019","Outlet_IdentifierOUT010","Item_Outlet_Sales")]

# Split the data to train & cross validate
set.seed(123)
dt = sort(sample(nrow(train_final2), nrow(train_final2)*.7))
train_data<-train_final2[dt,]
cv_data<-train_final2[-dt,]

bigmart_LR= lm(train_data$Item_Outlet_Sales ~ ., data = train_data)
summary(bigmart_LR)

# Use the model to predict on cv_data
cv_data$predicted_sales = predict(bigmart_LR, newdata = cv_data)
View(cv_data)


# Calculate RMSE 
install.packages("Metrics")
library("Metrics")
rmse(cv_data$Item_Outlet_Sales, cv_data$predicted_sales)
Metrics::accuracy(cv_data$Item_Outlet_Sales, cv_data$predicted_sales)


# Calculate R2
SSE = sum((cv_data$predicted_sales - cv_data$Item_Outlet_Sales)^2)
SST = sum((cv_data$Item_Outlet_Sales - mean(cv_data$Item_Outlet_Sales))^2)
(1-(SSE/SST))
