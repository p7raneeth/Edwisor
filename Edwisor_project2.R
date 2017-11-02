# Loading the data into R Environment and installing required libraries
#----------------------------------------------------------------------
data1 = read.csv('TextClassification_Data.csv',stringsAsFactors = F)
install.packages('FSelector')
library(tm)
library(FSelector)
library(rJava)
rm(dt5)

# Cleaning Data and Performing TF-IDF on the Existing Text
#---------------------------------------------------------

txt1 <- Corpus(VectorSource(data1$SUMMARY))
txt1 <- tm_map(txt1, stripWhitespace)
txt1 <- tm_map(txt1, content_transformer(tolower))
txt1 <- tm_map(txt1, removePunctuation)
txt1 <- tm_map(txt1, removeWords, stopwords("english"))
txt1 <- tm_map(txt1, stemDocument, language="english")
txt1 <- tm_map(txt1, removeNumbers)
txt1 <- tm_map(txt1,function(x)iconv(x, "latin1", "ASCII", sub=""))
dtm1 <- DocumentTermMatrix(txt1, control = list(weighting = weightTfIdf))
dtm1 <- removeSparseTerms(dtm1, 0.95)
data2 <- cbind(data1, as.matrix(dtm1))


txt1 <- Corpus(VectorSource(data1$DATA))
txt1 <- tm_map(txt1, stripWhitespace)
txt1 <- tm_map(txt1, content_transformer(tolower))
txt1 <- tm_map(txt1, removePunctuation)
txt1 <- tm_map(txt1, removeWords, stopwords("english"))
txt1 <- tm_map(txt1, stemDocument, language="english")
txt1 <- tm_map(txt1, removeNumbers)
txt1 <- tm_map(txt1,function(x)iconv(x, "latin1", "ASCII", sub=""))
dtm1 <- DocumentTermMatrix(txt1, control = list(weighting = weightTfIdf))
dtm1 <- removeSparseTerms(dtm1, 0.95)
data3 <- cbind(data2, as.matrix(dtm1))

dt4 = data3[,-c(1,2,3,7,78)]

# Data Visualisation 
#-------------------
library(sqldf)

dt4$categories =  factor(as.character(dt4$categories))
dt4$sub_categories =  factor(as.character(dt4$sub_categories))
dt4$previous_appointment =  factor(as.character(dt4$previous_appointment))


levels(dt4$categories)[2] = "ASK_A_DOCTOR"
levels(dt4$categories)[5] = "MISCELLANEOUS"
levels(dt4$sub_categories)[8] = "MEDICATION RELATED" 
  
levels(dt4$previous_appointment)[1] = "NO"
levels(dt4$previous_appointment)[2] = "NO"
levels(dt4$previous_appointment)[3] = "YES"
levels(dt4$previous_appointment)[2] = "YES"

v1 = sqldf('select categories, count(categories) as cnt from dt4 group by categories')
v2 = sqldf('select sub_categories, count(sub_categories) as cnt from dt4 group by sub_categories')
v3 = sqldf('select previous_appointment, count(previous_appointment) as cnt from dt4 group by previous_appointment')

library(plotly)

# From the below plot on categories it is evident that the 
# majority of the categories belongs to PRESCRIPTION

p <- plot_ly(
  x = v1$categories,
  y = v1$cnt,
  name = "Count of various Categories",
  type = "bar"
)

# From the below plot on Sub Categories it is evident that the 
# majority of the Sub Categories belongs to MEDICATION RELATED

q <- plot_ly(
  x = v2$sub_categories,
  y = v2$cnt,
  name = "Count of various Sub-Categories",
  type = "bar"
)

# From the below plot on PREVIOUS APPOINTMENT it is evident that the 
# majority of the patients had not had any previous appointment

r <- plot_ly(
  x = v3$previous_appointment,
  y = v3$cnt,
  name = "Count of Previous Appointment",
  type = "bar"
)

levels(dt4$categories)

#Finding the dependency between Categories and SubCategories
#------------------------------------------------------------

dt4$categories  = factor(dt4$categories, levels = c('APPOINTMENTS','ASK_A_DOCTOR','JUNK','LAB','MISCELLANEOUS','PRESCRIPTION'),labels = c(1,2,3,4,5,6))

dt4$sub_categories  = factor(dt4$sub_categories, levels = c('CANCELLATION','CHANGE OF HOSPITAL','CHANGE OF PHARMACY','CHANGE OF PROVIDER','FOLLOW UP ON PREVIOUS REQUEST','JUNK',
                                                            'LAB RESULTS','MEDICATION RELATED','NEW APPOINTMENT','OTHERS','PRIOR AUTHORIZATION',
                                                            'PROVIDER','QUERIES FROM INSURANCE FIRM','QUERIES FROM PHARMACY','QUERY ON CURRENT APPOINTMENT',
                                                            'REFILL','RESCHEDULING','RUNNING LATE TO APPOINTMENT','SHARING OF HEALTH RECORDS (FAX, E-MAIL, ETC.)',
                                                            'SHARING OF LAB RECORDS (FAX, E-MAIL, ETC.)','SYMPTOMS'),labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

dt4$categories =  as.numeric(dt4$categories)
dt4$sub_categories =   as.numeric(dt4$sub_categories)
dt4$previous_appointment =   as.numeric(dt4$previous_appointment)


class(dt4$categories)
dt4$sub_categories
str(dt4$previous_appointment)

cor_val = cor(dt4$sub_categories,dt4$previous_appointment)

dt4$categories =  factor(as.character(dt4$categories))
dt4$sub_categories =  factor(as.character(dt4$sub_categories))
dt4$previous_appointment =  factor(as.character(dt4$previous_appointment))


tbl = table(dt4$categories,dt4$sub_categories)

chisq.test(tbl)

#Finding out the important variables from the whole lot
#------------------------------------------------------
install.packages('doBy')
library(doBy)
imp_var = random.forest.importance(categories~., data = dt4)
imp_var2345<-orderBy(~-attr_importance,imp_var)
important_variables<-rownames(imp_var2345)[1:150]
dt5<-dt4[,important_variables]

data = cbind(dt5,dt4$categories)
names(data)[151] = "categories"


ind = sample(2, nrow(data),replace = T,prob = c(0.75,0.25))
train =  data[ind == 1,]
test =  data[ind == 2,]

colSums(is.na(inp))

#Machine leanrning model using SVM on Both test and train data sets
#------------------------------------------------------------------

library(e1071)
library(caret)

model_cat = svm(train$categories~., train)                    
class_predicted = predict(model_cat,train)
cm = table(train$categories,class_predicted)
accuracy_Cat_train = sum(diag(cm))/sum(cm)

class_predicted = predict(model_cat,test)
cm = table(test$categories,class_predicted)
accuracy_Cat_test = sum(diag(cm))/sum(cm)

model_subCat = svm(train$sub_categories~., train) 

class_predicted = predict(model_subCat,train)
cm = table(train$sub_categories,class_predicted)
accuracy_subcat_train = sum(diag(cm))/sum(cm)

class_predicted = predict(model_subCat,test)
cm = table(test$sub_categories,class_predicted)
accuracy_subcat_test = sum(diag(cm))/sum(cm)

pred_file = cbind(test$sub_categories,class_predicted)

pred_file = as.data.frame(pred_file)
cm = as.data.frame(cm)

accuracy = sum(diag(cm))/sum(cm)
summary(model)

#Fitting logistic regression on training set
#-------------------------------------------
install.packages('arm')
library(randomForest)
class(train$sub_categories)

colSums(is.na(inp))

classifier_subCat = naiveBayes(categories ~., data =  train)
rm(classifier_subCat)
y_pred =  predict(classifier_subCat,train)
cm1 = table(train$categories, y_pred)
acc = sum(diag(cm1))/sum(cm1)

y_pred =  predict(classifier_subCat,train)
cm1 = table(train$categories, y_pred)
acc = sum(diag(cm1))/sum(cm1)

classifier_subCat = naiveBayes(categories ~., data =  train)
rm(classifier_subCat)
y_pred =  predict(classifier_subCat,train)
cm1 = table(train$categories, y_pred)
acc = sum(diag(cm1))/sum(cm1)

y_pred =  predict(classifier_subCat,train)
cm1 = table(train$categories, y_pred)
acc = sum(diag(cm1))/sum(cm1)



