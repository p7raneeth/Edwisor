
    # Loading the data into R Environment and installing required libraries
    #----------------------------------------------------------------------
    data1 = read.csv('TextClassification_Data.csv',stringsAsFactors = F)
    install.packages('FSelector')
    library(tm)
    library(FSelector)
    library(rJava)
    
    # Data Visualisation 
    #-------------------
    library(sqldf)
    
    dt4$categories =  factor(as.character(dt4$categories))
    dt4$sub_categories =  factor(as.character(dt4$sub_categories))
    dt4$previous_appointment =  factor(as.character(dt4$previous_appointment))
    
    v1 = sqldf('select categories, count(categories) as cnt from dt4 group by categories')
    v2 = sqldf('select sub_categories, count(sub_categories) as cnt from dt4 group by sub_categories')
    v3 = sqldf('select previous_appointment, count(previous_appointment) as cnt from dt4 group by previous_appointment')
    
 
    
    library(plotly)
    
    p <- plot_ly(
      x = v1$categories,
      y = v1$cnt,
      name = "Count of various Categories",
      type = "bar"
    )

    q <- plot_ly(
      x = v2$sub_categories,
      y = v2$cnt,
      name = "Count of various Sub-Categories",
      type = "bar"
    )
    
    r <- plot_ly(
      x = v3$previous_appointment,
      y = v3$cnt,
      name = "Count of Previous Appointment",
      type = "bar"
    )
    
    
    barplot(v3$cnt, main = "Count of Previous appointment", 
            names.arg = c('APPOINTMENTS','ASK_A_DOCTOR','JUNK','LAB','MISCELLANEOUS',
                          'PRESCRIPTION'))
  
    
    # Cleaning Data and Performing TF-IDF on the Existing Text
    #---------------------------------------------------------
    
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

    names(data3)[1:10]
    
    dt4 = data3[,-c(1,2,3,7,78)]
    
    
    
    levels(dt4$categories)[5] = "MISCELLANEOUS"
    # levels(dt4$previous_appointment)[2] = "YES"
    
    imp_var = random.forest.importance(categories+sub_categories~., data = dt4)
    
    
    rownames(imp_var)
    important_variables<-rownames(imp_var2345)[1:150]
    
    dt5<-dt4[,important_variables]
    data = cbind(dt5,dt4$categories)
    
    
    data$sub_categories
    
    data$sub_categories =  factor(as.character(data$sub_categories))
    data$categories =  factor(as.character(data$categories))
    data$previous_appointment =  factor(as.character(data$previous_appointment))
    
    
    levels(data$previous_appointment)
    # [8] = "MEDICATION RELATED"
    levels(data)
    str()
    str(data$categories)[5] = "MISCELLANEOUS"
    
    names(data)[151] = "categories"
    
    ind = sample(2, nrow(data),replace = T,prob = c(0.75,0.25))
    train =  data[ind == 1,]
    test =  data[ind == 2,]
    
    
    train$previous_appointment =  factor(as.character(train$previous_appointment))
    
    
    library(e1071)
    library(caret)
    
    levels(test$previous_appointment)[2] = "NO"
    str(test$categories)
    
    model_cat = svm(categories~., train)                    
    
    class_predicted = predict(model_cat,test)
    
    str(test)
    
    
    
    
    pred_file = cbind(train$categories,class_predicted)
    
    cm = data.frame(cm)
    
    cm = table(test$categories,class_predicted)
    accuracy = sum(diag(cm))/sum(cm)
    summary(model)