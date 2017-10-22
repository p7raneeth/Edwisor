#---------------------------------------------
# Read data from csv file

data1 = read.csv('TextClassification_Data.csv',stringsAsFactors = F)

# data1 = data1

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
data2 <- cbind(data1, as.matrix(dtm1))


ind = sample(2, nrow(data2),replace = T,prob = c(0.7,0.3))
train =  data2[ind == 1,]
test =  data2[ind == 2,]

install.packages('caTools')
library(caTools)

str(train$)

test$categories =  factor(as.character(test$categories))
train$categories =  factor(as.character(train$categories))

class(tr1$addit)
rm(model);gc();
tr1 = train[,-c(2,3,4,5)]
str(tr1)
mat = cor(train[,-c(2,3,4,5)])

model = fit(categories ~ ., data = tr1 ,model = 'svm',kpar = list(sigma = 0.10),C = 2 )

summary(model)
naive.importance = Importance(model, data = train[,-c(2,3,4,5)])
varImp(model)
methods(varImp)
