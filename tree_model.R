EnsurePackage<-function(x)
{ # EnsurePackage(x) - Installs and loads a package
  # if necessary
  x <- as.character(x)
  if (!require(x, character.only=TRUE))
  {
    install.packages(pkgs=x,
                     repos="http://cran.r-project.org")
  }
  require(x, character.only=TRUE)
  
}

#Installs and loads all packages necessary
#
Prepare.models<-function(){
  
  EnsurePackage("ISLR")
  EnsurePackage("MASS")
  EnsurePackage("mosaic")
  EnsurePackage("mosaicData")
  EnsurePackage("rpart")
  EnsurePackage("tidyr")
}

Prepare.models()


library(ElemStatLearn)
data(spam)
# Exploratory Analysis
?spam
ncol(spam)
head(spam)
table(spam[58])
summary(spam[58]
        )
# description of the dataset

# This is a quantitative collection of 4601 observations of emails which contain spam emails and non spam emails. A total of 2788 non spam emails and 1813 spam emails. Each row is description of an email, with precentages of key words in the email as attributes among other things. The total number of attributes of 'spam' dataset is 58 among this the first 57 are numerical vectors while the last column contains a factor label, that describes if the mail is a spam or not. 

#The first 48 columns contain continuous real attributes,[0,100], which describes the percentage of words in the email that match a WORD in the email. What the word is can be given as the name of the attribute. A standard format is to use word\_freq\_WORD. The next 6 attributes are also real and continous,[0,100], that gives percentage of number of characters in the email that match a particular CHAR. The next 3 attributes are continuous real variables, [1,...], which describe average length of uninterupted sequence of capital letters, length of the longest uninterrupted sequence of capital letters, total number of capital letters in the e-mail. 

#There are no null values in this dataset.

# plot of spam/non-spam
count.table <- as.data.frame(table(spam[58]))
?plot

plot(spam$spam)


# splitting data into training and testing

set.seed(123)
?sample
ind.spam <- sample(2, nrow(spam), replace = TRUE, prob = c(0.7, 0.3))
spam.train <- spam[ind.spam ==1,]
spam.test <- spam[ind.spam ==2,]


# creating a model using rpart
spam.model.rpart <- rpart(spam ~ ., method = "class", data = spam.train, cp = 0.001)

plotcp(spam.model.rpart)

printcp(spam.model.rpart)

# minimum xerror from cptable = 0.21671, correponding cp = 0.0015760

# pruning the tree with the correponding cp
spam.model.pruned <- prune(spam.model.rpart, cp = 0.00157)
plot(spam.model.pruned, uniform = TRUE, main = "Classification Tree Model using rpart")
text(spam.model.pruned, all = TRUE, cex = 0.65)
print(spam.model.pruned)

# prediction
spam.predict.rpart <-predict(spam.model.pruned, newdata=spam.test, type = 'class')
table(spam.predict.rpart, spam.test$spam)
mean(spam.predict.rpart == spam.test$spam) # classification mean
rpart.error <- mean(spam.predict.rpart != spam.test$spam) # miss classification mean - testing error -rpart

rpart.error
