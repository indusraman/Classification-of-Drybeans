# Classification-of-Drybeans
# Installing the required packages and loading the libraries

# to read excel files in to R
install.packages("readxl") 
library(readxl)
#The caret package (short for Classification And REgression Training)
#is a set of functions that attempt to streamline the process for creating 
#predictive models. The package contains tools for:data splitting
#pre-processing,feature selection,model tuning using resampling,variable importance estimation

install.packages ("caret")
library(caret)
# graphs in R
install.packages("ggplot2")
library(ggplot2)
install.packages("xtable")
library(xtable)
# reading the excel file in to r 
filename<-"C:/Users/indupravs/Downloads/DataScienceProjects/Dry Bean Dataset/Dry_Bean_Dataset.xlsx"
drybean<-read_excel(filename)
colnames(drybean)

#Create a Validation Dataset
#We will split the loaded dataset into two, 80% of which we will
#use to train our models and 20% that we will hold back as a validation dataset.
# create a list of 80% of the rows in the original dataset we can use for training
validationIndex <- createDataPartition(drybean$Class, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- drybean[-validationIndex,]
# use the remaining 80% of data to training and testing the models
drybean <- drybean[validationIndex,]

# dimensions of dataset
library(vtable)
dim(drybean)
str(drybean)
summary(drybean)
st(drybean)
# list types for each attribute
sapply(drybean,class)
drybean$Class=as.factor(drybean$Class)
# list the levels for the class
levels(drybean$Class)


# Compute percentages
data$fraction = data$count / sum(data$count)


# summarize the class distribution
percentage <- prop.table(table(drybean$Class)) * 100
cbind(freq=table(drybean$Class), percentage=percentage)

# summarize attribute distributions
summary(drybean$Class)

# Plotting the class distribution
data<-data.frame(table(drybean$Class))

dim(data)
colnames(data)
# Compute percentages
data$fraction = data$Freq / sum(data$Freq)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) /2

# Compute a good label
data$label <- paste0(data$Var1, "\n %: ",round((data$fraction*100),digits=1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3.75, fill=Var1)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4))+
  theme_void() +
  theme(legend.position = "none")

print(cbind(Freq=table(drybean$Class), Percentage=round(percentage,digits=2)))

# split input and output
pred <- drybean[,1:16]
res <- drybean[,17]
# boxplot for each attribute on one image
par(mfrow=c(1,6))
for(i in 1:6) {
  boxplot(pred[,i], main=names(dataset)[i+10])
}

# barplot for class breakdown
plot(res)
pred <- drybean[,13:16]
res <- drybean[,17]

# scatter plot matrix
featurePlot(x=pred, y=res$Class, plot="ellipse")
# box and whisker plots for each attribute
featurePlot(x=pred, y=res$Class, plot="box")
#density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=pred, y=res$Class, plot="density", scales=scales)
# Run algorithms using 10-fold cross-validation
trainControl <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# LDA
set.seed(4444)
fit.lda <- train(Class~., data=drybean, method="lda", metric=metric,
                 trControl=trainControl)
# CART
set.seed(4444)
fit.cart <- train(Class~., data=drybean, method="rpart", metric=metric,
                  trControl=trainControl)
# KNN
set.seed(4444)
fit.knn <- train(Class~., data=drybean, method="knn", metric=metric,
                 trControl=trainControl)
# SVM
set.seed(4444)
fit.svm <- train(Class~., data=drybean, method="svmRadial", metric=metric,
                 trControl=trainControl)
# Random Forest
set.seed(4444)
fit.rf <- train(Class~., data=drybean, method="rf", metric=metric, trControl=trainControl)
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
# compare accuracy of models
dotplot(results)
# estimate skill of SVM on the validation dataset
predictions <- predict(fit.svm, validation)
confusionMatrix(predictions, validation$Class)

validation$Class<-as.factor(validation$Class)
confusionMatrix(predictions, validation$Class)

#create the preidctions for the entire set 
filename<-"C:/Users/indupravs/Downloads/DataScienceProjects/Dry Bean Dataset/Dry_Bean_Dataset.xlsx"
drybean<-read_excel(filename)
drybean$Class=as.factor(drybean$Class)
trainControl <- trainControl(method="cv", number=10)
metric <- "Accuracy"

set.seed(4444)
fit.svm <- train(Class~., data=drybean, method="svmRadial", metric=metric,
                 trControl=trainControl)
fit.svm



