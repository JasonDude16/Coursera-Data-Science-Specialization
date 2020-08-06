Weight Lifting Classification
================

# Summary

In this analysis, I created several machine learning models with the
goal of classifying several variations of a weight lifting exercise (a
barbell curl). In some instances, the exercise was performed with good
form, and in other instances the the exercise was intentionally
performed with poor form (i.e. swinging hips). The model was used to
differentiate between 5 variations of the exercise using 3
accelerometers that were placed on the arm, upper arm, and waist
(located anteriorly) of 6 participants. An overview of the dataset and
where it was collected from can be found
[here](http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har).

# Importing and Viewing

The following packages are needed to reproduce the code below. Note that
the seed is set at `123` for this entire analysis.

``` r
set.seed(123)
library(caret) 
library(ggplot2)
library(dplyr)
library(rattle) 
library(plyr) 
library(randomForest)
```

  - The training set can be downloaded
    [here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv).

  - The testing set can be downloaded
    [here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv).

<!-- end list -->

``` r
training <- read.csv("./data/pml-training.csv")
testing <- read.csv("./data/pml-testing.csv")
```

Below are the main functions I used to get a feel for the data. I didn’t
print the results to save space. The full dataset consists of 19622 rows
and 160 columns.

``` r
str(training)
summary(training)
colSums(is.na(training))
training[,sapply(training, is.character)]
```

# Cleaning

The data cleaning process was very straightforward. I converted the
outcome variable to a numeric factor, then removed columns with NAs and
empty character values `""`. This removed nearly half of the variables.
It’s important to note that the variables either had zero NA values or
they had a lot (over 90% of the column was empty); there wasn’t a single
case where a variable had only a few NA values. The columns with mostly
NA values were likely aggregated columns, but I decided not to use these
variables because of the much smaller sample size. After this, the first
few columns were removed which consisted of indexing and time stamps.

``` r
# CREATE DUMMY VARS FOR OUTCOME
training$classe <- mapvalues(training$classe, c("A", "B", "C", "D", "E"), 0:4)
training$classe <- as.factor(training$classe)

# REMOVE NA COLUMNS 
NAs <- unlist(which(colSums(is.na(training)) > 0))
training <- training[-NAs]

# REMOVE CHAR NA COLUMNS 
char_NAs <- unlist(which(sapply(training, is.character)))
training <- training[-char_NAs]

# REMOVE INDEX AND TIMESTAMP COLUMNS
training <- training[-1:-4]
```

After the dataset was cleaned, the variance of the remained variables
was assessed. If the variance was near zero, I would’ve removed the
variables (if it seemed appropriate); but none of the remaining
variables had near zero variance.

``` r
# ASSESS VARIANCE OF REMAINING VARIABLES
nearZeroVar(training)
```

    ## integer(0)

The cleaned dataset consists of 19622 rows and 53 columns. Except for
the outcome variable, all variables were either integer or numeric.

# Exploratory Analysis

Below is a shiny app I created to visualize the data. Histograms,
boxplots, and scatter plots can be created, data can be grouped or
ungrouped, and the scaling can be adjusted. The app initially computed
kmeans classification accuracy for the X and Y variable, but
unfortunately that feature did not work once the app was uploaded to the
shiny server.

<iframe src ="https://jason-dude.shinyapps.io/Visualizing-Weight-Classification/" height=700px width=1000 />

# Modeling

Now we’re on to the fun stuff. This analysis was my first time creating
machine learning models, so I decided to try out several classification
algorithms rather than attempting to tune the parameters of one
algorithm. I wouldn’t know how to properly tune the model, anyway, so
choosing a variety of models sounded best.

### Treebag Model

``` r
mod_treebag <- train(classe ~ ., 
                     data = training, 
                     preProcess = "pca", 
                     method = "treebag")
```

``` r
confusionMatrix(predict(mod_treebag, training), training$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1    2    3    4
    ##          0 5578    0    0    0    0
    ##          1    0 3795    0    0    0
    ##          2    1    2 3422    5    5
    ##          3    1    0    0 3211    0
    ##          4    0    0    0    0 3602
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9993          
    ##                  95% CI : (0.9988, 0.9996)
    ##     No Information Rate : 0.2844          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9991          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
    ## Sensitivity            0.9996   0.9995   1.0000   0.9984   0.9986
    ## Specificity            1.0000   1.0000   0.9992   0.9999   1.0000
    ## Pos Pred Value         1.0000   1.0000   0.9962   0.9997   1.0000
    ## Neg Pred Value         0.9999   0.9999   1.0000   0.9997   0.9997
    ## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1934   0.1744   0.1636   0.1836
    ## Detection Prevalence   0.2843   0.1934   0.1751   0.1637   0.1836
    ## Balanced Accuracy      0.9998   0.9997   0.9996   0.9992   0.9993

### Gradient Boosting Model

``` r
trainctrl <- trainControl(verboseIter = TRUE, number = 5)
mod_gbm <- train(classe  ~ ., 
                 data = training,
                 preProcess = "pca",
                 method = "gbm",
                 verbose = TRUE,
                 trControl = trainctrl)
```

``` r
confusionMatrix(predict(mod_gbm, training), training$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1    2    3    4
    ##          0 5107  382  177  114   74
    ##          1   70 2975  210   35  187
    ##          2  160  313 2920  326  223
    ##          3  195   53   74 2691  130
    ##          4   48   74   41   50 2993
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.8504          
    ##                  95% CI : (0.8453, 0.8553)
    ##     No Information Rate : 0.2844          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8105          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
    ## Sensitivity            0.9152   0.7835   0.8533   0.8368   0.8298
    ## Specificity            0.9468   0.9683   0.9369   0.9724   0.9867
    ## Pos Pred Value         0.8724   0.8556   0.7407   0.8562   0.9336
    ## Neg Pred Value         0.9656   0.9491   0.9680   0.9681   0.9626
    ## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2603   0.1516   0.1488   0.1371   0.1525
    ## Detection Prevalence   0.2983   0.1772   0.2009   0.1602   0.1634
    ## Balanced Accuracy      0.9310   0.8759   0.8951   0.9046   0.9082

### Naive Bayes Model

``` r
trainctrl <- trainControl(number = 5, verboseIter = TRUE)
mod_nb <- train(classe  ~ ., 
                data = training,
                preProcess = "pca",
                method = "nb",
                trControl = trainctrl)
```

``` r
confusionMatrix(predict(mod_nb, training), training$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1    2    3    4
    ##          0 4095  626  749  336  189
    ##          1  284 2307  309  107  467
    ##          2  579  410 2046  425  310
    ##          3  562  207  165 2171  220
    ##          4   60  247  153  177 2421
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.6646          
    ##                  95% CI : (0.6579, 0.6712)
    ##     No Information Rate : 0.2844          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.5748          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
    ## Sensitivity            0.7339   0.6076   0.5979   0.6751   0.6712
    ## Specificity            0.8647   0.9263   0.8936   0.9297   0.9602
    ## Pos Pred Value         0.6831   0.6641   0.5427   0.6529   0.7917
    ## Neg Pred Value         0.8910   0.9077   0.9132   0.9359   0.9284
    ## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2087   0.1176   0.1043   0.1106   0.1234
    ## Detection Prevalence   0.3055   0.1770   0.1921   0.1695   0.1558
    ## Balanced Accuracy      0.7993   0.7669   0.7457   0.8024   0.8157

### Ensemble Model

``` r
boost_pred <- predict(mod_gbm, training)
bagging_pred <- predict(mod_treebag, training)
all_preds <- data.frame(boost_pred, bagging_pred, classe = training$classe)
```

``` r
mod_ensemble <- train(classe ~ ., data = all_preds, method = "treebag")
```

``` r
confusionMatrix(predict(mod_ensemble, training), training$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1    2    3    4
    ##          0 5578    0    0    0    0
    ##          1    0 3795    0    0    0
    ##          2    1    2 3422    5    5
    ##          3    1    0    0 3211    0
    ##          4    0    0    0    0 3602
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9993          
    ##                  95% CI : (0.9988, 0.9996)
    ##     No Information Rate : 0.2844          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9991          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
    ## Sensitivity            0.9996   0.9995   1.0000   0.9984   0.9986
    ## Specificity            1.0000   1.0000   0.9992   0.9999   1.0000
    ## Pos Pred Value         1.0000   1.0000   0.9962   0.9997   1.0000
    ## Neg Pred Value         0.9999   0.9999   1.0000   0.9997   0.9997
    ## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1934   0.1744   0.1636   0.1836
    ## Detection Prevalence   0.2843   0.1934   0.1751   0.1637   0.1836
    ## Balanced Accuracy      0.9998   0.9997   0.9996   0.9992   0.9993

### Random Forest

``` r
mod_rf <- randomForest(classe ~. , data = training, do.trace = TRUE)
```

``` r
confusionMatrix(predict(mod_rf, training), training$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1    2    3    4
    ##          0 5580    0    0    0    0
    ##          1    0 3797    0    0    0
    ##          2    0    0 3422    0    0
    ##          3    0    0    0 3216    0
    ##          4    0    0    0    0 3607
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9998, 1)
    ##     No Information Rate : 0.2844     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##                                      
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2844   0.1935   0.1744   0.1639   0.1838
    ## Detection Prevalence   0.2844   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

### Summary of Models

The `treebag`, `gradient boosting`, and `naive bayes` models were all
pre-processed using principal components analysis. In a way, the
`ensemble` model was as well, because the model combined the `gradient
boosting` and `treebag` models which both used PCA. The models were left
to their default settings, except the `boosting` and `naive bayes` were
both set to `number = 5`. This was done because the models were taking
awhile to finish, so I lowered the number to speed up the process. As
you can see in the printout summaries the `Random Forest` model had
perfect accuracy.

# Results

I implemented the `Random Forest` model for the test set because, after
doing a little more research, it seems that this model performs well
with noisy data. This information, combined with the 100% accuracy on
the training set, made it the sensical choice.

``` r
predictions <- predict(mod_rf, testing)
factor(predictions, labels = c("A", "B", "C", "D", "E"))
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
    ##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
    ## Levels: A B C D E
