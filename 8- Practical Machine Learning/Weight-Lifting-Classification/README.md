# Weight-Lifting-Classification
Analysis link: https://jasondude16.github.io/Weight-Lifting-Classification/

## Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit, it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 

## Analysis 
In this analysis, I created several machine learning models with the goal of classifying several variations of a weight lifting exercise (a barbell curl). In some instances, the exercise was performed with good form, and in other instances the the exercise was intentionally performed with poor form (i.e. swinging hips). The model was used to differentiate between 5 variations of the exercise using 3 accelerometers that were placed on the arm, upper arm, and waist (located anteriorly) of 6 participants. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

## Results 
I created 5 models to use on the training set: treebag, gradient boosting, naive bayes, and a random forest. Of these 5 models, the random forest performed the best, achieving 100% accuracy on the training set (over 19,000 observations of time-series data). I implemented the model on a test set which consisted of 20 observations, and the random forest again achieved 100% accuracy.

## Data
* The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 
* Training data can be downloaded here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
* Test data can be downloaded here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
