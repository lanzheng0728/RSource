Exploring the credit data
100xp
We will examine the dataset loan_data discussed in the video further throughout the exercises.

Given loan_data, you're particularly interested in the defaulted loans in the data set, so you want to get an idea of the number and percentage of defaults. Default happens rarely, so you always want to check what the proportion of defaults is in a loan data set. The CrossTable() function is very useful here.

Remember that default information is stored in the response variable loan_status, where 1 represents default and 0 non-default.

To learn more about variable structures and spot unexpected tendencies in the data, you should examine the relationship between loan_status and certain factor variables. For example, you would expect that the proportion of defaults in the group of customers with grade G (worst credit rating score) is substantially higher than the proportion of defaults in the grade A group (best credit rating score).

Conveniently, CrossTable() can also be applied on two categorical variables. Let's explore!

Instructions
Get familiar with the dataset by looking at its structure with str().
Load the gmodels package using library(). It is already installed on DataCamp's servers.
Have a look at the CrossTable() of loan status, using just one argument: loan_data$loan_status.
Call CrossTable() with x argument loan_data$grade and y argument loan_data$loan_status. We only want row-wise proportions, so set prop.r to TRUE, but prop.c , prop.t and prop.chisq to FALSE (default values here are TRUE, and this would lead to inclusion of column proportions, table proportions and chi-square contributions for each cell. We do not need these here.)
 Take Hint (-30xp)
script.R


# View the structure of loan_data
str(loan_data)
# Load the gmodels package 
library(gmodels)
# Call CrossTable() on loan_status
CrossTable(loan_data$loan_status)
# Call CrossTable() on grade and loan_status
CrossTable(loan_data$grade,loan_data$loan_status,
prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Histograms
100xp
The data set loan_data is loaded in your workspace. You previously explored categorical variables using the CrossTable() function. Now you would like to explore continuous variables to identify potential outliers or unexpected data structures.

To do this, let's experiment with the function hist() to understand the distribution of the number of loans for different customers.

Instructions
Use hist() to create a histogram with only one argument: loan_data$loan_amnt. Assign the result to a new object called hist_1.
Use $breaks along with the object hist_1 to get more information on the histogram breaks. Knowing the location of the breaks is important because if they are poorly chosen, the histogram may be misleading.
Change the number of breaks in hist_1 to 200 by specifying the breaks argument. Additionally, name the x-axis "Loan amount" using the xlab argument and title it "Histogram of the loan amount" using the main argument. Save the result to hist_2. Why do the peaks occur where they occur?
 Take Hint (-30xp)
script.R


# Create histogram of loan_amnt: hist_1
hist_1<-hist(loan_data$loan_amnt)
# Print locations of the breaks in hist_1
hist_1 <- hist(loan_data$loan_amnt,breaks=200)
# Change number of breaks and add labels: hist_2
hist_2 <- hist(loan_data$loan_amnt, breaks = "200", xlab = "Loan amount", main = "Histogram of the loan amount")
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Outliers
100xp
Now, let's have a look at the structure of variable age. A histogram is plotted on your right. Similar to what we observed in the video for annual income, there is a lot of blank space on the right-hand side of the plot. This is an indication of possible outliers. We will look at a scatterplot to verify this, then delete the outlier(s).

If outliers are observed for several variables, it might be useful to look at bivariate plots. It's possible the outliers belong to the same observation. If so, there is even more reason to delete the observation because it is then more likely that some information stored in it is wrong.

Instructions
Build a scatterplot of the variable age (through loan_data$age) using the function plot(). Give the y-axis the appropriate label "Age" using ylab as a second argument,.
The oldest person in this data set is older than 122 years! Get the index of this outlier using which() and the age of 122 as a cutoff (you can do this using loan_data$age > 122). Assign it to the object index_highage.
Create a new data set new_data, after removing the observation with the high age using the object index_highage.
Have a look at the bivariate scatterplot, with age on the x-axis and annual income on the y-axis. Change the labels to "Age" and "Annual Income", respectively.
 Take Hint (-30xp)
script.R

# Plot the age variable
plot(loan_data$age,ylab="Age")
# Save the outlier's index to index_highage
index_highage<-which(loan_data$age>122)
# Create data set new_data with outlier deleted
new_data <- loan_data[-index_highage, ]
# Make bivariate scatterplot of age and annual income
plot(loan_data$age, loan_data$annual_inc, xlab = "Age", 
ylab = "Annual Income")
 Submit Answer
Plots
1/1
R Console
Slides
>  

DataCampCourse Outline
Deleting missing data
100xp
As we have seen before, the interest rate int_rate in the data set loan_data depends on the customer. Unfortunately, these interest rates are missing for some observations. Let's see how many of them are missing, then try to delete them.

Instructions
Take a look at the number of missing inputs for the variable int_rate using summary().
Use which() and is.na() to get the indices of the observations without a recorded interest rate. Store the result in an object na_index.
Create a new data set called loan_data_delrow_na, which does not contain the observations with missing interest rates.
Recall that we made a copy of loan_data called loan_data_delcol_na. Instead of deleting the observations with missing interest rates, delete the entire int_rate column by setting it equal to NULL.
 Take Hint (-30xp)
script.R


# Look at summary of loan_data
summary(loan_data$int_rate)
# Get indices of missing interest rates: na_index
na_index <- which(is.na(loan_data$int_rate))
# Remove observations with missing interest rates: loan_data_delrow_na
loan_data_delrow_na <- loan_data[-na_index, ]
# Make copy of loan_data
loan_data_delcol_na <- loan_data
# Delete interest rate column from loan_data_delcol_na
loan_data_delcol_na$int_rate<-NULL
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Replacing missing data
100xp
Rather than deleting the missing interest rates, we may just want to replace them. The object na_index, which contains the indices of the observations with missing interest rates, is still loaded in your workspace.

Instructions
Create an object called median_ir, containing the median of the interest rates in loan_data using the median() function. Don't forget to include the argument na.rm = TRUE.
In the new data set loan_data_replace, replace all the missing instances in the indices stored in object na_index with the median of all the interest rates, median_ir.
Have a look at the variable int_rate in the new data set using summary() to make sure that the NAs are gone.
 Take Hint (-30xp)
script.R


# Compute the median of int_rate
median_ir<-median(loan_data$int_rate,na.rm=T)
# Make copy of loan_data
loan_data_replace <- loan_data
# Replace missing interest rates with median
loan_data_replace$int_rate[na_index] <- median_ir
# Check if the NAs are gone
summary(loan_data_replace$int_rate)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Replacing missing data
100xp
Rather than deleting the missing interest rates, we may just want to replace them. The object na_index, which contains the indices of the observations with missing interest rates, is still loaded in your workspace.

Instructions
Create an object called median_ir, containing the median of the interest rates in loan_data using the median() function. Don't forget to include the argument na.rm = TRUE.
In the new data set loan_data_replace, replace all the missing instances in the indices stored in object na_index with the median of all the interest rates, median_ir.
Have a look at the variable int_rate in the new data set using summary() to make sure that the NAs are gone.
 Take Hint (-30xp)
script.R


# Compute the median of int_rate
median_ir<-median(loan_data$int_rate,na.rm=T)
# Make copy of loan_data
loan_data_replace <- loan_data
# Replace missing interest rates with median
loan_data_replace$int_rate[na_index] <- median_ir
# Check if the NAs are gone
summary(loan_data_replace$int_rate)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Replacing missing data
100xp
Rather than deleting the missing interest rates, we may just want to replace them. The object na_index, which contains the indices of the observations with missing interest rates, is still loaded in your workspace.

Instructions
Create an object called median_ir, containing the median of the interest rates in loan_data using the median() function. Don't forget to include the argument na.rm = TRUE.
In the new data set loan_data_replace, replace all the missing instances in the indices stored in object na_index with the median of all the interest rates, median_ir.
Have a look at the variable int_rate in the new data set using summary() to make sure that the NAs are gone.
 Take Hint (-30xp)
script.R


# Compute the median of int_rate
median_ir<-median(loan_data$int_rate,na.rm=T)
# Make copy of loan_data
loan_data_replace <- loan_data
# Replace missing interest rates with median
loan_data_replace$int_rate[na_index] <- median_ir
# Check if the NAs are gone
summary(loan_data_replace$int_rate)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Creating a confusion matrix
100xp
The test set is available in your workspace. Recall that the observed outcome or the actual loan status (default versus non-default) can be found in the loan_status column. Assume we have run a model, and the predicted outcomes according to the model are stored in a vector called model_pred. Recall the confusion matrix structure:



and formulas:

Classification accuracy=(TP+TN)(TP+FP+TN+FN)
Classification accuracy=(TP+TN)(TP+FP+TN+FN)
Sensitivity=TP(TP+FN)
Sensitivity=TP(TP+FN)
Specificity=TN(TN+FP)
Specificity=TN(TN+FP)
Instructions
Create a confusion matrix comparing the loan_status column in test_set with the vector model_pred. You can use the table() function with two arguments to do this. Store the matrix in object conf_matrix.
Compute the classification accuracy and print the result. You can either select the correct matrix elements from the confusion matrix using conf_matrix, or copy and paste the desired values.
Compute the sensitivity and print the result.
 Take Hint (-30xp)
script.R


# Create confusion matrix
test_set$model_pred<-test_set$loan_status
conf_matrix<-table(test_set$loan_status,model_pred)
# Compute classification accuracy
TN<-conf_matrix[1,1]
TP<-conf_matrix[2,2]
FN<-conf_matrix[2,1]
FP<-conf_matrix[1,2]
(TN+TP)/(TN+FN+TP+FP)
# Compute sensitivity
TP/(TP+FN)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Basic logistic regression
100xp
In the video, we looked at a logistic regression model including the variable age as a predictor. Now, you will include a categorical variable, and learn how to interpret its parameter estimates.

When you include a categorical variable in a logistic regression model in R, you will obtain a parameter estimate for all but one of its categories. This category for which no parameter estimate is given is called the reference category. The parameter for each of the other categories represents the odds ratio in favor of a loan default between the category of interest and the reference category. Don't worry if this doesn't make complete sense to you yet, we'll do more exercises on this later on!

Instructions
Construct a logistic regression model called log_model_cat with the categorical variable ir_cat as the only predictor. Your call to glm() should contain three arguments:
loan_status ~ ir_cat
family = "binomial"
data = training_set
View the result in the console to see your parameter estimates.
Find out what the reference category is by looking at the structure of ir_cat (in the full data set loan_data) again. Use the table() function to do this.
 Take Hint (-30xp)
script.R


# build a glm model with variable ir_cat as a predictor
log_model_cat<-glm(formula=loan_status~ir_cat,family="binomial",data=training_set)
# Print the parameter estimates 
print(log_model_cat)
# Look at the different categories in ir_cat using table()
table(loan_data$ir_cat)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Predicting the probability of default
100xp
In the video we looked at the predicted probability of default for one case in the test set. Luckily, you can predict the probability for all the test set cases at once using the predict() function.

After having obtained all the predictions for the test set elements, it is useful to get an initial idea of how good the model is at discriminating by looking at the range of predicted probabilities. A small range means that predictions for the test set cases do not lie far apart, therefore the model might not be very good at discriminating good from bad customers. With low default percentages, you will notice that in general, very low probabilities of default are predicted. Let's have a look at a first model.

log_model_small is loaded in the workspace.

Instructions
The code for the prediction of test_case in the video is copied in your workspace. Change the code such that the function predict() is applied to all cases in test_set. You can store them in the object predictions_all_small.
Get an initial idea of how well the model can discriminate using range()
 Take Hint (-30xp)
script.R


# Build the logistic regression model
predictions_all_small <- predict(log_model_small, newdata = test_set, type = "response")
# Look at the range of the object "predictions_all_small"
range(predictions_all_small)
 Submit Answer
R Console
Slides
>  




DataCampCourse Outline
Making more discriminative models
100xp
In the previous exercise, the range for predicted probabilities of default was rather small. As discussed, small predicted default probabilities are to be expected with low default rates, but building bigger models (which basically means: including more predictors) can expand the range of your predictions.

Whether this will eventually lead to better predictions still needs to be validated and depends on the quality of the newly included predictors. But let us first have a look at how bigger models can expand the range.

Instructions
Make log_model_full like the way you made log_model_small, but this time, include all available predictors in the data set. If you don't want to type the name of every column seperately, you can simply select all variables using loan_status ~ .
Create your prediction vector predictions_all_full for all the cases in the test set. Notice that these values represent the probability of defaulting.
Look at the range of the predictions.
 Take Hint (-30xp)
script.R


# Change the code below to construct a logistic regression model using all available predictors in the data set
log_model_full <- glm(loan_status ~. , family = "binomial", data = training_set)
# Make PD-predictions for all test set elements using the the full logistic regression model
predictions_all_full <- predict(log_model_full,test_set,type="response")
# Look at the predictions range
range(predictions_all_full)
 Submit Answer
R Console
Slides
>  


DataCampCourse Outline
Specifying a cut-off
100xp
We have shown you how the specification of a cut-off can make the difference to obtain a good confusion matrix. Now, you will learn how to transform the prediction vector to a vector of binary values indicating the status of the loan. The ifelse() function in R can help you here.

Applying the ifelse() function in the context of a cut-off, you would have something like

ifelse(predictions > 0.3, 1, 0)
In the first argument, you are testing whether a certain value in the predictions-vector is bigger than 0.3. If this is TRUE, R returns "1" (specified in the second argument), if FALSE, R returns "0" (specified in the third argument), representing "default" and "no default", respectively.

Instructions
The code for the full logistic regression model along with the predictions-vector is given in your console.
Using a cutoff of 0.15, create vector pred_cutoff_15 using the the ifelse() function and predictions_all_full.
Look at the confusion matrix (enter the true values, so test_set$loan_status, in the first argument).
 Take Hint (-30xp)
script.R

# The code for the logistic regression model and the predictions is given below
log_model_full <- glm(loan_status ~ ., family = "binomial", data = training_set)
predictions_all_full <- predict(log_model_full, newdata = test_set, type = "response")
# Make a binary predictions-vector using a cut-off of 15%
pred_cutoff_15<-ifelse(predictions_all_full>0.15,1,0)
# Construct a confusion matrix
table(test_set$loan_status,pred_cutoff_15)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Comparing link functions for a given cut-off
100xp
In this last exercise, you will fit a model using each of the three link functions (logit, probit and cloglog), make predictions for the test set, classify the predictions in the appropriate group (default versus non-default) for a given cut-off, make a confusion matrix and compute the accuracy and sensitivity for each of the models given the cut-off value! Wow, you've learned a lot so far. And finally, you will try to identify the model that performs best in terms of accuracy given the cut-off value!

It is important to know that the differences between the models will generally be very small, and again, the results will depend on the chosen cut-off value. The observed outcome (default versus non-default) is stored in true_val in the console.

Instructions
Fit three logistic regression models using links logit, probit and cloglog respectively. Part of the code is given. Use age, emp_cat, ir_cat and loan_amnt as predictors.
Make predictions for all models using the test_set.
Use a cut-off value of 14% to make predictions for each of the models, such that their performance can be evaluated.
Make a confusion matrix for the three models.
Lastly, compute the classification accuracy for all three models.
 Take Hint (-30xp)
script.R


# Fit the logit, probit and cloglog-link logistic regression models
log_model_logit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = logit), data = training_set)
log_model_probit <- glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = probit), data = training_set)
log_model_cloglog <-  glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                       family = binomial(link = cloglog), data = training_set)
  
# Make predictions for all models using the test set
predictions_logit <- predict(log_model_logit, newdata = test_set, type = "response")
predictions_probit <- predict(log_model_probit, newdata = test_set, type = "response")
predictions_cloglog <- predict(log_model_cloglog, newdata = test_set, type = "response")
  
# Use a cut-off of 14% to make binary predictions-vectors
cutoff <- 0.14
class_pred_logit <- ifelse(predictions_logit > cutoff, 1, 0)
class_pred_probit <- ifelse(predictions_probit > cutoff, 1, 0)
class_pred_cloglog <- ifelse(predictions_cloglog > cutoff, 1, 0)
  
# Make a confusion matrix for the three models
tab_class_logit <- table(true_val,class_pred_logit)
tab_class_probit <- table(true_val,class_pred_probit)
tab_class_cloglog <- table(true_val,class_pred_cloglog)
  
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Computing the gain for a tree
100xp
In the video, we looked at how the gini-measure is used to create the perfect split for a tree. Now, you will compute the gain for the tree loaded in your workspace.

The data set contains 500 cases, 89 of these cases are defaults. This led to a Gini of 0.292632 in the root node. Compute the gini in the left hand and the right hand node, and the gain of the two leaf nodes with respect to the root node. The object containing the tree is small_tree.

As a small reminder, remember that

Gini of a certain node = 2 * proportion of defaults in this node * proportion of non-defaults in this node.

Instructions
The computation for the Gini of the root node is given.
Compute the Gini measure for the left leaf node.
Compute the Gini measure for the right leaf node.
Compute the gain by taking the difference between the root node Gini and the weighted leaf node Gini measures.
Information regarding the split in this tree can be found using $split and the tree object, small_tree. Instead of gain, you should look at the improve column here. improve is an alternative metric for gain, simply obtained by multiplying gain by the number of cases in the data set. Make sure that the object improve (code given) has the same value as in small_tree$split.
 Take Hint (-30xp)
script.R


# The Gini-measure of the root node is given below
gini_R <- 2 * 89 / 500 * 411 / 500
# Compute the Gini measure for the left leaf node
gini_ll <-2 * 401/446 * 45/446
# Compute the Gini measure for the right leaf node
gini_rl <- 2 * 10/54 * 44 /54
# Compute the gain
gain <- gini_R - 446 / 500 * gini_ll - 54 / 500 * 
gini_rl
# compare the gain-column in small_tree$splits with our 
computed gain, multiplied by 500, and assure they are 
the same
small_tree$splits
improve <- gain * 500
 Submit Answer
Plots
Previous Plot 2/2
R Console
Slides
>  

DataCampCourse Outline
Undersampling the training set
100xp
In the video, you saw that to overcome the unbalanced data problem, you can use under- or oversampling. We have undersampled the training set for you, such that 1/3 of the training set consists of defaults, and 2/3 of non-defaults. The resulting data set is available in your workspace and named undersampled_training_set, and contains less observations (6570 instead of 19394). In this exercise, you will create a decision tree using the undersampled data set.

You will notice that the trees in this and the next exercises are very big, so big that you cannot really read them anymore. Don't worry about this for now, we will tell you how you can make them more manageable in the next video!

Instructions
The rpart() package has been installed for you. Load the package in your workspace.
Change the code provided such that a decision tree is constructed using the undersampled training set instead of training_set. Additionally, add the argument control = rpart.control(cp = 0.001). cp, which is the complexity parameter, is the threshold value for a decrease in overall lack of fit for any split. If cp is not met, further splits will no longer be pursued. cp's default value is 0.01, but for complex problems, it is advised to relax cp.
Plot the decision tree using the function plot and the tree object name. Add a second argument uniform = TRUE to get equal-sized branches.
Add labels to the tree using function text() and the decision tree object name.
 Take Hint (-30xp)
script.R


# Load package rpart() in your workspace.
library(rpart)
# Change the code provided in the video such that a decision tree is constructed using the undersampled training set. Include rpart
.control to relax the complexity parameter to 0.001.
tree_undersample <- rpart(loan_status ~ ., method = "class", control = rpart.control(cp = 0.001),
                          data =  undersampled_training_set)
# Plot the decision tree
plot(tree_undersample,uniform = TRUE)
# Add labels to the decision tree 
text(tree_undersample)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Changing the prior probabilities
100xp
As mentioned in the video, you can also change the prior probabilities to obtain a decision tree. This is an indirect way of adjusting the importance of misclassifications for each class. You can specify another argument inside rpart() to include prior probabities. The argument you are looking for has the following form

parms = list(prior=c(non_default_proportion, default_proportion))
The rpart package is now already loaded in your workspace.

Instructions
Change the code provided such that a decision tree is constructed , including the argument parms and changing the proportion of non-defaults to 0.7, and of defaults to 0.3 (they should always sum up to 1). Additionally, include control = rpart.control(cp = 0.001) as well.
Plot the decision tree using the function plot and the tree object name. Add a second argument "uniform=TRUE" to get equal-sized branches.
Add labels to the tree using function text() and the decision tree object name.
 Take Hint (-30xp)
script.R


# Change the code below such that a tree is constructed with adjusted prior probabilities.
tree_prior <- rpart(loan_status ~ ., method = "class", parms = list(prior=c(0.7, 0.3)),
control = rpart.control(cp = 0.001),data = training_set)
# Plot the decision tree
plot(tree_prior,uniform=TRUE)
# Add labels to the decision tree 
text(tree_prior)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Including a loss matrix
100xp
Thirdly, you can include a loss matrix, changing the relative importance of misclassifying a default as non-default versus a non-default as a default. You want to stress that misclassifying a default as a non-default should be penalized more heavily. Including a loss matrix can again be done in the argument parms in the loss matrix.

parms = list(loss = matrix(c(0, cost_def_as_nondef, cost_nondef_as_def, 0), ncol=2))
Doing this, you are constructing a 2x2-matrix with zeroes on the diagonal and changed loss penalties off-diagonal. The default loss matrix is all ones off-diagonal.

Instructions
Change the code provided such a loss matrix is included, with a penalization that is 10 times bigger when misclassifying an actual default as a non-default. This can be done replacing cost_def_as_nondef by 10, and cost_nondef_as_def by 1. Similar to what you've done in the previous exercises, include rpart.control to relax the complexity parameter to 0.001.
Plot the decision tree using the function plot and the tree object name. Add a second argument uniform = TRUE to get equal-sized branches.
Add labels to the tree using function text() and the decision tree object name.
 Take Hint (-30xp)
script.R


# Change the code below such that a decision tree is constructed using a loss matrix penalizing 10 times more heavily 
for misclassified defaults.
tree_loss_matrix <- rpart(loan_status ~ ., method = "class", 
parms = list(loss = matrix(c(0, 10, 1, 0), ncol=2)),
control = rpart.control(cp = 0.001),
                          data =  training_set)
# Plot the decision tree
plot(tree_loss_matrix,uniform=TRUE)
# Add labels to the decision tree 
text(tree_loss_matrix)
 Submit Answer
R Console
Slides
> 

DataCampCourse Outline
Pruning the tree with changed prior probabilities
100xp
In the video, you have learned that pruning a tree is necessary to avoid overfitting. We had some big trees in the previous exercises. Now you will put what you have learned into practice and prune the previously constructed tree with the changed prior probabilities. The rpart package is already loaded in your workspace.

You will first set a seed to make sure the results are reproducible. As mentioned in the video, because you will be examining cross-validated error results, the results involve randomness and could differ slightly upon running the function again using another seed.

Instructions
Run the code to set a seed and construct tree_prior again.
Use function plotcp() to identify where the minimum cross-validated error is located for tree_prior.
Use printcp() to identify the complexity parameter that minimizes the cross-validated error.
In the script, we provided the code so you can automatically retrieve the cp for which the cross-validated error is minimized.
Use the prune()-function to obtain the pruned tree. Call the pruned tree ptree_prior.
Package rpart.plot is loaded in your workspace. plot the pruned tree using function prp() (default setting).
 Take Hint (-30xp)
script.R


# Set a seed and run the code to construct the tree with changed prior probabilities again
set.seed(345)
tree_prior <- rpart(loan_status ~ ., method = "class", data = training_set, 
                    parms = list(prior = c(0.7, 0.3)), 
                    control = rpart.control(cp = 0.001))
# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_prior)
# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized
printcp(tree_prior)
# The function below can be used to automatically obtain the cp that minimizes the cross-validated error
tree_min <- tree_prior$cptable[which.min(tree_prior$cptable[ ,"xerror"]), "CP"]
#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp =tree_min)
# Use prp() to plot the pruned tree
prp(ptree_prior)
 Submit Answer
R Console
Slides
>  

DataCampCourse Outline
Pruning the tree with the loss matrix
100xp
In this exercise, you will prune the tree that was built using a loss matrix in order to penalize misclassified defaults more than misclassified non-defaults.

Instructions
Run the code to set a seed and construct tree_loss_matrix again.
Use function plotcp() to examine the cross-validated error-structure.
Looking at the cp-plot, you will notice that pruning the tree using the minimum cross-validated error will lead to a tree that is as big as the unpruned tree, as the cross-validated error reaches its minimum for cp = 0.001. Because you would like to make the tree somewhat smaller, try pruning the tree using cp = 0.0012788. For this complexity parameter, the cross-validated error approaches the minimum observed error. Call the pruned tree ptree_loss_matrix.
Package rpart.plot is loaded in your workspace. Plot the pruned tree using function prp() (including argument extra = 1).
 Take Hint (-30xp)
script.R

# set a seed and run the code to construct the tree with the loss matrix again
set.seed(345)
tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set, 
                           parms = list(loss=matrix(c(0, 10, 1, 0), ncol = 2)),
                           control = rpart.control(cp = 0.001))
# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_loss_matrix)
# Prune the tree using cp = 0.0012788
ptree_loss_matrix <- prune(tree_loss_matrix, cp = 0.0012788)
# Use prp() and argument extra = 1 to plot the pruned tree
prp(ptree_loss_matrix,extra = 1)
 Submit Answer
R Console
Slides
> 

DataCampCourse Outline
Computing a bad rate given a fixed acceptance rate.
100xp
In the video, you learned how you can compute the bad rate (or, in other words, the percentage of defaults) in the loan portfolio of a bank given

a specific model
the acceptance rate
In this exercise, you will compute the bad rate a bank can expect when using the pruned tree ptree_prior fitted before, and an acceptance rate of 80%. As a reminder, the tree is plotted on your right hand side.

Instructions
In the script, we provided the code to make predictions for the probability of default using the pruned tree and test_set. Remember that if you use the predict()-function for a tree, the probability of default can be found in the second column. Therefore we pasted [,2] to the predict()-function.
Obtain the cut-off that leads to an acceptance rate of 80%, using prob_default_prior. You can use the quantile()- function to do this, setting the second argument to 0.8. Assign the name cutoff_prior.
Obtain the actual binary default predictions (0 or 1) using cutoff_prior. You can use the function ifelse() here. Name the object bin_pred_prior_80.
The code to select the default indicators of test_set for the accepted loans acording to a 80% acceptance rate is provided.
Compute the percentage of defaults (or the "bad rate") for the accepted loans. This is the number of occurences of 1 in accepted_status_prior_80, divided by the total number of instances in this vector. Print the solution to your R-console.
 Take Hint (-30xp)
script.R


# Make predictions for the probability of default using the pruned tree and the test set.
prob_default_prior <- predict(ptree_prior, newdata = test_set)[ ,2]
# Obtain the cutoff for acceptance rate 80%
  cutoff_prior<-quantile(prob_default_prior,0.8)
# Obtain the binary predictions.
bin_pred_prior_80<-ifelse(prob_default_prior>cutoff_prior,1,0)
# Obtain the actual default status for the accepted loans
accepted_status_prior_80 <- test_set$loan_status[bin_pred_prior_80 == 0]
# Obtain the bad rate for the accepted loans
as.numeric(table(accepted_status_prior_80))[2]/length(accepted_status_prior_80)
 Submit Answer
R Console
Slides
> 

