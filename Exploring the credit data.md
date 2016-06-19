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

