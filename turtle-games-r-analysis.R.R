## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

# 1.  Open RStudio and start setting up the R environment. 

# Determine your working directory
getwd()

# Change your current directory.
setwd("C:/Users/User/OneDrive/LSE Data Analytics/Course 3 Advanced Analytics for Organisational Impact!/Assisgnment 3")

############################################################################

# 2. Import the review.csv data file which is the cleaned version.

# Import a CSV file.
sales1 <- read.csv('reviews.csv', header = TRUE)

# View the head of the data.
head(sales1)

# 3. Import all the required libraries for the analysis and view the data.

# Install the tidyverse library.
install.packages('tidyverse')

# Import the tidyverse library.
library(tidyverse)

# Import the dplyr library.
library(dplyr)

# Install the skimr package.
#install.packages("skimr")  

# Import the ggplot2 library.
library(ggplot2)

# Print the data frame.
View(sales1)

############################################################################

# 4. Load and explore the data.

# Sense-check the data set
# Return the structure of the data frame.
str(sales1)

# Check the type of the data frame.
typeof(sales1)

# Check the class of the data frame.
class(sales1)

# Check the dimensions of the data frame.
dim(sales1)

# Create a summary of the data frame.
summary(sales1)

# Keep only the necessary columns.
sales <- select(sales1, gender, age, income, spend, loyalty, education, product)

# View the data frame.
head(sales)
dim(sales)
summary(sales)

############################################################################

# 2. Perform exploratory data analysis by creating tables and visualisations.

# Aim to better understand groupings and different perspectives on customer behaviour, 
# specifically how loyalty points are accumulated.

# Employ qplot() to visualize the variables within the dataset.

head(sales)

qplot(loyalty, data = sales)

qplot(loyalty, spend, data = sales)

qplot(loyalty, income, data = sales)

qplot(product, data = sales)

qplot(gender, data = sales)

qplot(education, data = sales)

qplot(age, color=education, data = sales)

qplot(age, income, color=education, data = sales)

qplot(loyalty, color=education, data = sales)


############################################################################

# 6. Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.

# Revisiting previous analysis in Python.

# a.Loyalty vs Spend

# a1. Create a scatterplot of loyalty vs spend with no method in geom_smooth() (spline).

# Customizing the theme.

my_theme <- theme(
  text = element_text(size = 12),  # Adjust font size.
  panel.grid.major = element_blank(),  # Remove major grid lines.
  panel.grid.minor = element_blank(),  # Remove minor grid lines.
  plot.title = element_text(hjust = 0.5),  # Center plot title.
  plot.subtitle = element_text(hjust = 0.5)  # Center plot subtitle.
)

# Create the plot.
ggplot(sales, aes(x = spend, y = loyalty)) + 
  geom_point(color = "blue", size = 3, alpha = 0.6) +  
  geom_smooth(color = "red", se = FALSE) +  
  labs(x = "Spending Score", y = "Loyalty Points") +  
  my_theme +  # Apply the custom theme.
  theme(axis.text = element_text(size = 10),  # Set the size of axis text.
        axis.title = element_text(size = 10))  # Set the size of axis titles.
 

# a2. Create a scatter plot of loyalty vs spend based on age. 
ggplot(sales, aes(x = spend, y = loyalty, color = age)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  # Customize colour gradient.
  scale_color_gradient(low = "blue", high = "red") +  
  # Add a title and subtitle and label axes.
  labs(title = "Relationship between Spending Score and Loyalty Points",
       subtitle = "Grouped by Age",
       x = "Spending Score (1-100)", y = "Loyalty Points",
       # Add caption.
       caption = "Assignment 3: G.McGerr") +  
  my_theme  

# Bar plot to show distribution of age. 
ggplot(data = sales, aes(x = age)) +
  geom_histogram(binwidth = 5, 
                 fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency") +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines.   
        panel.grid.minor = element_blank()) 

## The majority of customers fall within the age range of 20 to 60 years.


# a3. Create a scatter plot of loyalty vs spend based on education.
ggplot(sales, aes(x = spend, y = loyalty, color = education)) + 
  geom_smooth(se = FALSE) +  
  scale_color_manual(values = c("blue", "red", "green", "orange", "purple")) +  
  labs(title = "Spending Score and Loyalty Points",
       subtitle = "Grouped by Education",
       x = "Spending Score (1-100)", y = "Loyalty Points") +  
  my_theme +
  guides(color = FALSE) +
  theme(plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0)) +
  theme(axis.text = element_text(size = 10),  # Set the size of axis text.
        axis.title = element_text(size = 10))  # Set the size of axis titles.
 

## Customers with a basic education and who are graduated exhibit the highest loyalty points,
## with an increase corresponding to their spending score.


# Create bar chart to show loyalty vs education.
# Summarizing data to find average loyalty by education.
education_summary <- sales %>%
  group_by(education) %>%
  summarise(total_loyalty = sum(loyalty, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percentage = total_loyalty / sum(total_loyalty) * 100) %>%
  mutate(education = reorder(education, -total_loyalty))

# View thw output.
print(education_summary)

# Create bar chart to show total loyalty vs education.
ggplot(education_summary, aes(x = education, y = total_loyalty, fill = education)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), # Format percentage to one decimal place.
            position = position_stack(vjust = 0.75),    # Center the labels in the middle of the bars.
            color = "black",                          # White text color for better visibility
            size = 3) +                             # Text size
  labs(title = "Total Loyalty by Education Level",
       x = "Education Level",
       y = "Total Loyalty") +
  my_theme +  # Applying your custom theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text = element_text(size = 10),                
        axis.title = element_text(size = 10))   


# b. Loyalty vs Income. 

# b1. Create a scatterplot of loyalty vs income with no method in geom_smooth() (spline).

ggplot(sales, aes(x = income, y = loyalty)) + 
  geom_point(color = "blue", size = 3, alpha = 0.6) +  
  geom_smooth(color = "red", se = FALSE) +  
  labs(x = "Income(k£)",
       y = "Loyalty Points") +  # Remove y-axis label
  my_theme + # Apply the custom theme
theme(axis.text = element_text(size = 10),  # Set the size of axis text.
      axis.title = element_text(size = 10))  # Set the size of axis titles.

# b2. Create a scatter plot of loyalty vs income based on age. 
ggplot(sales, aes(x = income, y = loyalty, color = age)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  # Customize colour gradient.
  scale_color_gradient(low = "blue", high = "red") +  
  # Add a title and subtitle and label axes.
  labs(title = "Relationship between Income and Loyalty Points",
       subtitle = "Grouped by Age",
       x = "Income(k£)", y = "Loyalty Points",
       # Add caption.
       caption = "Assignment 3: G.McGerr") +  
  my_theme  

# b3. Create a scatter plot of loyalty vs income based on education.
ggplot(sales, aes(x = income, y = loyalty, color = education)) + 
  geom_smooth(se = FALSE) +  
  scale_color_manual(values = c("blue", "red", "green", "orange", "purple")) +  
  labs(title = "Income and Loyalty Points",
       subtitle = "Grouped by Education",
       x = "Income", y = "") +  
  my_theme +
  theme(plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0)) +
  theme(axis.text = element_text(size = 10),  # Set the size of axis text.
        axis.title = element_text(size = 10))  # Set the size of axis titles.


# c. Identifying top 10 Products.
# Identify how many different products.
unique(sales$product)     # There are 191 different products.

# Aggregate sales data by product and calculate total sales.
product_sales <- sales %>%
  group_by(product) %>%
  summarise(total_sales = n()) %>%
  arrange(desc(total_sales))  # Arrange in descending order of total sales.

# Select the top 10 products.
top_products <- head(product_sales, 10)

# View the top products.
top_products

# Identify the least selling products. 
# Group by product and count the number of occurrences.
least_selling_products <- sales %>%
  group_by(product) %>%
  summarise(Count = n()) %>%
  arrange(Count) %>%
  slice_head(n = 3)  
# Print the results
print(least_selling_products)

## Top 10 products are  1012, 1031, 979, 977, 107, 123, 195, 231, 249 and 283.
## The least selling products are 254, 453, 466.

# Create simple bar chart to show top 10 products.
# Calculate sales percentage for each product.
top_products <- product_sales %>%
  mutate(sales_percentage = total_sales / sum(total_sales) * 100) %>%
  arrange(desc(total_sales)) %>%
  head(10)

# Create a horizontal bar chart for the top 10 products by sales percentage. 
ggplot(top_products, aes(y = reorder(product, sales_percentage), x = sales_percentage)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Products by Sales Percentage",
       x = "Sales Percentage",
       y = "Product") +
  my_theme

# Create a sales chart by education and gender.
# Set data source, set x-variable, and pass x. 
ggplot(sales, aes(x = education, fill = gender)) +  
  # Specify the geom_bar function.
  geom_bar() +
  # Add title and axis labels.
  labs(title = "Sales by Education and Gender",
       x = "Education",
       y = "Count") +
  my_theme

## The sales analysis conducted on the product column lacks information regarding the quantity of product
## sales, indicating a need for additional data to ensure accurate analysis.

############################################################################

# 7. Note your observations and recommendations to the technical and business users.

## The loyalty levels vary across education levels, with the highest average loyalty 
## observed among individuals with basic education at 27%, followed by 
## graduates at 20%, and both Ph.D. and postgraduate holders at 18%.

## Customers with a basic education and who are graduated exhibit the highest loyalty points,
## with an increase corresponding to their spending score.

## The loyalty-income relationship typically shows a positive correlation,  
## however,  among those with basic education, where loyalty rises steadily 
## with income until £60K. After this point, loyalty growth slows and 
## eventually decreases. This suggests that factors beyond income begin to 
## impact loyalty differently, leading to fluctuations and declining levels.

## The majority of customers fall within the age range of 30 to 50 years.
## There are 191 different products.
## Top 10 products are 1012, 1031, 979, 977, 107, 123, 195, 231, 249 and 283.
## The least selling products are 254, 453, 466. The sales analysis conducted based on product counts;
## lacks information about the quantity of product sales, highlighting the necessity for additional data
## to ensure a comprehensive and accurate analysis.

###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################
################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Your code here.

# 1. Continue with cleaning, manipulating, and visualising the data.

# Import a CSV file.
loyalty1 <- read.csv('reviews.csv', header = TRUE)

# View the head of the data.
head(loyalty1)

# Keep only the necessary columns.
loyalty <- select(loyalty1, gender, age, income, spend, loyalty, education)

# 2. Load and explore the data.

# View the head of the data.

dim(loyalty)
summary(loyalty)


# 3. Perform a statistical analysis.

# Call the function to calculate the mean.
mean(loyalty$spend) 

# Call the function to calculate the median.
median(loyalty$spend) 


# Determine the minimum and maximum value.
min(loyalty$income)  
max(loyalty$income) 


# Range = Max - Min.
max(loyalty$income) - min(loyalty$income) 


# Calculate Q1 and Q3.
quantile(loyalty$income, 0.25)  
quantile(loyalty$income, 0.75)


# Use the summary() function.
summary(loyalty$income)  


# Calculate IQR.
IQR(loyalty$income)  


# Determine the variance.
var(loyalty$income)   


# Return the standard deviation.
sd(loyalty$income)   


###############################################################################

# 3a. Distribution of the data.

# Specify boxplot function.
boxplot(loyalty$income)  


# Specify histogram function.
hist(loyalty$income) 

## On average, customers earn approximately 48K, with a standard deviation of 
## around 23K, indicating variability in income levels. The histogram illustrating 
## income distribution reveals a non-symmetric shape, characterized by a left-skewed 
## distribution. This negative skew suggests that more customers fall within 
## the income range of 20K to 60K compared to those earning above 60K.

###############################################################################

# 3b. Determine normality of data.

# Specify qqnorm function (draw a qqplot).
qqnorm(loyalty$income) 

# Specify qqline function.
qqline(loyalty$income) 

# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(loyalty$income) 

# Install the moments package and load the library.
install.packages('moments') 
library(moments)

# Specify the skewness and kurtosis functions.
skewness(loyalty$income)  
kurtosis(loyalty$income) 


###################################################################################################

## Comments:
## The mean and median income values align closely, indicating an average income of around 48K with 
## a standard deviation of approximately 23K. Descriptive statistics suggest a potential normal 
## distribution with a slight tail. Visual examinations including a boxplot, histogram, and QQ-Plot 
## confirmed the data's normality, supported by the Shapiro-Wilk test. Skewness and kurtosis tests 
## revealed a kurtosis below three (2.591949), indicating a slight tail with no significant outliers.
## The data showed a slight right skewness. Overall, the light tails suggest that extreme income 
## variations were less pronounced than anticipated, implying successful process optimization. 
## This implies that most customers have incomes closer to the centre of the distribution, with 
## fewer individuals having very high incomes.

##################################################################################################

## 4. Create a Multiple linear regression model using income and spend vs loyalty.

##  - Evaluate the goodness of fit and interpret the model summary statistics.
# Create new data frame with numeric values only.
loyalty1 <- subset(loyalty, select = c("age", "income", "spend", "loyalty"))

# Determine correlation between variables.
cor(loyalty1)

# Visualise the correlation.

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function.
# character size (cex=2).
corPlot(loyalty1, cex = 2)

## This confirms the positive correlation between income and loyalty: 0.616 and between spend and loyalty: 0.672.

# Create a new object and specify the lm function and the variables.
loyalty_mlr = lm(loyalty~income+spend, data=loyalty1)

# Print the summary statistics.
summary(loyalty_mlr)

# Create a new object and specify the predict function.
predictTest <- predict(loyalty_mlr, interval = 'confidence')

# Print the object.
predictTest

## The linear regression model suggests that both income and spending have significant positive effects on
## loyalty, with an adjusted R-squared of 0.8267 indicating a strong overall fit of the model to the data.

##  - Create a visual demonstration of the model.

install.packages("ggplot2")
library(ggplot2)

# Update R packages
update.packages(ask = FALSE)

# Create the plot.
loyalty_mlr$spend_income <- loyalty_mlr$spend + loyalty_mlr$income

ggplot(loyalty_mlr, aes(x = spend_income, y = loyalty)) + 
  geom_point(color = "blue", size = 3, alpha = 0.6) +  
  geom_smooth(color = "red", se = FALSE) +  
  labs(title = "Income and Spend Predicting Loyalty",
       x = "Combined Income and Spending Score",
       y = "Loyalty") +
  my_theme +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))

##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.

## The MLR model "Income and Spend Predicting Loyalty" provides a basic understanding of how income 
## and spending relate to loyalty. The MLR model "Income and Spend Predicting Loyalty" 
##  requires consideration of diagnostic tests for multicollinearity and heteroscedasticity. 
##  Specifically, the Breusch-Pagan test identifies heteroscedasticity in the data, 
##  suggesting potential model refinement or robust standard error estimation techniques, 
## which I have conducted in Python previously.

##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).

## Given the MLR model "Income and Spend Predicting Loyalty," we can predict loyalty for 
## various scenarios. For instance, For a customer with an income of £30,000 and a spending 
## score of 50, the model predicts their loyalty score to be approximately 2500. If another 
## customer has an income of £70,000 and a spending score of 80, the predicted loyalty score
## increases to around 5500.

#######################################################################################################

## 5. Perform exploratory data analysis by using statistical analysis methods.

# 5a. Correlation Analysis: Determine if there are any correlations.

# Check correlation between spend and loyalty/ income and loyalty.
# Check for normality in the customers' income and spend values.
shapiro.test(loyalty$income)
shapiro.test(loyalty$spend)
shapiro.test(loyalty$age)

## The output for all is less than 0.05, so the data is not normally distributed.

# Check correlation between loyalty and income/spend using Pearson's correlation.
cor(loyalty$income, loyalty$loyalty)
cor(loyalty$spend, loyalty$loyalty)
cor(loyalty$age, loyalty$loyalty)

## The correlation coefficient between age and loyalty points accumulation is approximately -0.042. 
## This suggests a very weak negative linear relationship between age and loyalty points.
## The correlation coefficient of around 0.616 implies a moderately positive linear association 
## between income and loyalty. Conversely, the correlation coefficient of roughly 0.672 indicates 
## a stronger positive linear relationship between spending and loyalty, surpassing that of income 
## and loyalty. These coefficients suggest that as income or spending rises, loyalty tends to 
## increase as well, with spending showing a somewhat stronger correlation. Consequently, 
## a Multiple Linear Regression (MLR) model constructed using income, spending, and 
## loyalty as predictor variables.

###################################################################################################
# 5b. Segmentation Analysis: K-means customer clustering.

# Install the factoextra package for k-means clustering. 
install.packages('factoextra')

# Import the necessary libraries.
# Visualisation and data wrangling.
library(tidyverse)
# For k-means clustering and visualisation.
library(factoextra) 

# View the top six rows.
head(loyalty1)

# View summary statistics.
summary(loyalty1)

# - Visualise the data.
# Create a scatterplot to view the data set.
# Specify x as sepal_length, y as sepal_width, and color as fruit_type.
ggplot(loyalty1, aes(x=income,
                  y=spend)) +
  geom_point()

# -Explore the data.
# Determine whether there are any missing values.
colSums(is.na(loyalty1)) #No missing values in the data set.

# View the result.
head(loyalty1)

# -Calculate the distance.
# Scale the data set.
loyalty_scale <- scale(loyalty1)

# View the result.
head(loyalty_scale)
loyalty_scale

## The values are now scaled and are much smaller than in the original data frame.

# - Calculate the distance.
# Use the dist() function to calculate distance metrics between observations.
loyalty_data <- dist(loyalty_scale)

# View the result.
head(loyalty_data)

# -Select the number of clusters.

install.packages("factoextra")
library(factoextra)

# Select the optimal number of clusters (k).
fviz_nbclust(loyalty_scale, kmeans, method='wss') +
  labs(subtitle="Elbow method")

# - Create the model.
# Create the k-means model with the kmeans() function.
model <- kmeans(loyalty_scale, centers=5, nstart=100)

# View the output.
print(model)

# - Visualise the output.
# Assign cluster labels to each observation.
loyalty1$cluster <- model$cluster

# Plot the clusters
ggplot(loyalty1, aes(x = income, y = spend, color = factor(cluster))) +
  geom_point(alpha = 0.6, size = 3) +  
  labs(title = "K-Means Clustering by Income and Spending",
       x = "Income",
       y = "Spending",
       color = "Cluster") +
  scale_color_discrete(name = "Cluster") +
  theme_minimal() +  # Applying a minimal theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right") 

## The clustering in R appears less distinct compared to the k-means clustering achieved in Python.

# 5c. Hypothesis Testing: If you have specific hypotheses about the factors influencing 
# loyalty points accumulation, you can conduct hypothesis tests to evaluate these hypotheses. 
# For example, you could test whether there is a significant difference in loyalty points 
# accumulation between different education levels or product categories.

## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. 

## On average, customers earn approximately 48K, with a standard deviation of 
## around 23K, indicating variability in income levels. The histogram illustrating 
## income distribution reveals a non-symmetric shape, characterized by a left-skewed 
## distribution. This negative skew suggests that more customers fall within 
## the income range of 20K to 60K compared to those earning above 60K.

## The linear regression model suggests that both income and spending have significant positive effects on
## loyalty, with an adjusted R-squared of 0.8267 indicating a strong overall fit of the model to the data.

## The mean and median income values align closely, indicating an average income of around 48K with 
## a standard deviation of approximately 23K. Descriptive statistics suggest a potential normal 
## distribution with a slight tail. Visual examinations including a boxplot, histogram, and QQ-Plot 
## confirmed the data's normality, supported by the Shapiro-Wilk test. Skewness and kurtosis tests 
## revealed a kurtosis below three (2.591949), indicating a slight tail with no significant outliers.
## The data showed a slight right skewness. Overall, the light tails suggest that extreme income 
## variations were less pronounced than anticipated, implying successful process optimization. 
## This implies that most customers have incomes closer to the centre of the distribution, with 
## fewer individuals having very high incomes.

## The MLR model "Income and Spend Predicting Loyalty" provides a basic understanding of how income 
## and spending relate to loyalty. The MLR model "Income and Spend Predicting Loyalty" 
## requires consideration of diagnostic tests for multicollinearity and heteroscedasticity. 
## Specifically, the Breusch-Pagan test identifies heteroscedasticity in the data, 
## suggesting potential model refinement or robust standard error estimation techniques, 
## which I have conducted in Python previously.

## Given the MLR model "Income and Spend Predicting Loyalty," we can predict loyalty for 
## various scenarios. For instance, For a customer with an income of £30,000 and a spending 
## score of 50, the model predicts their loyalty score to be approximately 2500. If another 
## customer has an income of £70,000 and a spending score of 80, the predicted loyalty score
## increases to around 5500.

## The correlation coefficient between age and loyalty points accumulation is approximately -0.042. 
## This suggests a very weak negative linear relationship between age and loyalty points.
## The correlation coefficient of around 0.616 implies a moderately positive linear association 
## between income and loyalty. Conversely, the correlation coefficient of roughly 0.672 indicates 
## a stronger positive linear relationship between spending and loyalty, surpassing that of income 
## and loyalty. These coefficients suggest that as income or spending rises, loyalty tends to 
## increase as well, with spending showing a somewhat stronger correlation. Consequently, 
## a Multiple Linear Regression (MLR) model constructed using income, spending, and 
## loyalty as predictor variables.

## The K-means clustering in R appears less distinct compared to the k-means clustering achieved in Python.


###############################################################################
###############################################################################

