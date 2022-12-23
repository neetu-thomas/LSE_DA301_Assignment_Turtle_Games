## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################
# Install the necessary packages

# install.packages('tidyverse')
# install.packages('skimr')
# install.packages('DataExplorer')
# install.packages('moments')
# install.packages('plotly')
#install.packages('psych')


# Import the necessary packages

# Load the packages
library(tidyverse)

# Create statistical summaries
library(skimr)

# Create a report as a HTML file
library(DataExplorer)

# Skewness and Kurtosis
library(moments)

# Interactive ggplots
library(plotly)

library(psych)

# Load and explore the data
sales <- read.csv("turtle_sales.csv", header= TRUE)

# Print the data frame.
head(sales)
dim(sales)
str(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_new <- select(sales, -c(Ranking, Year, Genre, Publisher))

# View the data frame.
head(sales_new)

# View the descriptive statistics.
summary(sales_new)

# Add new column and convert the 'product' column to factor
sales_new <- mutate(sales_new, Product_Id = as.factor(Product))

# Explore the data frame using as.tibble()
as.tibble(sales_new)

glimpse(sales_new)

# Determine if there are missing values
sum(is.na(sales_new))

# View the results
summary(sales_new)

unique(sales$Genre)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

## Global_Sales Vs EU_Sales 

qplot(Global_Sales, EU_Sales, data = sales_new, col=Platform,
      geom=c('point','jitter'),main = 'Global sales Vs EU Sales')

## Global_Sales Vs NA_Sales 
qplot(Global_Sales, NA_Sales, data = sales_new,col = Platform, 
      main = 'Global Sales Vs NA Sales')

qplot(Global_Sales, NA_Sales, data = sales_new, 
      main = 'Global Sales Vs NA Sales')

## EU_Sales Vs NA_Sales
qplot(EU_Sales, NA_Sales, data = sales_new,main='EU Sales Vs NA Sales')


## 2b) Histograms
# Create histograms.
qplot(EU_Sales, data = sales_new, main = 'EU Sales Histogram')

qplot(NA_Sales, data = sales_new, main = 'NA Sales Histogram')

qplot(Global_Sales, data = sales_new, main = 'Global Sales Histogram')
 
## 2c) Boxplots
# Create boxplots.

qplot(EU_Sales, data = sales_new, geom = 'boxplot', main = 'EU Sales Boxplot')


qplot(NA_Sales, data = sales_new, geom ='boxplot', colour = I('red'),
      main = 'NA Sales Boxplot')

qplot(Global_Sales, data = sales_new, geom = 'boxplot',
      main = 'Global Sales Boxplot')

## Additional exploration and insights:

# Comparing the platforms
qplot(Platform, data = sales_new, fill = Platform, main = 'Platform comparison')
qplot(Platform, Global_Sales, data= sales_new, col = Platform,
      main = ' Platform vs Global sales')
qplot(Genre, data = sales, geom='bar', fill=Genre, main = 'Genre comparison')
qplot(Publisher, data = sales, geom='bar', fill = Publisher,
      main = 'Publisher Comparison')




## The X360, PS3 and PC platforms are the most popular followed by DS and Wii.
###############################################################################

# 3. Observations and insights

## The scatterplots of the sales across NA , EU ang global sales show there exists
## a positive correlation. Global sales is impacted more by NA_sales than EU_sales.
## Histograms show NA_sales are more than EU_Sales in the global market.
## Boxplots show there are outliers in the data which tend to skew the results. 

## Additional exploration shows the popular platforms as
## X360, PS3 and PC followed by Wii and Ds.

## Shooter and Action are the popular genres.


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
head(sales_new)

# Check output: Determine the min, max, and mean values of sales data

# EU_Sales:
# Determine minimum, maximum and mean for EU_Sales
min(sales_new$EU_Sales)
max(sales_new$EU_Sales)
mean(sales_new$EU_Sales)

# NA_Sales:
min(sales_new$NA_Sales)
max(sales_new$NA_Sales)
mean(sales_new$NA_Sales)

# Global_Sales:
min(sales_new$Global_Sales)
max(sales_new$Global_Sales)
mean(sales_new$Global_Sales)

# View the descriptive statistics.
summary(sales_new$EU_Sales)
summary(sales_new$NA_Sales)
summary(sales_new$Global_Sales)

## Data Explorer
DataExplorer :: create_report(sales_new)

#skim(sales_new)

## Summary:
# From the summary of sales, it can be seen that Na_Sales is slightly more than 
# EU_Sales.
###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.

# Group data based on Product and determine the sum per Product.
sales_product <- sales_new %>% group_by(Product_Id) %>%
  summarise(Total_EU_Sales = sum(EU_Sales),
            Total_NA_Sales = sum(NA_Sales),
            Total_Global_Sales = sum(Global_Sales),
            Total_Other_Sales = sum(Global_Sales) - sum(EU_Sales) -sum(NA_Sales),
            .groups = 'drop')


# View the data frame.
head(sales_product)

# Explore the data frame.
summary(sales_product)

## Arrange from highest to lowest
arrange(sales_product, desc(Total_Global_Sales))

## top 5 Products
top_products <- head(arrange(sales_product,desc(Total_Global_Sales)), n=5)
top_products



# Group sales based on Platform:
sales_platform<- sales_new %>% group_by(Platform) %>%
  summarise(Total_EU_Sales = sum(EU_Sales),
            Total_NA_Sales = sum(NA_Sales),
            Total_Global_Sales = sum(Global_Sales),
            .groups = 'drop')

head(sales_platform)
top_platforms <- head(arrange(sales_platform,desc(Total_Global_Sales)),n=5)
top_platforms



# Group sales based on Genre:
sales_genre <- sales %>% group_by(Genre) %>%
  summarise(Total_EU_Sales = sum(EU_Sales),
            Total_NA_Sales = sum(NA_Sales),
            Total_Global_Sales = sum(Global_Sales),
            .groups = 'drop')

head(sales_genre)
top_genre <- head(arrange(sales_genre,desc(Total_Global_Sales)),n=5)
top_genre

# Creating a copy of the sales_product data frame
sales_df <- sales_product



###############################################################################


# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots

## Q-Q Plot : EU_total, NA_total, Global_total
qqnorm(sales_df$Total_EU_Sales, 
       main = 'EU Sales Q-Q Plot')
qqline(sales_df$Total_EU_Sales, col = 'red')

qqnorm(sales_df$Total_NA_Sales,
       main = 'NA Sales Q-Q Plot')
qqline(sales_df$Total_NA_Sales, col='red')

qqnorm(sales_df$Total_Global_Sales,
       main = 'Global Sales Q-Q Plot')
qqline(sales_df$Total_Global_Sales, col = 'red')

## The qqplots show that the data is not normally distributed. 
## This can be verified using the Shapio Wilk test

## Perform Shapiro-Wilk test
shapiro.test(sales_df$Total_EU_Sales)

shapiro.test(sales_df$Total_NA_Sales)

shapiro.test(sales_df$Total_Global_Sales)

## The p-value is lesser than 0.05 which mean the data is not normally distributed.
# There tends to be a lot of extreme values or outliers in the distribution.

# Skewness and Kurtosis.

# EU Sales:
skewness(sales_df$Total_EU_Sales)
kurtosis(sales_df$Total_EU_Sales)

# NA Sales
skewness(sales_df$Total_NA_Sales)
kurtosis(sales_df$Total_NA_Sales)

# Global sales
skewness(sales_df$Total_Global_Sales)
kurtosis(sales_df$Total_Global_Sales)

## All sales total across EU, NA and the rest of the world(global) have a positive skewness 
## and a high kurtosis greater than 3.
## This suggests the data is not platykurtic. 
## Data has heavy tail
## Sales data is positively skewed with outliers. 


## 3d) Determine correlation
# Determine correlation.

cor(sales_df$Total_EU_Sales, sales_df$Total_NA_Sales)
cor(sales_df$Total_Global_Sales, sales_df$Total_EU_Sales)
cor(sales_df$Total_Global_Sales, sales_df$Total_NA_Sales)


# The values are positive and closer to 1. This suggests a strong positive correlation.
# If EU or NA sales increase, the Global sales will also increase. 
# NA has a very strong positive correlation with the global sales.

# Correlation values between the EU, NA and Global sales total suggests a positive 
# correlation 
###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Global Sales Vs European Sales 
ggplot(data = sales_df, 
       aes(x = Total_Global_Sales, y = Total_EU_Sales)) + 
  geom_point(color ='blue', alpha = 0.5, size = 2) + 
  geom_smooth(method = 'lm', se=FALSE, col='red') +
  scale_x_continuous(breaks = seq(0, 80, 5)) + 
  scale_y_continuous(breaks = seq(0, 50, 5)) + 
  labs(title = 'Relationship between Global and European Sales',
     x = 'Global Sales in millions (£)',
     y = 'EU Sales in millions (£)',
     col = 'Sales rank of Products', 
     caption = 'Data:turtle_sales.csv') 


# Global Vs NA
ggplot(data = sales_df, 
       mapping = aes(x = Total_Global_Sales, y = Total_NA_Sales)) + 
  geom_point(color = 'blue', alpha = 0.5, size = 2) + 
  geom_smooth(method = 'lm', se=FALSE, col='red') +
  scale_x_continuous(breaks = seq(0, 80, 5)) + 
  scale_y_continuous(breaks = seq(0, 50, 5)) + 
  labs(title = 'Relationship between Global and North American Sales',
     x = 'Global Sales in millions (£)',
     y = 'NA Sales in millions (£)',
     caption = 'Data:turtle_sales.csv'
)

# EU Vs NA
ggplot(data = sales_df, 
       mapping = aes(x = Total_EU_Sales, y = Total_NA_Sales)) + 
  geom_point(color = 'blue', alpha = 0.5, size = 2) + 
  geom_smooth(method = 'lm', se=FALSE, col='red') +
  scale_x_continuous(breaks = seq(0, 80, 5)) + 
  scale_y_continuous(breaks = seq(0, 50, 5)) + 
  labs(title = 'Relationship between European and North American Sales',
       x = 'EU Sales in millions (£)',
       y = 'NA Sales in millions (£)',
       caption = 'Data:turtle_sales.csv'
  )

# Create histograms.

## EU sales:
ggplot(sales_df, aes(x=Total_EU_Sales)) + 
  geom_histogram() + 
  labs(title = 'Total Sales by Product in Europe',
       x = 'Total Sales in EU in millions £',
       y = 'Count')

# NA_Sales:
ggplot(sales_df, aes(x=Total_NA_Sales)) + 
  geom_histogram() + 
  labs(title = 'Total Sales by Product in North America',
       x = 'Total Sales in NA in millions £',
       y = 'Count')

# Other sales:
ggplot(sales_df, aes(x=Total_Other_Sales)) + 
  geom_histogram() + 
  labs(title = 'Total Sales by Product- rest of the world',
       x = 'Total Sales in other areas in millions £',
       y = 'Count')

## Global_Sales : Boxplot(ggplot)
ggplot(data = sales_df, aes(Total_Global_Sales)) + 
  geom_boxplot(color = I('blue'),outlier.colour = 'blue', outlier.size = 2) +
  labs(title = 'Boxplot for Global Sales by product') + 
  theme_classic()

## EU_Sales : Boxplot(ggplot)
ggplot(data = sales_df, aes(Total_EU_Sales)) + 
  geom_boxplot() +
  labs(title = 'Boxplot for EU Sales by product') 

## NA_Sales : Boxplot(ggplot)
ggplot(data = sales_df, aes(Total_NA_Sales)) + 
  geom_boxplot(color = I('blue'),outlier.colour = 'blue', outlier.size = 2) +
  labs(title = 'Boxplot for NA Sales by product')

library(reshape2)
head(sales_df)
sales_group <- select(sales_product, Total_Global_Sales,Total_NA_Sales, Total_EU_Sales)
sales_group <- melt(sales_group)
head(sales_group)
summary(sales_group)

# Distribution of sales across different geographical areas
ggplot(sales_group, aes(x = value, y = variable))+
  geom_boxplot()+
  labs(title = 'Distribution of Sales by Products', 
       x = 'Sales in Millions £',
       y = 'Sales by Geographical Area')

## NA sales are higher than EU Sales




# Interactive Scatterplot between total North American and European Sales based on Genre
plot_genre <- ggplot(sales_genre, aes(x=Total_NA_Sales, y=Total_EU_Sales, 
                                      fill=Genre, col=Total_Global_Sales)) + 
  geom_point(size=2.5) + 
  labs(title='NA and EU Sales by Genre')

ggplotly(plot_genre)

## 


# Interactive Scatterplot between total North American and European Sales 
# based on Platform
plot_platform <- ggplot(sales_platform, aes(x=Total_NA_Sales,y=Total_EU_Sales, 
                                            fill=Platform, col=Total_Global_Sales)) + 
  geom_point(size=2.5) + 
  labs(title='NA and EU Sales by Platform')

ggplotly(plot_platform)

## Additional plots:

# Top selling products based on Global sales: 

ggplot(data = top_products, aes(fct_reorder(Product_Id, -Total_Global_Sales),
                                Total_Global_Sales, fill = Product_Id)) + 
  geom_col() + 
  labs(title = 'Top selling products based on Global Sales',
       x = 'Product Id',
       y = 'Total Global Sales' ) + 
  theme_minimal()

# Top Genre based on Global sales:
ggplot(data = sales_genre,
       aes(x = reorder(Genre, Total_Global_Sales,sum),
           y = Total_Global_Sales, fill = Genre)) + 
  geom_col() + 
  labs(title = 'Top Genre based on Global Sales',
       x = 'Genre',
       y = 'Total Global Sales' ) + 
  coord_flip() + 
  theme_minimal()


# Top Platforms based on Global sales:

ggplot(data = top_platforms,
       aes(x = reorder(Platform, -Total_Global_Sales,sum),
           y = Total_Global_Sales, 
           fill = Platform)) + 
  geom_col() + 
  labs(title = 'Top Platforms based on Global Sales',
       x = 'Platform',
       y = 'Total Global Sales' ) 

###############################################################################

# 5. Observations and insights

## The sales data are grouped based on products. QQ plots and Shapiro Wilk test
## show data is not normally distributed.
## Skewness is positive and  high Kurtosis > 3 with heavy tail/outliers.
## The top selling product ids are 107, 515, 123, 254 and 195
## The top plaforms are Wii, X360  PS3, Ds and GB
## the top genres are Shooter, Platform, Action, Role playing and Sports. 



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
head(sales_df) 
# Determine a summary of the data frame.

summary(sales_df)
###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(sales_df$Total_EU_Sales, sales_df$Total_NA_Sales)
cor(sales_df$Total_Global_Sales, sales_df$Total_EU_Sales)
cor(sales_df$Total_Global_Sales, sales_df$Total_NA_Sales)

# Create a linear regression model 

# Global sales - EU Sales : 
model_EU <- lm(Total_Global_Sales ~ Total_EU_Sales, sales_df)

# Plot residuals
plot(model_EU$residuals, pch =20,
     main = 'Residual Plot for Global Sales-EU_Sales')

# Summary of the model:
summary(model_EU)

## EU_total sales is highly significant in determining the Global sales. 
# Multiple R-squared 72% variability in global Sales is explained by 
# EU Sales.
##########################################

# Global sales - NA Sales : 
model_NA <- lm(Total_Global_Sales ~ Total_NA_Sales, sales_df)

# Plot residuals
plot(model_NA$residuals, col = 'brown', pch =20,
     main = 'Residual Plot for Global Sales-NA_Sales')

# Summary of the model:
summary(model_NA)

# Highly significant. 83.9% variability in Global Sale sis explained by 
# variability in NA_Sales.

# EU sales - NA Sales:
model1 <- lm(Total_EU_Sales ~ Total_NA_Sales, sales_df)

# Plot residuals
plot(model1$residuals, col = 'blue', pch =20,
     main = 'Residual Plot for EU sales-NA_Sales')

summary(model1)

## Only 38.5% variability is explained by sales in the NA for sales in EU. 


## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# EU sales model
plot(sales_df$Total_EU_Sales, sales_df$Total_Global_Sales,
     main = 'Global_Sales Vs EU_Sales',
     xlab = 'Total EU Sales',
     ylab = ' Total Global Sales',
     col = 'blue', pch = 20)
#plot(model_EU$residuals)
coefficients(model_EU)

# Add line of best fit
abline(coefficients(model_EU), col = 'red')


## NA_Sales model:
plot(sales_df$Total_NA_Sales, sales_df$Total_Global_Sales,
     main = 'Global_Sales Vs NA_Sales',
     xlab = 'Total NA Sales',
     ylab = ' Total Global Sales',
     col = 'blue', pch = 20)

# Add line of best fit
abline(coefficients(model_NA), col ='red')

## NA-EU sales
plot(sales_df$Total_EU_Sales, sales_df$Total_NA_Sales,
     main = 'EU_Sales Vs NA_Sales',
     xlab = 'Total EU Sales',
     ylab = ' Total NA Sales', col = 'blue', pch = 20)
# Add line of best fit
abline(coefficients(model1), col = 'red')
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
colnames(sales_df)

sales_num <- select(sales_df, c(Total_EU_Sales, Total_NA_Sales, Total_Global_Sales))
head(sales_num)

#cor(sales_num)

library(psych)
# Use the corPlot() function to create a correlation matrix
# Specify the data frame (sales_num) and set 
# character size (cex=2).
corPlot(sales_num, cex=2)

## From the corplot, NA_Sales and Global_Sales have a higher correlation (0.93)
# Strong positive correlation.
# It clearly shows as NA_sales increases,Global_Sales increase. 
# EU_sales and Global_Sales also have a positive correlation.


head(sales_new)

# Multiple linear regression model.
mlr_model = lm(Total_Global_Sales ~ Total_EU_Sales+ Total_NA_Sales, data = sales_df)

summary(mlr_model)

## Using original data set : sales_new without grouping by product

mlr_2 = lm(Global_Sales ~ EU_Sales+NA_Sales, data = sales_new)
summary(mlr_2)

## The coefficients of EU and NA sales have stars(***) meaning they are highly 
## significant in determining the dependent variable(Global_Sales).
## The adjusted R2of 96.6% explains the variability in the Global sales. 
###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
tail(sales_num)
sales_forecast <- data.frame(Total_EU_Sales = c(23.80,1.56,0.65,0.97,0.52),
                            Total_NA_Sales = c(34.02,3.93,2.73,2.26,22.08))
sales_forecast
predict_global <- predict(mlr_model,
                          newdata=sales_forecast)


sales_forecast <- mutate(sales_forecast,
                         Predicted_Global = predict_global)
sales_forecast
###############################################################################

# 5. Observations and insights
# Your observations and insights here...

## EU_total sales is significant in determining the Global sales. 
# Multiple R-squared 72% variability in global Sales is explained by 
# EU Sales.

# NA_Sales: Highly significant. 83.9% variability in Global Sales is explained by 
# variability in NA_Sales.

## NA -EU do not have correlation.(weak) 

## Multiple linear regression has R2 of 96.6% - means more accurate the model 
# in predicting global sales.

## The global sales can be predicted using the mlr model of EU and NA sales.


###############################################################################
###############################################################################




