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
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)
library(skimr)
library(DataExplorer)
library(moments)
library(forecast)
library(ggpmisc)
library(reshape2)

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=TRUE)

# Print the data frame.
sales

# summarise initial dataframe
summary(sales)
## product column is a number, i think it would work better as a string as it is
## essentially a categorical variable
## also 2 NAs in Year - will have a look at these

# convert product to string
sales$Product <- as.character(sales$Product)
summary(sales)

# Look at the Nas
filter(sales, is.na(Year))

# impacts 2 products, have a look at them
filter(sales, Product %in% c(948, 7141))
## these two products have entries in Year in other rows, so replace with them

sales <- within(sales, Year[is.na(Year) & Product == 948] <- 2010)
sales <- within(sales, Year[is.na(Year) & Product == 7141] <- 2003)

# check it worked
filter(sales, Product %in% c(948, 7141))

# summary the df again
summary(sales)

# I want to add a column to show "Other Sales" (Global - EU - NA)
sales$Other_Sales <- round(sales$Global_Sales - sales$NA_Sales - sales$EU_Sales, 2)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_1 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
sales_1

# View the descriptive statistics.
summary(sales_1)
skim(sales_1)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

# NA_Sales v EU_Sales

ggplot(data=sales_1, mapping=aes(x=EU_Sales, y=NA_Sales, color=Platform)) +
  geom_point(alpha=0.8, size=5) +  
  labs(title= "Relationship between EU and NA sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="EU_Sales",
       y="NA_Sales") +
  scale_fill_brewer('set2')

#Global_Sales v NA_Sales

ggplot(data=sales_1, mapping=aes(x=NA_Sales, y=Global_Sales, color=Platform)) +
  geom_point(alpha=0.8, size=4) +  
  labs(title= "Relationship between NA and Global sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="NA_Sales",
       y="Global_Sales") +
  scale_fill_brewer('set2')

#Global_Sales v EU_Sales

ggplot(data=sales_1, mapping=aes(x=EU_Sales, y=Global_Sales, color=Platform)) +
  geom_point(alpha=0.8, size=4) +  
  labs(title= "Relationship between EU and Global sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="EU_Sales",
       y="Global_Sales") +
  scale_fill_brewer('set2')


## 2b) Histograms
# Create histograms.

# EU Sales Histogram
ggplot(data=sales_1, mapping=aes(x=EU_Sales)) +
  geom_histogram(fill= 'darkblue', binwidth=2, alpha=0.6) +
  geom_vline(aes(xintercept=median(EU_Sales)),
             color="blue", linetype="dashed", size=1) +
  labs(title= "EU Sales Histogram",
       caption="Source: Turtle_Sales.csv. 
       Dashed Line = Median",
       x="EU_Sales",
       y="Count",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# NA Sales Histogram
ggplot(data=sales_1, mapping=aes(x=NA_Sales)) +
  geom_histogram(fill= 'darkblue', binwidth=2, alpha=0.6) +
  geom_vline(aes(xintercept=median(NA_Sales)),
             color="blue", linetype="dashed", size=1) +
  labs(title= "NA Sales Histogram",
       caption="Source: Turtle_Sales.csv.
       Dashed Line = Median",
       x="NA_Sales",
       y="Count",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Other Sales Histogram
ggplot(data=sales_1, mapping=aes(x=Other_Sales)) +
  geom_histogram(fill= 'darkblue', binwidth=1, alpha=0.6) +
  geom_vline(aes(xintercept=median(Other_Sales)),
             color="blue", linetype="dashed", size=1) +
  labs(title= "Other Sales Histogram",
       caption="Source: Turtle_Sales.csv.
       Dashed Line = Median",
       x="Other_Sales",
       y="Count",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Global Sales Histogram
ggplot(data=sales_1, mapping=aes(x=Global_Sales)) +
  geom_histogram(fill= 'darkblue', binwidth=3, alpha=0.6) +
  geom_vline(aes(xintercept=median(Global_Sales)),
             color="blue", linetype="dashed", size=1) +
  labs(title= "Global Sales Histogram",
       caption="Source: Turtle_Sales.csv.
       Dashed Line = Median",
       x="Global_Sales",
       y="Count",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## 2c) Boxplots
# Create boxplots.

# EU Sales Boxplot
ggplot(data=sales_1, mapping=aes(x=EU_Sales)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "EU Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="EU_Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# NA Sales Boxplot
ggplot(data=sales_1, mapping=aes(x=NA_Sales)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "NA Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="NA_Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# Other Sales Boxplot
ggplot(data=sales_1, mapping=aes(x=Other_Sales)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "Other Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="Other_Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# Global Sales Boxplot
ggplot(data=sales_1, mapping=aes(x=Global_Sales)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "Global Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="Global_Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# Attempt to plot all on one Graph using Facet

# reshape the data and save as new df
sales_melt <- melt(sales_1,id.vars='Product', measure.vars=c('Global_Sales',
                                                             'NA_Sales',
                                                             'EU_Sales',
                                                             'Other_Sales'))

# check df
head(sales_melt)

# plot boxplots
ggplot(data=sales_melt, mapping=aes(x=value)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "Total Turtle Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  facet_wrap(~variable)

## after all that not very helpful due to scales! :)

# standard bar plots

# Global Sales by Platform
ggplot(data=sales_1, mapping=aes(x=reorder(Platform, Global_Sales,sum), y=Global_Sales, fill=Platform)) +
  geom_bar(stat='sum')  +
  geom_text(
    aes(label = after_stat(y), group = Platform), 
    stat = 'summary', fun = sum, hjust=-0.1, vjust=0.25, size=3.5) +
  labs(title="Total Global Sales by Platform",
       caption="Source: Turtle_Sales.csv",
       x="Platform",
       y="Global Sales") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") 

# Global Sales by Genre - use original df for this
ggplot(data=sales, mapping=aes(x=reorder(Genre, Global_Sales,sum), y=Global_Sales, fill=Genre)) +
  geom_bar(stat='sum')  +
  geom_text(
    aes(label = after_stat(y), group = Genre), 
    stat = 'summary', fun = sum, hjust=0, vjust=0.25, size=3.5) +
  labs(title="Total Global Sales by Genre",
       caption="Source: Turtle_Sales.csv",
       x="Genre",
       y="Global Sales") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") 


# Global Sales by Publisher - use original df for this
ggplot(data=sales, mapping=aes(x=reorder(Publisher, Global_Sales,sum), y=Global_Sales, fill=Publisher)) +
  geom_bar(stat='sum')  +
  geom_text(
    aes(label = after_stat(y), group = Publisher), 
    stat = 'summary', fun = sum, hjust=0, vjust=0.25, size=3.5) +
  labs(title="Total Global Sales by Publisher",
       caption="Source: Turtle_Sales.csv",
       x="Publisher",
       y="Global Sales") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") 

# Global Sales % by Region 
ggplot(data=filter(sales_melt, variable != "Global_Sales"), 
       mapping=aes(x="", y=value, fill=variable)) +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous (label = scales::percent, breaks=seq(0, 1, 0.1)) +
  labs(title="Total Global Sales Share by Region",
       caption="Source: Turtle_Sales.csv",
       x="",
       y="% of Sales",
       fill = "Region") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) +
  theme_classic()
###############################################################################

# Sales by Year 

# melt df
sales_melt2 <- melt(sales,id.vars='Year', measure.vars=c('Global_Sales',
                                                             'NA_Sales',
                                                             'EU_Sales',
                                                         'Other_Sales'))

# group the df by year
sales_melt2_grouped <- aggregate(value~Year + variable, sales_melt2, sum)

# check df
sales_melt2_grouped

# plot line chart
ggplot(data=sales_melt2_grouped, mapping=aes(x=Year, y=value, group=variable)) +
  geom_line(aes(col=variable), size=1.5)  +
  #  geom_text(
  #    aes(label = after_stat(y), group = Year), 
  #    stat = 'summary', fun = sum, hjust=0, vjust=0.25, size=3.5) +
  scale_x_continuous(breaks=seq(0, 2016, 2)) +
  scale_y_continuous(breaks=seq(0, 170, 20)) +
  labs(title="Sales by Year Game Released",
       caption="Source: Turtle_Sales.csv",
       x="Year",
       y="Sales £m") +
  theme_classic()


# working out average sales per year

# assuming current year is 2015, as this is the latest material year in data
# melt the dataframe again, in a different way
sales_melt3 <- melt(sales,id.vars=c('Product', 'Genre', 'Publisher', 'Year'), 
                    measure.vars='Global_Sales')

# check dataframe
head(sales_melt3)

# create new column to calculate average sales per year, assuming now is 2015
sales_melt3$avg_sales_per_year <- round(sales_melt3$value/(2015 - sales_melt3$Year),2)

# manually overwrite the single 2016b game in the data
sales_melt3['avg_sales_per_year'][sales_melt3['avg_sales_per_year'] == 'Inf'] <- 0.35


# see top ten products
top_n(sales_melt3,10,avg_sales_per_year)

# not sure this is useful, will use first "time-series" looking chart

###############################################################################

# 4. Observations and insights

## Data looks relatively clean to start with
## Not sure why we remove publisher and genre - seem like helpful columns?

## Scatterplots
##  - EU v NA - positive correlation, looks like low correlation as dots are 
## spread quite widely and a few big outliers that sold much more in one region 
## than the other
## - NA v Globa - closer positive correlation, fewer outliers
## - EU v Global - positive correlation, not as strong as NA v Global, a few 
## outliers that sold well in Global but not EU

## Histograms
## All data is strongly positively skewed, with long tails and a reasonable 
## amount of, and large in size, outliers. This is potentially to be expected 
## if these are total sales for all games given some have been on sale for 30 
## years and some for 1 or 2.

## Boxplots
## These back up the eyeballing of the histograms, several outliers exist in all
## columns

## More info/further analysis
## Look at the genre/year/platform/publisher of the data to see what is causing 
## the outliers? I think adding the categorical variables would improve insight.
## Standard bar plots show the platforms with the most sales - Turtle could ensure 
## focus on the big ones/quickest growing and identify those no longer in service
## Shooters are the most popular genre. ~75% sales come from top 5 genres - Shooter
## Platform, Action, RPG, Sports.
## Games from mid 00s to early 2010s the biggest selling - does this mean we are 
## seeing a decline in recent years? 2015 looks to be the most recently completed 
## full year -potentially look at this in terms of sales per year (assume in 2015) 
## to normalize the data a little?
## Nintendo by far the highest selling publisher - again be interesting to see 
## this for games released over time.
## 47% sales come from NA, 31% EU and 22% other





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
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales_1)

# Check output: Determine the min, max, and mean values.
min(sales_1$NA_Sales)
min(sales_1$EU_Sales)
min(sales_1$Global_Sales)
min(sales_1$Other_Sales)

max(sales_1$NA_Sales)
max(sales_1$EU_Sales)
max(sales_1$Global_Sales)
max(sales_1$Other_Sales)

mean(sales_1$NA_Sales)
mean(sales_1$EU_Sales)
mean(sales_1$Global_Sales)
mean(sales_1$Other_Sales)

median(sales_1$NA_Sales)
median(sales_1$EU_Sales)
median(sales_1$Global_Sales)
median(sales_1$Other_Sales)


# View the descriptive statistics.
summary(sales_1)

###############################################################################


# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
## note grouping on products because some products sell on multiple platforms.
sales_1_grouped <- aggregate(cbind(NA_Sales, EU_Sales, Global_Sales, Other_Sales)
                             ~Product, sales_1, sum)


# View the data frame.
sales_1_grouped

# Explore the data frame.
dim(sales_1_grouped)
## has shortened the lengtrh of the df to 175 from 352
skim(sales_1_grouped)
## all product values unique
summary(sales_1_grouped)
## some of the desc stats changed due to groupingeg. mean

###############################################################################
# 3. Plot the data
# Create plots to gain insights into data.
# Repeat charts from week 4

# Scatterplots

# NA Sales v EU Sales
ggplot(data=sales_1_grouped, mapping=aes(x=EU_Sales, y=NA_Sales)) +
  geom_point(alpha=0.8, size=5) +  
  labs(title= "Relationship between EU and NA sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="EU_Sales",
       y="NA_Sales") +
  scale_fill_brewer('set2') +
  theme_classic()


#Global_Sales v NA_Sales
ggplot(data=sales_1_grouped, mapping=aes(x=NA_Sales, y=Global_Sales)) +
  geom_point(alpha=0.8, size=4) +  
  labs(title= "Relationship between NA and Global sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="NA_Sales",
       y="Global_Sales") +
  scale_fill_brewer('set2') +
  theme_classic()

#Global_Sales v EU_Sales
ggplot(data=sales_1_grouped, mapping=aes(x=EU_Sales, y=Global_Sales)) +
  geom_point(alpha=0.8, size=4) +  
  labs(title= "Relationship between EU and Global sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="EU_Sales",
       y="Global_Sales") +
  scale_fill_brewer('set2') +
  theme_classic()

# Histograms
# EU Sales Histogram
ggplot(data=sales_1_grouped, mapping=aes(x=EU_Sales)) +
  geom_histogram(fill= 'darkblue', binwidth=2, alpha=0.6) +
  geom_vline(aes(xintercept=median(EU_Sales)),
             color="blue", linetype="dashed", size=1) +
  labs(title= "EU Sales Histogram",
       caption="Source: Turtle_Sales.csv. 
       Dashed Line = Median",
       x="EU_Sales",
       y="Count",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# NA Sales Histogram
ggplot(data=sales_1_grouped, mapping=aes(x=NA_Sales)) +
  geom_histogram(fill= 'darkblue', binwidth=2, alpha=0.6) +
  geom_vline(aes(xintercept=median(NA_Sales)),
             color="blue", linetype="dashed", size=1) +
  labs(title= "NA Sales Histogram",
       caption="Source: Turtle_Sales.csv.
       Dashed Line = Median",
       x="NA_Sales",
       y="Count",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Other Sales Histogram
ggplot(data=sales_1_grouped, mapping=aes(x=Other_Sales)) +
  geom_histogram(fill= 'darkblue', binwidth=1, alpha=0.6) +
  geom_vline(aes(xintercept=median(Other_Sales)),
             color="blue", linetype="dashed", size=1) +
  labs(title= "Other Sales Histogram",
       caption="Source: Turtle_Sales.csv.
       Dashed Line = Median",
       x="Other_Sales",
       y="Count",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Global Sales Histogram
ggplot(data=sales_1_grouped, mapping=aes(x=Global_Sales)) +
  geom_histogram(fill= 'darkblue', binwidth=3, alpha=0.6) +
  geom_vline(aes(xintercept=median(Global_Sales)),
             color="blue", linetype="dashed", size=1) +
  labs(title= "Global Sales Histogram",
       caption="Source: Turtle_Sales.csv.
       Dashed Line = Median",
       x="Global_Sales",
       y="Count",) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Boxplots
# EU Sales Boxplot
ggplot(data=sales_1_grouped, mapping=aes(x=EU_Sales)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "EU Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="EU_Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# NA Sales Boxplot
ggplot(data=sales_1_grouped, mapping=aes(x=NA_Sales)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "NA Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="NA_Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# Other Sales Boxplot
ggplot(data=sales_1_grouped, mapping=aes(x=Other_Sales)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "Other Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="Other_Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# Global Sales Boxplot
ggplot(data=sales_1_grouped, mapping=aes(x=Global_Sales)) +
  geom_boxplot(fill='darkblue', outlier.color='red', alpha=0.6) +
  labs(title= "Global Sales Boxplot",
       caption="Source: Turtle_Sales.csv",
       x="Global_Sales") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

###############################################################################
# Determine the normality of the data set.

## 4a) Create Q-Q Plots
# Create Q-Q Plots.

# EU sales
qqnorm(sales_1_grouped$EU_Sales, main="EU Sales QQ Plot")
# Add line to show normal distribution
qqline(sales_1_grouped$EU_Sales, col='red')

# NA sales
qqnorm(sales_1_grouped$NA_Sales, main="NA Sales QQ Plot")
# Add line to show normal distribution
qqline(sales_1_grouped$NA_Sales, col='red')

# Other sales
qqnorm(sales_1_grouped$Other_Sales, main="Other Sales QQ Plot")
# Add line to show normal distribution
qqline(sales_1_grouped$Other_Sales, col='red')

# Global sales
qqnorm(sales_1_grouped$Global_Sales, main="Global Sales QQ Plot")
# Add line to show normal distribution
qqline(sales_1_grouped$Global_Sales, col='red')

## 4b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.
# EU
shapiro.test((sales_1_grouped$EU_Sales))
# NA
shapiro.test((sales_1_grouped$NA_Sales))
# Other
shapiro.test((sales_1_grouped$Other_Sales))
# Global
shapiro.test((sales_1_grouped$Global_Sales))

## null hypothesis states the data is normally distributed
## p values all under 0.05
## we can reject the null hypothesis and state data is not normally distributed


## 4c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# EU
skewness(sales_1_grouped$EU_Sales)
kurtosis(sales_1_grouped$EU_Sales)
# NA
skewness(sales_1_grouped$NA_Sales)
kurtosis(sales_1_grouped$NA_Sales)
# Other
skewness(sales_1_grouped$Other_Sales)
kurtosis(sales_1_grouped$Other_Sales)
# Global
skewness(sales_1_grouped$Global_Sales)
kurtosis(sales_1_grouped$Global_Sales)



## 4d) Determine correlation
# Determine correlation.
cor(sales_1_grouped$EU_Sales, sales_1_grouped$NA_Sales)
cor(sales_1_grouped$EU_Sales, sales_1_grouped$Global_Sales)
cor(sales_1_grouped$NA_Sales, sales_1_grouped$Global_Sales)
cor(sales_1_grouped$Other_Sales, sales_1_grouped$Global_Sales)

###############################################################################
# 5. Create Plots to Gain Insights into the Data
## the best plot types to use to identify relationships between the sales
## columns are scatterplots, with lines of best fit to illustrate the correlation.
## these plot types show how one of the variables changes in relation to the other.
## and how much of the change in one variable can be said to be down to the other
## variable (r^2).
## i improved the visuals by changing colours and adding best fit line
## plus i thought it heped to show the r^2 so found a package that could do that
## within stat called ggpmisc

ggplot(data=sales_1_grouped, mapping=aes(x=EU_Sales, y=NA_Sales)) +
  geom_point(alpha=0.8, size=5, colour='cornflowerblue') +  
  labs(title= "Relationship between EU and NA sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="EU_Sales",
       y="NA_Sales") +
  geom_smooth(method = "lm", colour='deeppink2') +
  stat_poly_eq() +
  scale_fill_brewer('set2') +
  theme_classic()


#Global_Sales v NA_Sales

ggplot(data=sales_1_grouped, mapping=aes(x=NA_Sales, y=Global_Sales)) +
  geom_point(alpha=0.8, size=4, colour='cornflowerblue') +  
  labs(title= "Relationship between NA and Global sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="NA_Sales",
       y="Global_Sales") +
  geom_smooth(method = "lm", colour='deeppink2') +
  stat_poly_eq() +
  scale_fill_brewer('set2') +
  theme_classic()


#Global_Sales v EU_Sales

ggplot(data=sales_1_grouped, mapping=aes(x=EU_Sales, y=Global_Sales)) +
  geom_point(alpha=0.8, size=4, colour='cornflowerblue') +  
  labs(title= "Relationship between EU and Global sales",
       subtitle="by Platform",
       caption="Source: Turtle_Sales.csv",
       x="EU_Sales",
       y="Global_Sales") +
  geom_smooth(method = "lm", colour='deeppink2') +
  stat_poly_eq() +
  scale_fill_brewer('set2') +
  theme_classic()

###############################################################################
# 5. Observations and insights
## Scatterplots, Histograms and BoxPlots ## 
## The aggregation of the data has not made a material difference to the 
## scatterplots other than reducing the number of points.
## Histograms have all become slightly less right skewed which will be in part due 
## to the consolidation of and removal of smaller volumes, so this change makes 
## sense.
## Boxplots have been slightly impacted too, predominantly the reduction of 
## outliers in the data, an increase in the IQRs and a general shift to the right 
## of the plot. Again I would expect this from consolidating values.

## Normality, Kurtosis and Skewness ## 
## Non of the sales columns are normally distributed. All have similar looking 
## shapes in the QQ plots, with a slight upward curve which suggests skewed data 
## to the right. Transformation could be necessary (sqrt, log) to improve a linear 
## regression model here.
## Shapiro tests confirm non-normality of the data - all have low p values so the 
## H0 of normal distribution can be rejected.
## Skewness calculation applied to each column again show heavy right/positive 
## skewed data with positive values being returned. 
## Kurtosis shows all columns are leptokurtic/positive kurtosis with high peaks 
## and taller tails, with more of the values away from the mean

## Correlation ## 
## EU and NA Sales are moderately correlated (0.62), with better correlation 
## between EU and Global (0.85) and NA and Global (0.92). This is not surprising 
## given that Global Sales is a total of the regions, which include EU and NA which
## make up 31% and 47% respectively. More a case or causation not correlation 
## when looking at Global Sales?
  
## Further Investigations ## 
## -	Outlier analysis - any improvement from removing outliers?
## -	Transformation of data - any better correlation is sqrt or log 
## transformation done?
  
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

# 1. Load and explore the data
# View data frame created in Week 5.
sales_1_grouped

# Determine a summary of the data frame.
summary(sales_1_grouped)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

# check correlations of the df
cor(select(sales_1_grouped, -Product))

# Look at 3 correlations, create 3 models

model1 <- lm(NA_Sales~EU_Sales, data=sales_1_grouped)
model2 <- lm(Global_Sales~EU_Sales, data=sales_1_grouped)
model3 <- lm(Global_Sales~NA_Sales, data=sales_1_grouped)

# View Summaries
summary(model1)
summary(model2)
summary(model3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
# Plot Residuals
plot(model1$residuals)
abline(h=0, col='red')
plot(model2$residuals)
abline(h=0, col='red')
plot(model3$residuals)
abline(h=0, col='red')

# Plot the scatterplots with best fit lines
plot(sales_1_grouped$EU_Sales, sales_1_grouped$NA_Sales)
# Add line-of-best-fit.
abline(coefficients(model1))

plot(sales_1_grouped$EU_Sales, sales_1_grouped$Global_Sales)
# Add line-of-best-fit.
abline(coefficients(model2))

plot(sales_1_grouped$NA_Sales, sales_1_grouped$Global_Sales)
# Add line-of-best-fit.
abline(coefficients(model3))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
model4 = lm(Global_Sales~NA_Sales + EU_Sales, data=sales_1_grouped)

# Multiple linear regression model.
summary(model4)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Create object to hold predictions and pass to a df
global_sales_pred <- predict(model4, sales_1_grouped)

global_sales_pred_df <- as.data.frame(global_sales_pred)

# append to the grouped dataframe for analysis
sales_1_grouped$Global_Sales_Predictions <- round(global_sales_pred_df$global_sales_pred,2)

# check df
head(sales_1_grouped)

# check accuracy of predictions
accuracy(sales_1_grouped$Global_Sales_Predictions, sales_1_grouped$Global_Sales)

# plot predictions v actuals and show a line where 100% accuracy is
plot(sales_1_grouped$Global_Sales_Predictions, sales_1_grouped$Global_Sales)
abline(a=0, b=1, col='red')

###############################################################################

# 5. Observations and insights

## Of the 3 models created, the best predictor of Global Sales in NA sales. 
## There is a reasonable r^2 value when predicting Global Sales from EU Sales and 
## weak r^2 between EU sales and NA sales. This can be seen on the plotting of the
## scatterplot with best fit and the plotting of residuals too which shows NA v 
## Global with the residuals closest to the line. Therefore I would continue with 
## using the NA Sales if we were just using a simple linear regression.

## In the multiple linear regression we should use both EU and NA sales, this 
## has a high r^2 of 97%.  Again I would expect this given that EU+NA = 75% of the 
## Global sales so it is always going to be a strong driver.

## When doing the predictions, the values given to us in the module were for 
## the non-aggregated  data which the model wasn't built on (the model 
## consolidated platforms). Therefore I decided to just run a prediction on the 
## full aggregated dataframe, add in as a column then compare to observed, plot 
## and run an accuracy report. The MAPE of the model is 12.8% which is a good 
## accuracy score, saying the model, on aberage, gets within 12.8% of the 
## observed value. The plot also shows for each product how close observed is 
## to the predicted



###############################################################################
###############################################################################



