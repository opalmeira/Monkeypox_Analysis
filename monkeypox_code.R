#!/usr/bin/Rscript

#########################   Monkeypox Analaysis   #############################
### This script will attempt to answer the questions bellow through some    ###
### statistical analysis on a free available dataset on curated Monkeypox   ###
### cases from the 2022 outbreak.                                           ###
### 1. Which country reported more confirmed cases of monkepox infection?   ###
### 2. In what age range there were more confirmed cases of monkepox        ###
### infection?                                                              ###
### 3. Is there a relationship between country and age in the cases?        ###
###############################################################################


message("#### Monkeypox outbreak 2022 Analysis #### 
1. Which country reported more confirmed cases of monkepox infection?
2. In what age range there were more confirmed cases of monkepox infection?
3. Is there a relationship between country and age in the cases?")

# Loading libraries
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)
library(gridExtra,warn.conflicts = FALSE)

# Dataset downloaded from:
# https://www.kaggle.com/datasets/hanzlanawaz/monkeypox-outbreak?resource=download

# Checking how many files there are and their names in the zipped folder
message("\nChecking the zip file")
unzip("Monkeypox.zip", list = TRUE)

# Load the target file
mpx_df <- read.table(unz("Monkeypox.zip", "Monkeypox.csv"), 
                     fill = TRUE, sep = ",",quote = "\"", header = TRUE, stringsAsFactors = FALSE)

# Check columns
message("\nNames of the columns")
colnames(mpx_df)

# Since we are only interested in the variables Country, Age and Gender of confirmed cases
# we will keep only these relevant columns
col_keep <- c("ID","Status","Country","Age")
mpx_df1 <- mpx_df[,col_keep]

# Filtering data for only confirmed cases, known age and country
mpx_df1 <- mpx_df1 %>% filter(Status == "confirmed", Age != "", Country != "")

# Drop column Status
mpx_df1 <- mpx_df1[,-2]


### Answering question 1
message("\n ###### Answering question 1 ######")

# Making a table of frequency per country so that we can visualize in a bar plot 
country_occ <- mpx_df1 %>% group_by(Country) %>% count(sort = TRUE)

#Visualization on a bar plot
g1 <- ggplot(country_occ, aes(x = reorder(Country,-n), y = n)) 
g1 <- g1 + geom_bar(stat = "identity", color='#ff66ff',fill='#ff66ff')
g1 <- g1 + labs(x = "Countries", y = "Confirmed Cases")
g1 <- g1 + theme(axis.text.x=element_text(angle=45, hjust=1,size = 23),
		 axis.text.y = element_text(size = 25),
		 axis.title = element_text(size = 25),
		 legend.text = element_text(size = 28),
		 legend.title = element_text(size = 28),
		 plot.title = element_text(size=30))
g1 <- g1 + ggtitle("Confirmed Monkeypox cases per country")

# Graph shows that Portugal had the highest number of cases, followed by Canada and Germany

message("\nPortugal has the highest number of cases followed by Canada and Germany as shown on the graph")

### Answer question 2
message("\n ###### Answering question 2 ######")

# Making a table of frequency per age-range so that we can visualize in a bar plot 
age_occ <- mpx_df1 %>% group_by(Age) %>% count(sort = TRUE)

# Visualization in a bar plot
g2 <- ggplot(age_occ, aes(x = reorder(Age,-n), y = n)) 
g2 <- g2 + geom_bar(stat = "identity", color='#00cc66',fill='#00cc66')
g2 <- g2 + labs(x = "Age-range", y = "Confirmed Cases")
g2 <- g2 + theme(axis.text.x=element_text(angle=45, hjust=1,size = 23),
		 axis.text.y = element_text(size = 25),
                 axis.title = element_text(size = 25),
                 legend.text = element_text(size = 28),
                 legend.title = element_text(size = 28),
		 plot.title = element_text(size=30))
g2 <- g2 + ggtitle("Confirmed Monkeypox cases per Age range")

g <- gridExtra::grid.arrange(g1, g2, nrow=1)

message("\nRange 20-64 has the highest number of cases as shown on the graph, however, this range is very large so we will need some data manipulation")

message("\nPress a key to see the graphs")
invisible(readLines("stdin", n=1))

x11(width = 25, height = 10)
plot(g)

message("\nPress a key to continue")
invisible(readLines("stdin", n=1))

#Graph shows that the age range 20-64 had the highest number of cases, however, this range is very large,
#so we cannot draw conclusions from that

# Check Ages
message("\nAge groups have different ranges. Some data manipulation to standadize range will be performed")
message("The age range will be separated by dash so that we obtain the lower-bound and the upper-bound in separate columns")
sort(unique(mpx_df$Age))

# Upon checking values in Age column, we noticed that the ranges are not uniform
# so we will have to create a standard range age for further analysis.
# First we add columns for lower and upper-bound age and a third column with the length
# of the range
mpx_df1[["age_l"]]<-''
mpx_df1[["age_h"]]<-''
mpx_df1[["age_dif"]]<-''

# Funcion to separate age limits in the Age column
expand.age <- function(age_range) {
  limits <- as.numeric(unlist(strsplit(age_range, '-')))
  c(limits[1], limits[2])
}

# We use the function created above to separate the values of Age columns
# in each row and then assign lower-bound age and upper-bound ages to specific columns
# Then we calculate the difference between the values including the limits
for(i in 1:nrow(mpx_df1)) {
    age <- suppressWarnings(expand.age(mpx_df1$Age[i]))
    mpx_df1$age_l[i] <- age[1]
    mpx_df1$age_h[i] <- age[2]
    mpx_df1$age_dif[i] <- (age[2] - age[1]) + 1
}

#Let's take a look at the age range occurrences and se if we can analyse them separately
age_dif_occ <- mpx_df1 %>% group_by(age_dif) %>% count(sort = TRUE)

# age range of length 5 seems to be more appropriate for analysis since the range is short, yet
# the sample size is larger than range length 10, so we subsample the data where age_dif
# is 5

mpx_df2 <- with(mpx_df1, mpx_df1[which(age_dif == 5),])

#Eliminate rows with NAs
mpx_df2 <- mpx_df2[complete.cases(mpx_df2), ]

range_5 <- mpx_df2 %>% group_by(Age) %>% count(sort = TRUE)

# Visualization
g3 <- ggplot(range_5, aes(x = reorder(Age,-n), y = n)) 
g3 <- g3 + geom_bar(stat = "identity", color='#00cc66',fill='#00cc66')
g3 <- g3 + labs(x = "Age-range", y = "Confirmed Cases")
g3 <- g3 + theme(axis.text.x=element_text(angle=45, hjust=1,size = 23),
                 axis.text.y = element_text(size = 25),
                 axis.title = element_text(size = 25),
                 legend.text = element_text(size = 28),
                 legend.title = element_text(size = 28),
		 plot.title = element_text(size=30))
g3 <- g3 + ggtitle("Confirmed Monkeypox cases per age-range(5)")


# Considering an age range of 5, we observe that the highest number of cases
# occurred in younger people with age ranging from 25-39 in comparison to lower 
# number of cases in older people whose age ranged from 40-59

message("\nThe highest number of cases occurred in younger people with age ranging from 25-39 in comparison to lower 
	number of cases in older people whose age ranged from 40-59.")

message("\nPress a key to see the graph")
invisible(readLines("stdin", n=1))

x11(width = 15, height = 10)
plot(g3)

message("Press a key to continue")
invisible(readLines("stdin", n=1))


## Answer question 3
message("\n##### Answering question 3 #####")
# Prepare the data for statistical computation
mpx_df2$Country <- as.factor(mpx_df2$Country)
mpx_df2$age_l <- as.numeric(mpx_df2$age_l)
mpx_df2$age_h <- as.numeric(mpx_df2$age_h)

# Descriptive statistics of lower-bound age range per country
message("\nFirst, we take an overview of the data distribution in relation to lower and upper-bound age range of length 5")
message("Descriptive statistics of confirmed cases in lower-bound age range")
group_by(mpx_df2, Country) %>%
  summarise(
    count = n(),
    mean = mean(age_l, na.rm = TRUE),
    sd = sd(age_l, na.rm = TRUE),
    median = median(age_l, na.rm = TRUE),
    IQR = IQR(age_l, na.rm = TRUE)
  )
  
# Descriptive statistics of upper-bound age range per country
message("\nDescriptive statistics of confirmed cases in upper-bound age range")
group_by(mpx_df2, Country) %>%
  summarise(
    count = n(),
    mean = mean(age_h, na.rm = TRUE),
    sd = sd(age_h, na.rm = TRUE),
    median = median(age_h, na.rm = TRUE),
    IQR = IQR(age_h, na.rm = TRUE)
  )
 
# Check for normality of the data distribution
# since p-value < than 0.05 the data is not normally distributed
#lower-bound
message("\nNormality test")
message("Lower-bound age range")
shapiro.test(mpx_df2$age_l)

#upper-bound
message("Upper-bound age range")
shapiro.test(mpx_df2$age_h)
message("The data is not normal distributed. Since p-value is lower than 0.05 
we can reject the null hypothesis of the test")

# Since we want to see the relationship between categorical variables and 
# numeric variables, we choose to use of Non-parametric correlations. More specifically 
# Kruskal-Wallis test because we have more than two indepenent groups(countries) and the data is not normally 
# distributed
#Lower-bound
message("\nNow we test the hypothesis that age and country are related")
message("\nTest if age is related to countries")

message("\nLower-bound range")
kruskal.test(age_l ~ Country, data = mpx_df2)

message("\nUpper-bound range")
#Upper-bound
kruskal.test(age_h ~ Country, data = mpx_df2)
 
#Answer3: Neither limits of age range have correlation with the countries. However, more data points is necessary to reach a conclusion

message("\nP-values of the test for both lower and uppper-bound are higher than 0.05, 
so we cannot reject the null hypothesis that age and country are not related")

message("Neither limits of age range have correlation with the countries. 
However, more data points is necessary to reach a conclusion")
message("\nEnd of Analysis")
