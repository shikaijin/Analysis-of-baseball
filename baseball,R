library(tidyverse)
library(Hmisc)
library(ggpubr)
library(bestglm)


# Objectives
# This project aims to determine factors associated with game performance and 
# in particular whether tight defense is associated with increased game won, and 
# whether the conclusion that tight defenses increase game won is valid across 
# multiple time periods.

data <- read.csv('C:/Users/ROG/Desktop/baseball_teams.csv')
head(data)
df <- data[, c(1, 9, 10, 15:40)]

# Check missing point
apply(df, 2, function(x) sum(is.na(x)))


# Period 1 – before 1920
df_before_1920 <- filter(df, Year < 1920)

# Period 2 -  1920 to 1960
df_1920_1960 <- filter(df, between(Year, 1920, 1960))


# Period 3 – 1960 to 1990
df_1960_1990 <- filter(df, between(Year, 1960, 1990))

# Period 4 – 1990 to 2010
df_1990_2010 <- filter(df, between(Year, 1990, 2010))


# Perform an exploratory data analysis to understand the statistical 
# properties and distribution shapes of 8 key variables. 
# Perform this analysis for a single dataset that spans 1960 to 2010

# Calculate the descriptive statistics for the 8 variables 
df_1960_2010 <- filter(df, between(Year, 1960, 2010))
key_variables <- df_1960_2010[, c('Runs_Scored','At_Bats','Hits', 'Walks', 
                                  'Runs_Against', 'Earned_Run_Average', 'Fielding_Percentage',
                                  'Saves')]
summary(key_variables)


# histogram for each of the 8 variables
hist.data.frame(key_variables)


# boxplot for each of the 8 variables
ggplot(stack(key_variables), aes(x = ind, y = values)) + 
  geom_boxplot()


# validity of the assumption that each of the 8 variables is normally distributed
test <- matrix(0, nrow = ncol(key_variables), ncol = 2)
for (i in 1:ncol(key_variables)) {
  test[i, 1] <- names(key_variables)[i]
  test[i, 2] <- shapiro.test(key_variables[,i])[[2]]
}
tibble('variables' = test[,1], 'p_value' = test[,2])

# if the p-value > 0.05 implying that the distribution of the data are not 
# significantly different from normal distribution



# Correlation analysis for the following pairs of fields for the complete data 
# spanning 1960 to 2010

# Games Won vs Runs Scored
cor(df_1960_2010$Games_Won, df_1960_2010$Runs_Scored)
ggplot(df_1960_2010, aes(x = Runs_Scored, y = Games_Won)) + 
  geom_point()

#	Games Won vs Runs Against
cor(df_1960_2010$Games_Won, df_1960_2010$Runs_Against)
ggplot(df_1960_2010, aes(x = Runs_Scored, y = Runs_Against)) + 
  geom_point()

#	Games Won vs (Runs Scored minus Runs Against)
cor(df_1960_2010$Games_Won, df_1960_2010$Runs_Scored - df_1960_2010$Runs_Against)
ggplot(df_1960_2010, aes(x = Runs_Scored, y = Runs_Scored - Runs_Against)) + 
  geom_point()


#	Games Lost vs Runs Scored
cor(df_1960_2010$Games_Lost, df_1960_2010$Runs_Scored)
ggplot(df_1960_2010, aes(x = Runs_Lost, y = Games_Won)) + 
  geom_point()

#	Games Lost vs Runs Against
cor(df_1960_2010$Games_Lost, df_1960_2010$Runs_Against)
ggplot(df_1960_2010, aes(x = Runs_Lost, y = Runs_Against)) + 
  geom_point()

#	Games Lost vs (Runs Scored minus Runs Against)
cor(df_1960_2010$Games_Lost, df_1960_2010$Runs_Scored - df_1960_2010$Runs_Against)
ggplot(df_1960_2010, aes(x = Runs_Lost, y = Runs_Scored - Runs_Against)) + 
  geom_point()
