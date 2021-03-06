library(tidyverse)
library(Hmisc)
library(ggpubr)
library(Metrics)


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
for (i in 1:ncol(key_variables)){
  print(ggplot(stack(key_variables[i]), aes(x = ind, y = values)) + 
          geom_boxplot())
}


# validity of the assumption that each of the 8 variables is normally distributed

# if the p-value > 0.05 implying that the distribution of the data are not 
# significantly different from normal distribution
test <- matrix(0, nrow = ncol(key_variables), ncol = 2)
for (i in 1:ncol(key_variables)) {
  test[i, 1] <- names(key_variables)[i]
  test[i, 2] <- shapiro.test(key_variables[,i])[[2]]
}
tibble('variables' = test[,1], 'p_value' = test[,2])


# Correlation analysis for the following pairs of fields for the complete data 
# spanning 1960 to 2010

# Games Won vs Runs Scored, Games Lost vs Runs Scored
# Games Won vs Runs Against, Games Lost vs Runs Against
# Games Won vs (Runs Scored minus Runs Against), Games Lost vs (Runs Scored 
# minus Runs Against)

df2 <- df_1960_2010 %>% 
  select(Games_Won, Games_Lost, Runs_Scored, Runs_Against) %>% 
  mutate('Runs_Scored-Runs_Against' = Runs_Scored - Runs_Against)

for (i in 3:ncol(df2)){
  for (j in 1:2) {
    print(ggplot(mapping = aes(x = unlist(df2[i]), y = unlist(df2[j]))) + 
            geom_point() +
            xlab(names(df2)[i]) +
            ylab(names(df2)[j]))
    print(cor(df2[i], df2[j]))
  }
}

# Dynamic graph of correlation for Games Won vs (Runs Scored minus Runs Against) from 1960 to 2010 according to different teams
p <- ggplot(df_1960_2010, aes(x = Runs_Scored-Runs_Against, y = Games_Won, color = Team)) +
  geom_point(alpha = 3, show.legend = FALSE) +
  labs(title = 'Year: {frame_time}', x = 'Runs_Scored-Runs_Against', y = 'Games_Won') +
  transition_time(Year) +
  ease_aes('linear')

animate(p, renderer = gifski_renderer())    
   
      
# Create a multiple linear regression model for each of the 4 time periods noted 
# above and select the independent variables that are good predictors of games won

drop.na <- function(data) {
  dt <- data
  empty_frame <- data.frame(matrix(0, nrow(dt), ncol(dt)))
  colnames(empty_frame) <- names(dt)
  for (i in 1:ncol(dt)){
    if (colSums(is.na(dt[i])) < nrow(dt)) {
      empty_frame[i] <- dt[i]
    }
  }
  new_dt <- na.omit(empty_frame) %>%
    select_if(colSums(.) != 0)
  rownames(new_dt) <- NULL
  new_dt
}


# Period 1 - before 1920
df_p1 <- drop.na(df_before_1920[-c(1, 3)])


## Forward stepwise regression: starts with the intercept and adds new ones until the model cannot be improved
null_modelp1 <- lm(Games_Won ~ 1, data = df_p1)
full_modelp1 <- lm(Games_Won ~ . , data = df_p1)

step(null_modelp1, direction = "forward", scope = formula(full_modelp1))

fit1 <- lm(Games_Won ~ Runs_Against + Runs_Scored + Errors + 
             Strike_Outs + Doubles + Strikeouts_Allowed + Walks_Allowed + 
             Home_Run_Allowed, data = df_p1)

summary(fit1)

# Period 2 -  1920 to 1960
df_p2 <- drop.na(df_1920_1960[-c(1, 3)])

## Forward stepwise regression
null_modelp2 <- lm(Games_Won ~ 1, data = df_p2)
full_modelp2 <- lm(Games_Won ~ . , data = df_p2)

step(null_modelp2, direction = "forward", scope = formula(full_modelp2))

fit2 <- lm(Games_Won ~ Runs_Scored + Runs_Against + Saves + 
             Complete_Games + Infield_Put_Outs + At_Bats + Hits + Stolen_Bases + 
             Double_Plays + Strikeouts_Allowed + Walks_Allowed, data = df_p2)

summary(fit2)

# Period 3 - 1960 to 1990
df_p3 <- drop.na(df_1960_1990[-c(1, 3)])

## Forward stepwise regression
null_modelp3 <- lm(Games_Won ~ 1, data = df_p3)
full_modelp3 <- lm(Games_Won ~ . , data = df_p3)

step(null_modelp3, direction = "forward", scope = formula(full_modelp3))

fit3 <- lm(Games_Won ~ Runs_Scored + Earned_Run_Average + Saves + 
             Complete_Games + Fielding_Percentage + Shutout + Errors + 
             Infield_Put_Outs + At_Bats + Hits + Runs_Against + Caught_Stealing + 
             Double_Plays + Strike_Outs + Walks + Stolen_Bases, data = df_p3)

summary(fit3)

# Period 4 - 1990 to 2010
df_p4 <- drop.na(df_1990_2010[-c(1, 3)])

## Forward stepwise regression
null_modelp4 <- lm(Games_Won ~ 1, data = df_p4)
full_modelp4 <- lm(Games_Won ~ . , data = df_p4)

step(null_modelp4, direction = "forward", scope = formula(full_modelp4))

fit4 <- lm(Games_Won ~ Saves + Runs_Scored + Runs_Against + 
             Triples + Shutout + Complete_Games + Strikeouts_Allowed + 
             Walks_Allowed + At_Bats + Infield_Put_Outs + Hits + Home_Run_Allowed + 
             Errors + Fielding_Percentage + Home_Runs, data = df_p4)

summary(fit4)

# From the summary, we can find
# The p-value indicates if the regression coefficient is significantly different 
# from 0. If the p-value is smaller than 0.05, then we reject the null hypothesis 
# that the regression coefficient is equal to 0 at 0.05 significance level.
# R^2 is a measure of the variance of y that is explained by the model. 
# The higher the R^2 is, the “better” is the model. However, adding additional 
# variables will always increase R^2 while this may not improve the model in the 
# sense that it may not improve prediction for new data. The adjusted  
# R^2 accounts for the number of variables in the model and is a better measure 
# of the model’s quality.
# F-statistic tells us whether the model is statistically significant or not. 
# The null hypothesis is that all coefficients are zero and the alternative 
# hypothesis is that not all coefficients are zero.
      
      

# Use the 4th regression model from 1990 to 2010 and forecast the number of games 
# won for the New York Yankees and the Toronto Blue Jays using values for the 
# independent variables for 2012 and 2015.

# New York Yankees
df_yk <- data %>%  
  filter(Team_Name == 'New York Yankees', Year %in% c(2012, 2015))

df_yk2 <- df_yk %>% 
  select(Saves, Runs_Scored, Runs_Against, Triples, Shutout, Complete_Games, Strikeouts_Allowed, 
           Walks_Allowed, At_Bats, Infield_Put_Outs, Hits, Home_Run_Allowed, 
           Errors, Fielding_Percentage, Home_Runs)

pred_yk <- predict(fit4, df_yk2)
      
cor(pred_yk, df_yk$Games_Won)

# Low values of RMSE indicate good fit
rmse(as.double(df_yk$Games_Won), as.double(pred_yk))

      
# Toronto Blue Jays 
df_bj <- data %>%  
  filter(Team_Name == 'Toronto Blue Jays', Year %in% c(2012, 2015))

df_bj2 <- df_bj %>% 
  select(Saves, Runs_Scored, Runs_Against, Triples, Shutout, Complete_Games, Strikeouts_Allowed, 
           Walks_Allowed, At_Bats, Infield_Put_Outs, Hits, Home_Run_Allowed, 
           Errors, Fielding_Percentage, Home_Runs)

pred_bj <- predict(fit4, df_bj2)
      
cor(pred_bj, df_bj$Games_Won)

# Low values of RMSE indicate good fit
rmse(as.double(df_bj$Games_Won), as.double(pred_bj))
