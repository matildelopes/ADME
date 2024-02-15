#install.packages("ISLR")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("DescTools")
#install.packages("leaps")


# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization
library(ISLR)
library(DescTools)
library(leaps)

data <- ISLR::Auto
data$name <- NULL
data <- data[1:49, ]
data


# Analise estatistica dos dados

resultados <- sapply(data, function(col) {
  estatisticas <- c(
    média = mean(col, na.rm = TRUE),
    mediana = median(col, na.rm = TRUE),
    var = var(col, na.rm = TRUE),
    mad = mad(col, na.rm = TRUE),
    sd = sd(col, na.rm = TRUE),
    min = min(col, na.rm = TRUE),
    max = max(col, na.rm = TRUE),
    trimmed_mean = mean(col, trim = 0.1), 
    winsorized_mean = mean(Winsorize(col, probs = c(0.1, 0.9)))
  )
  return(estatisticas)
})

resultados

#resultados_finais <- subset(resultados, select= -c(year, pct))
#resultados_finais


# Plotting
plot(data)

# Summary of the dataset
summary(data)

#Analise estatistica, graficos, valores e correlações MANDAR SOFIA
library(psych)
pairs.panels(data,smooth =FALSE,ellipses=FALSE,lm=TRUE)


#boxplots
library(car)
par(mar=c(1,1,1,1))
par(mfrow=c(3,2))
Boxplot(~mpg, data = data, id.n = Inf, main = "mpg")
Boxplot(~cylinders, data = data, id.n = Inf, main = "cylinders")
Boxplot(~displacement, data = data, id.n = Inf, main = "displacement")
Boxplot(~horsepower, data = data, id.n = Inf, main = "horsepower")
Boxplot(~weight, data = data, id.n = Inf, main = "weight")
Boxplot(~acceleration, data = data, id.n = Inf, main = "acceleration")


#----------------------------------//--------------------------

#Best subset - MELHOR
best_subset <- regsubsets(mpg ~ ., data = data, method = "forward")
summary(best_subset)
summary(best_subset)$adjr2

best_subset_2 <- regsubsets(mpg ~ ., data = data, method = "backward")
#summary(best_subset_2)
summary(best_subset_2)$adjr2

best_subset_3 <- regsubsets(mpg ~ ., data = data, method = "exhaustive")
#summary(best_subset_3)
summary(best_subset_3)$adjr2

best_subset_4 <- regsubsets(mpg ~ ., data = data, method = "seqrep")
#summary(best_subset_4)
summary(best_subset_4)$adjr2


# Fit a multiple linear regression model
model <- lm(mpg ~ ., data = data)

# Summary of the regression model - Original
summary(model)

#PERGUNTAR A STORA
anova(model)


# Fit a refined multiple linear regression model excluding 'displacement' and 'year'
refined_model_1 <- lm(mpg ~ cylinders + horsepower + weight + acceleration + origin, data = data)

# Summary of the refined regression model
summary(refined_model_1)
anova(refined_model_1)


# Fit a refined 2 multiple linear regression model excluding 'origin'
refined_model_2 <- lm(mpg ~ cylinders + horsepower + weight + acceleration , data = data)

# Summary of the refined regression model 2
summary(refined_model_2)
anova(refined_model_2)

# Fit a refined 3 multiple linear regression model excluding 'horsepower' - MELHOR
refined_model_3 <- lm(mpg ~ cylinders + weight + acceleration , data = data)

# Summary of the refined regression model 2
summary(refined_model_3)
anova(refined_model_3)


#-------------------------------//----------------------------------------------

par(mfrow = c(2, 2))
# Residual plot
plot(refined_model_3, which = 1,pch = 16)  # Residuals vs Fitted

# Normal Q-Q plot
plot(refined_model_3, which = 2,pch = 16)  # Normal Q-Q plot

# Scale-location plot
plot(refined_model_3, which = 3,pch = 16)  # Scale-location plot (Square root of standardized residuals vs Fitted values)

# Residuals vs Leverage plot
plot(refined_model_3, which = 5,pch = 16)  # Residuals vs Leverage


#DIAGNOSTIC
library(car)
## Males regression
# Hat's values - leverage points
p=4
n=49

hM=hatvalues(refined_model_3)
hMlev=hM[hM>2*p/n]
# Cook's distances - influential observations
cM=cooks.distance(refined_model_3)
cMinfl=cM[cM>4/(n-p)]
cMinfl_R=cM[cM>4*mean(cM)] # R rule
# Influential plots
influenceIndexPlot(refined_model_3, pch = 16)

#COOKS
library(car)
influencePlot(refined_model_3)


# Identify potential outliers using studentized residuals
outliers <- which(abs(rstandard(refined_model_3)) > 2)

# Display potential outlier observations
data[outliers, ]



#---------------------------//---------------------------

# Observation indices
obs_14 <- 14
obs_31 <- 31

# Extracting the data for these observations
data_obs_14 <- data[obs_14, -1]  # Excluding 'mpg' column for observation 14
data_obs_31 <- data[obs_31, -1]  # Excluding 'mpg' column for observation 31

# Calculating confidence intervals for expected value (CI)
ci_expected_14 <- predict(refined_model_3, newdata = data_obs_14, interval = "confidence", level = 0.975)
ci_expected_31 <- predict(refined_model_3, newdata = data_obs_31, interval = "confidence", level = 0.975)

# Calculating prediction intervals (PI)
pi_14 <- predict(refined_model_3, newdata = data_obs_14, interval = "prediction", level = 0.975)
pi_31 <- predict(refined_model_3, newdata = data_obs_31, interval = "prediction", level = 0.975)

ci_expected_14
ci_expected_31

pi_14
pi_31

predict(refined_model_3)

data

#-----------------------//-------------------------------------


# Test for significance of the regression
anova(model)

# Check for variable importance using stepwise regression
step_model <- step(model)

# Summary of the stepwise regression model
summary(step_model)
