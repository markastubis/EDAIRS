# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(urca)
library(forecast)
library(tseries)
# importing IRS2020 data
setwd("C:/Users/Admin/Documents/R projects")
irs2020 <- read.table("IRS2020.csv", header = TRUE, sep = ";")

# converting amount variables into dollar amounts
irs2020$TotalIncome <- 1000 * irs2020$TotalIncome
irs2020$Salaries <- 1000 * irs2020$Salaries
irs2020$Divis <- 1000 * irs2020$Divis
irs2020$Retax <- 1000 * irs2020$Retax

print(head(irs2020))

# descriptive analysis
desc_stats <- summarise(irs2020, 
                        across(c("TotalIncome", "Salaries", "Divis", "Retax"), 
                               list(min = min, max = max, mean = mean, median = median, sd = sd))
)

print(desc_stats)

# showing how to calculate the new variables 
irs2020 <- irs2020 %>%
  mutate(AvgIncome = TotalIncome / NumofReturns, 
         AvgSalaryIncome = Salaries / NumofReturns,
         ## if 0 then exclude it from calculation but do not delete it
         AvgDividends = ifelse(Divis > 0, Divis / NumofRetwithDivis, NA), 
         AvgRealEstateTaxes = ifelse(Retax > 0, Retax / NumofRetwithRE, NA)) 

# checking the dataframe
View(irs2020)

# reshaping the data to a long format
irs2020_long <- irs2020 %>%
  pivot_longer(cols = starts_with("Avg"), 
               names_to = "Variable", 
               values_to = "Value")

# creating new columns for each variable
irs2020_long <- irs2020_long %>%
  pivot_wider(names_from = Variable,
              values_from = Value)

# summary

summary(irs2020_long[, c("AvgDividends", "AvgIncome", "AvgRealEstateTaxes", "AvgSalaryIncome")])

# plotting the boxplot
boxplot(irs2020_long[, c("AvgIncome", "AvgSalaryIncome", "AvgDividends", "AvgRealEstateTaxes")],
        main = "Boxplot of Household Averages Across Variables",
        xlab = "Variable",
        ylab = "Value",
        col = c('navy','red','darkgreen',"yellow"))
## adjusting the margins because the x axis doesnt like to load properly
par(las = 0, cex.axis = 0.7, mar = c(2, 2, 3, 2) + 0.1) 

legend("topright", legend = c("Dividends", "Income", "Real Estate Taxes", "Salary"),
       fill = c("navy", "red", "darkgreen", "yellow"))

## seeing the min,max of each new variable
highest_income <- irs2020_long[which.max(irs2020_long$AvgIncome), c('ZIPCODE', 'AvgIncome', 'STATE')]
lowest_income <- irs2020_long[which.min(irs2020_long$AvgIncome), c('ZIPCODE', 'AvgIncome', 'STATE')]

print(paste("Highest income ZIP: ", highest_income$ZIPCODE, " with average income: ", highest_income$AvgIncome, " in state: ", highest_income$STATE))
print(paste("Lowest income ZIP: ", lowest_income$ZIPCODE, " with average income: ", lowest_income$AvgIncome, " in state: ", lowest_income$STATE))

highest_salary <- irs2020_long[which.max(irs2020_long$AvgSalaryIncome), c('ZIPCODE', 'AvgSalaryIncome', 'STATE')]
lowest_salary <- irs2020_long[which.min(irs2020_long$AvgSalaryIncome), c('ZIPCODE', 'AvgSalaryIncome', 'STATE')]

print(paste("Highest salary ZIP: ", highest_salary$ZIPCODE, " with average salary: ", highest_salary$AvgSalaryIncome, " in state: ", highest_salary$STATE))
print(paste("Lowest salary ZIP: ", lowest_salary$ZIPCODE, " with average salary: ", lowest_salary$AvgSalaryIncome, " in state: ", lowest_salary$STATE))

highest_dividends <- irs2020_long[which.max(irs2020_long$AvgDividends), c('ZIPCODE', 'AvgDividends', 'STATE')]
lowest_dividends <- irs2020_long[which.min(irs2020_long$AvgDividends), c('ZIPCODE', 'AvgDividends', 'STATE')]

print(paste("Highest dividends ZIP: ", highest_dividends$ZIPCODE, " with average dividends: ", highest_dividends$AvgDividends, " in state: ", highest_dividends$STATE))
print(paste("Lowest dividends ZIP: ", lowest_dividends$ZIPCODE, " with average dividends: ", lowest_dividends$AvgDividends, " in state: ", lowest_dividends$STATE))

highest_re_taxes <- irs2020_long[which.max(irs2020_long$AvgRealEstateTaxes), c('ZIPCODE', 'AvgRealEstateTaxes', 'STATE')]
lowest_re_taxes <- irs2020_long[which.min(irs2020_long$AvgRealEstateTaxes), c('ZIPCODE', 'AvgRealEstateTaxes', 'STATE')]

print(paste("Highest real estate taxes ZIP: ", highest_re_taxes$ZIPCODE, " with average real estate taxes: ", highest_re_taxes$AvgRealEstateTaxes, " in state: ", highest_re_taxes$STATE))
print(paste("Lowest real estate taxes ZIP: ", lowest_re_taxes$ZIPCODE, " with average real estate taxes: ", lowest_re_taxes$AvgRealEstateTaxes, " in state: ", lowest_re_taxes$STATE))


## calculating the fraction of dividends
irs2020_long$fraction_dividends <- irs2020_long$NumofRetwithDivis / irs2020_long$NumofReturns
fraction_distribution <- table(irs2020_long$fraction_dividends)

## creating a scatterplot
plot(irs2020_long$AvgIncome, irs2020_long$fraction_dividends, 
     xlab = "Income", ylab = "Fraction of receiving dividends", 
     main = "Scatterplot of Income vs Fraction of receiving dividends")

## same scatterplot but each US State has a different color

ggplot(irs2020_long, aes(x = AvgIncome, y = fraction_dividends, color = STATE)) +
  geom_point() +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Income", y = "Fraction of receiving dividends", color = "State") +
  ggtitle("Scatterplot of Income vs Fraction of Receiving Dividends by State") +
  theme_minimal()

## converting STATE to a factor
irs2020_long$STATE <- factor(irs2020_long$STATE)

## fiting the  linear regression model
model <- lm(AvgIncome ~ fraction_dividends, data = irs2020_long)

## ploting the  scatter plot with the regression line
plot(irs2020_long$fraction_dividends, irs2020_long$AvgIncome, 
     xlab = "Fraction of households receiving dividends", 
     ylab = "Household Income",
     main = "Regression of households Income on Fraction of Households receiving dividends")
abline(model, col = "red")

# assessing is it a good fit 
summary(model)

# ploting residuals
plot(model$residuals, 
     xlab = "Observation",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "blue")
###############################################################################
###############################################################################
## 1f repeat everything with 2019 data
## because the code is identical to the 2020 data there is no need for more comments
irs2019 <- read.table("IRS2019.csv", header = TRUE, sep = ";")


irs2019$TotalIncome <- 1000 * irs2019$TotalIncome
irs2019$Salaries <- 1000 * irs2019$Salaries
irs2019$Divis <- 1000 * irs2019$Divis
irs2019$Retax <- 1000 * irs2019$Retax

print(head(irs2019))


desc_stats <- summarise(irs2019, 
                        across(c("TotalIncome", "Salaries", "Divis", "Retax"), 
                               list(min = min, max = max, mean = mean, median = median, sd = sd))
)

print(desc_stats)


irs2019 <- irs2019 %>%
  mutate(AvgIncome = TotalIncome / NumofReturns,  
         AvgSalaryIncome = Salaries / NumofReturns,  
         AvgDividends = ifelse(NumofRetwithDivis > 0, Divis / NumofRetwithDivis, 0), 
         AvgRealEstateTaxes = ifelse(NumofRetwithRE > 0, Retax / NumofRetwithRE, 0)) 


View(irs2019)


irs2019_long <- irs2019 %>%
  pivot_longer(cols = starts_with("Avg"), 
               names_to = "Variable", 
               values_to = "Value")


irs2019_long <- irs2019_long %>%
  pivot_wider(names_from = Variable,
              values_from = Value)


View(irs2019_long)

summary(irs2019_long[, c("AvgDividends", "AvgIncome", "AvgRealEstateTaxes", "AvgSalaryIncome")])

boxplot(irs2019_long[, c("AvgIncome", "AvgSalaryIncome", "AvgDividends", "AvgRealEstateTaxes")],
        main = "Boxplot of Household Averages Across Variables",
        xlab = "Variable",
        ylab = "Value",
        col = c('navy','red','darkgreen',"yellow"))

par(las = 0, cex.axis = 0.7, mar = c(2, 2, 3, 2) + 0.1) 

legend("topright", legend = c("Dividends", "Income", "Real Estate Taxes", "Salary"),
       fill = c("navy", "red", "darkgreen", "yellow"))



highest_income <- irs2019_long[which.max(irs2019_long$AvgIncome), c('ZIPCODE', 'AvgIncome', 'STATE')]
lowest_income <- irs2019_long[which.min(irs2019_long$AvgIncome), c('ZIPCODE', 'AvgIncome', 'STATE')]

print(paste("Highest income ZIP: ", highest_income$ZIPCODE, " with average income: ", highest_income$AvgIncome, " in state: ", highest_income$STATE))
print(paste("Lowest income ZIP: ", lowest_income$ZIPCODE, " with average income: ", lowest_income$AvgIncome, " in state: ", lowest_income$STATE))

highest_salary <- irs2019_long[which.max(irs2019_long$AvgSalaryIncome), c('ZIPCODE', 'AvgSalaryIncome', 'STATE')]
lowest_salary <- irs2019_long[which.min(irs2019_long$AvgSalaryIncome), c('ZIPCODE', 'AvgSalaryIncome', 'STATE')]

print(paste("Highest salary ZIP: ", highest_salary$ZIPCODE, " with average salary: ", highest_salary$AvgSalaryIncome, " in state: ", highest_salary$STATE))
print(paste("Lowest salary ZIP: ", lowest_salary$ZIPCODE, " with average salary: ", lowest_salary$AvgSalaryIncome, " in state: ", lowest_salary$STATE))

highest_dividends <- irs2019_long[which.max(irs2019_long$AvgDividends), c('ZIPCODE', 'AvgDividends', 'STATE')]
lowest_dividends <- irs2019_long[which.min(irs2019_long$AvgDividends), c('ZIPCODE', 'AvgDividends', 'STATE')]

print(paste("Highest dividends ZIP: ", highest_dividends$ZIPCODE, " with average dividends: ", highest_dividends$AvgDividends, " in state: ", highest_dividends$STATE))
print(paste("Lowest dividends ZIP: ", lowest_dividends$ZIPCODE, " with average dividends: ", lowest_dividends$AvgDividends, " in state: ", lowest_dividends$STATE))

highest_re_taxes <- irs2019_long[which.max(irs2019_long$AvgRealEstateTaxes), c('ZIPCODE', 'AvgRealEstateTaxes', 'STATE')]
lowest_re_taxes <- irs2019_long[which.min(irs2019_long$AvgRealEstateTaxes), c('ZIPCODE', 'AvgRealEstateTaxes', 'STATE')]

print(paste("Highest real estate taxes ZIP: ", highest_re_taxes$ZIPCODE, " with average real estate taxes: ", highest_re_taxes$AvgRealEstateTaxes, " in state: ", highest_re_taxes$STATE))
print(paste("Lowest real estate taxes ZIP: ", lowest_re_taxes$ZIPCODE, " with average real estate taxes: ", lowest_re_taxes$AvgRealEstateTaxes, " in state: ", lowest_re_taxes$STATE))



irs2019_long$fraction_dividends <- irs2019_long$NumofRetwithDivis / irs2019_long$NumofReturns
fraction_distribution <- table(irs2019_long$fraction_dividends)


plot(irs2019_long$AvgIncome, irs2019_long$fraction_dividends, 
     xlab = "Income", ylab = "Fraction of receiving dividends", 
     main = "Scatterplot of Income vs Fraction of receiving dividends")



ggplot(irs2019_long, aes(x = AvgIncome, y = fraction_dividends, color = STATE)) +
  geom_point() +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Income", y = "Fraction of receiving dividends", color = "State") +
  ggtitle("Scatterplot of Income vs Fraction of Receiving Dividends by State") +
  theme_minimal()





irs2019_long$STATE <- factor(irs2019_long$STATE)

model <- lm(AvgIncome ~ fraction_dividends, data = irs2019_long)


plot(irs2019_long$fraction_dividends, irs2019_long$AvgIncome, 
     xlab = "Fraction of households receiving dividends", 
     ylab = "Household Income",
     main = "Regression of households Income on Fraction of Households receiving dividends")
abline(model, col = "red")


summary(model)


plot(model$residuals, 
     xlab = "Observation",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "blue")

