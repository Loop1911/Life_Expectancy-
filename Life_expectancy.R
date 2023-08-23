# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('Life Expectancy Data.csv')
dataset = dataset[, 3:22]

# Encoding categorical data
dataset$Status = factor(dataset$Status,
                       levels = c('Developing', 'Developed'),
                       labels = c(1, 2))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools'
library(caTools)
set.seed(123)
split = sample.split(dataset$Life.expectancy, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)


#Features rejected using backward elimination : thinness 1-19 years, thinness 5-9 years, GDP, Population {STEP 1}
#Features rejected using backward elimination : Hepatitis B, Measles {STEP 2}
#Features rejected using backward elimination : Total Expenditure, Alcohol {STEP 3}

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Life.expectancy ~ Status + Adult.Mortality + infant.deaths + percentage.expenditure + 
                 BMI + under.five.deaths + Polio + Diphtheria + HIV.AIDS + Income.composition.of.resources + Schooling,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

#Visualizing Results
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$Life.expectancy, y = y_pred),
             colour = 'red') +
  ggtitle('Life Expectancy vs Predicted Life Expectancy') +
  xlab('Life Expectancy') +
  ylab('Predicted Life Expectancy')