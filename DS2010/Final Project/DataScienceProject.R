?state.x77
smp_siz = floor(0.70*nrow(state.x77))  # creates a value for dividing the data into train and test. In this case the value is defined as 70% of the number of rows in the dataset
smp_siz  # shows the value of the sample size
set.seed(123)   # set seed to ensure you always have same random numbers generated
train_ind = sample(seq_len(nrow(state.x77)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =state.x77[train_ind,] #creates the training dataset with row numbers stored in train_ind
train
test=state.x77[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind
test

pairs(state.x77)

#Turning matrix columns into df-columns for analysis
Population <- train[,1]
Income <- train[,2]
Illiteracy <- train[,3]
LifeExp <- train[,4]
Murder <- train[,5]
HSGrad <- train[,6]
Frost <- train[,7]
Area <- train[,8]

#Plots HSGrad v. all other variables
plot(HSGrad, Population) #looks maybe binomial
plot(HSGrad, Income) #looks positive and linear
plot(HSGrad, Illiteracy) #looks negative and linear
plot(HSGrad, LifeExp) #looks positive and linear
plot(HSGrad, Murder) #looks maybe binomial

#Correlation tests
cor.test(HSGrad, Population, method="pearson")
cor.test(HSGrad, Income, method="pearson")
cor.test(HSGrad, Illiteracy, method="pearson")
cor.test(HSGrad, LifeExp, method="pearson")
cor.test(HSGrad, Murder, method="pearson")

#Creating polynomial models
#Irrelevant
model1.1 <- lm(Population ~ HSGrad)
model1.2 <- lm(Population ~ poly(HSGrad, 2, raw = TRUE))
model1.3 <- lm(Population ~ poly(HSGrad, 3, raw = TRUE))
model1.4 <- lm(Population ~ poly(HSGrad, 4, raw = TRUE))
model1.5 <- lm(Population ~ poly(HSGrad, 5, raw = TRUE))
model1.6 <- lm(Population ~ poly(HSGrad, 6, raw = TRUE))

model2.1 <- lm(Income ~ HSGrad)
model2.2 <- lm(Income ~ poly(HSGrad, 2, raw = TRUE))
model2.3 <- lm(Income ~ poly(HSGrad, 3, raw = TRUE))
model2.4 <- lm(Income ~ poly(HSGrad, 4, raw = TRUE))
model2.5 <- lm(Income ~ poly(HSGrad, 5, raw = TRUE))
model2.6 <- lm(Income ~ poly(HSGrad, 6, raw = TRUE))

model3.1 <- lm(Illiteracy ~ HSGrad)
model3.2 <- lm(Illiteracy ~ poly(HSGrad, 2, raw = TRUE))
model3.3 <- lm(Illiteracy ~ poly(HSGrad, 3, raw = TRUE))
model3.4 <- lm(Illiteracy ~ poly(HSGrad, 4, raw = TRUE))
model3.5 <- lm(Illiteracy ~ poly(HSGrad, 5, raw = TRUE))
model3.6 <- lm(Illiteracy ~ poly(HSGrad, 6, raw = TRUE))

model4.1 <- lm(LifeExp ~ HSGrad)
model4.2 <- lm(LifeExp ~ poly(HSGrad, 2, raw = TRUE))
model4.3 <- lm(LifeExp ~ poly(HSGrad, 3, raw = TRUE))
model4.4 <- lm(LifeExp ~ poly(HSGrad, 4, raw = TRUE))
model4.5 <- lm(LifeExp ~ poly(HSGrad, 5, raw = TRUE))
model4.6 <- lm(LifeExp ~ poly(HSGrad, 6, raw = TRUE))

model5.1 <- lm(Murder ~ HSGrad)
model5.2 <- lm(Murder ~ poly(HSGrad, 2, raw = TRUE))
model5.3 <- lm(Murder ~ poly(HSGrad, 3, raw = TRUE))
model5.4 <- lm(Murder ~ poly(HSGrad, 4, raw = TRUE))
model5.5 <- lm(Murder ~ poly(HSGrad, 5, raw = TRUE))
model5.6 <- lm(Murder ~ poly(HSGrad, 6, raw = TRUE))

#Looking at the summary of each polynomial model
print(anova(model2.1,model2.2,model2.3,model2.4,model2.5,model2.6)) #linear
print(anova(model3.1,model3.2,model3.3,model3.4,model3.5,model3.6)) #linear
print(anova(model4.1,model4.2,model4.3,model4.4,model4.5,model4.6)) #linear
print(anova(model5.1,model5.2,model5.3,model5.4,model5.5,model5.6)) #power of 2 or 3, choose ^2 because not big set of training data, simpler = better

#Summary for polynomial models
summary(model5.2)
summary(model5.3)

#See models with training data
#HSGrad v. Population: polynomial model (power of 2)
plot(HSGrad, Murder, main = "High School Graduation Percent v. Murder Rate", xlab = "% high-school graduates (1970)", ylab = "Murder rate per 100,000 population (1976)")
lines(sort(HSGrad), fitted(model5.2)[order(HSGrad)], col='red')
#HSGrad v. Murder: polynomial model (power of 3)
plot(HSGrad, Murder, main = "High School Graduation Percent v. Murder Rate", xlab = "% high-school graduates (1970)", ylab = "Murder rate per 100,000 population (1976)")
lines(sort(HSGrad), fitted(model5.3)[order(HSGrad)], col='red')

plot(HSGrad, LifeExp, main = "High School Graduation Percent v. LifeExp", xlab = "% high-school graduates (1970)", ylab = "LifeExp")
lines(sort(HSGrad), fitted(model4.2)[order(HSGrad)], col='red')

#Testing Goodness of Fit of each model
plot(model5.2)
plot(model5.3)

#Testing error
LifeExp <- train[,4]
Murder <- test[,5]
HSGrad <- test[,6]

y0hat.murder2 = predict(model5.2, data.frame(x=HSGrad))
test.err.murder2 = mean((Murder-y0hat.murder2)^2)
print(test.err.murder2)

y0hat.murder3 = predict(model5.3, data.frame(x=HSGrad))
test.err.murder3 = mean((Murder-y0hat.murder3)^2)
print(test.err.murder3)

y0hat.life = predict(model4.2, data.frame(x=HSGrad))
test.err.life = mean((LifeExp-y0hat.life)^2)
print(test.err.life)

