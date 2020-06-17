## PART 1

remove(list = ls())

install.packages("corrplot")
library(corrplot)

BodyFat <- read.csv("BodyFat.csv")

x <- cor(BodyFat)
corrplot(x ,method = "number", type = "lower")

plot(BODYFAT ~ DENSITY + AGE + WEIGHT + HEIGHT + ADIPOSITY +
       NECK + CHEST + ABDOMEN + HIP + THIGH + KNEE + ANKLE +
       BICEPS + FOREARM + WRIST, data = BodyFat)


## PART 2

adiposity <- BodyFat$ADIPOSITY
hist(adiposity, main = "", xlab = "Adiposity values")
qqnorm(adiposity, main = "", xlab = "Qauntiles", ylab = "Adiposity values")

thigh <- BodyFat$THIGH
hist(thigh, main = "", xlab = "Thigh values")
qqnorm(thigh, main = "", xlab = "Qauntiles", ylab = "Thigh values")

ankle <- BodyFat$ANKLE
hist(ankle, main = "", xlab = "Ankle values")
qqnorm(ankle, main = "", xlab = "Qauntiles", ylab = "Ankle values")

cor(adiposity, thigh)
cor(adiposity, ankle)
cor(thigh, ankle)


## PART 3

# simple linear regression
model1 <- lm(BODYFAT ~ ADIPOSITY, data = BodyFat)
summary(model1)
plot(BODYFAT ~ ADIPOSITY, data = BodyFat)
abline(model1, col = "red")

model2 <- lm(BODYFAT ~ THIGH, data = BodyFat)
summary(model2)
plot(BODYFAT ~ THIGH, data = BodyFat)
abline(model2, col = "red")

model3 <- lm(BODYFAT ~ ANKLE, data = BodyFat)
summary(model3)
plot(BODYFAT ~ ANKLE, data = BodyFat)
abline(model3, col = "red")


# multiple regression
m1 <- lm(BODYFAT ~ KNEE + BICEPS + AGE, data = BodyFat)
summary(m1)

m2 <- lm(BODYFAT ~ ADIPOSITY + THIGH + ANKLE, data = BodyFat)
summary(m2)

m3 <- lm(BODYFAT ~ ADIPOSITY + CHEST + WRIST, data = BodyFat)
summary(m3)

m4 <- lm(BODYFAT ~ ADIPOSITY + ABDOMEN + CHEST, data = BodyFat)
summary(m4) 

summary(lm(BODYFAT ~ CHEST, data = BodyFat))
summary(lm(BODYFAT ~ ABDOMEN, data = BodyFat))
summary(lm(BODYFAT ~ ADIPOSITY, data = BodyFat))

# my model
library(MASS)
fit <- lm(BODYFAT ~ ABDOMEN + ADIPOSITY + CHEST + HIP + WEIGHT, data = BodyFat)
step <-stepAIC(fit, direction="both")
step$anova

fit2 <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = BodyFat)
summary(fit2)
anova(fit, fit2)


## STEP 4

pounds <- BodyFat$WEIGHT
inches <- BodyFat$HEIGHT

bmi <- pounds * 703 / (inches^2)

abdomen <- BodyFat$ABDOMEN
cor(bmi, pounds)
cor(bmi, abdomen)

bmi_fit <- lm(bodyfat ~ abdomen + pounds + bmi, data = BodyFat)
summary(bmi_fit)

summary(lm(bodyfat ~ bmi))

# BMI classes
under <- bmi[bmi < 18.5]
under

bmi_normal <- bmi[bmi >= 18.5 & bmi <= 24.99]
abd_normal <- abdomen[bmi_normal]
wght_normal <- pounds[bmi_normal]
bfat_normal <- bodyfat[bmi_normal]

fit_normal <- lm(bfat_normal ~ bmi_normal + abd_normal + wght_normal)
summary(fit_normal)

bmi_over <- bmi[bmi >= 25 & bmi <= 29.99]
abd_over <- abdomen[bmi_over]
wght_over <- pounds[bmi_over]
bfat_over <- bodyfat[bmi_over]

fit_over <- lm(bfat_over ~ bmi_over + wght_over + abd_over)
summary(fit_over)

bmi_obese <- bmi[bmi >= 30]
abd_obese <- abdomen[bmi_obese]
wght_obese <- pounds[bmi_obese]
bfat_obese <- bodyfat[bmi_obese]

fit_obese <- lm(bfat_obese ~ bmi_obese + wght_obese + abd_obese)
summary(fit_obese)

