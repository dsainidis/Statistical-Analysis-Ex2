library("summarytools")
library(psych)

#reading the data
labels <- read.csv("AutoOnline_labels.csv", sep = ";")
values <- read.csv("AutoOnline_values.csv", sep = ";")

colnames(values)[1] <- 'qnrnumb'
colnames(labels)[1] <- 'qnrnumb'

yes.no_vector <- c('Yes', 'No')
often_vector <- c('Never', 'Almost never', 'Occasionally', 'Often', 'Very Often')
agree_vector <- c('Strongly Disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly Agree')
no.yes_vector <- c('No', 'Yes')
how.much_vector <- c('A great deal better', 'Much better', 'Somewhat better', 'Just a bit better')
marital_vector <- c('Single', 'Married', 'Widowed', 'Divorced', 'Seperated')
education_vector <- c('Less than high school', 'High school', 'Some college', 'Undergraduate degree', 'Graduate degree', 'Other')
race_vector <- c('Caucasian', 'Black', 'Asian', 'American Indian', 'Hispanic', 'Other')
income_vector <- c('Under $35,000', '$50,000 - $65,000', '$65,000 - $80,000', '$80,000 - $95,000', '$95,000 - $110,000', 'Over $110,000')
gender_vector <- c('Male', 'Female')
children_vector <- c('0', '1', '2', '3', '4', '5')

labels$visit <- factor(labels$visit, levels = yes.no_vector)
labels$howoft <- factor(labels$howoft, levels = often_vector, ordered = TRUE)
labels$likenet <- factor(labels$likenet, levels = agree_vector, ordered = TRUE)
labels$research <- factor(labels$research, levels = agree_vector, ordered = TRUE)
labels$safeweb <- factor(labels$safeweb, levels = agree_vector, ordered = TRUE)
labels$goodtool <- factor(labels$goodtool, levels = agree_vector, ordered = TRUE)
labels$notouse <- factor(labels$notouse, levels = agree_vector, ordered = TRUE)
labels$another <- factor(labels$another, levels = agree_vector, ordered = TRUE)
labels$likeproc <- factor(labels$likeproc, levels = agree_vector, ordered = TRUE)
labels$hassle <- factor(labels$hassle, levels = agree_vector, ordered = TRUE)
labels$friend <- factor(labels$friend, levels = no.yes_vector)
labels$billbrd <- factor(labels$billbrd, levels = no.yes_vector)
labels$banner <- factor(labels$banner, levels = no.yes_vector)
labels$surfing <- factor(labels$surfing, levels = no.yes_vector)
labels$sengine <- factor(labels$sengine, levels = no.yes_vector)
labels$tv <- factor(labels$tv, levels = no.yes_vector)
labels$theater <- factor(labels$theater, levels = no.yes_vector)
labels$nwspaper <- factor(labels$nwspaper, levels = no.yes_vector)
labels$other <- factor(labels$other, levels = no.yes_vector)
labels$easyuse <- factor(labels$easyuse, levels = agree_vector, ordered = TRUE)
labels$helpful <- factor(labels$helpful, levels = agree_vector, ordered = TRUE)
labels$positive <- factor(labels$positive, levels = agree_vector, ordered = TRUE)
labels$useonly <- factor(labels$useonly, levels = agree_vector, ordered = TRUE)
labels$influenc <- factor(labels$influenc, levels = agree_vector, ordered = TRUE)
labels$secure <- factor(labels$secure, levels = agree_vector, ordered = TRUE)
labels$didbuy <- factor(labels$didbuy, levels = yes.no_vector)
labels$better <- factor(labels$better, levels = yes.no_vector)
labels$howmuch <- factor(labels$howmuch, levels = how.much_vector, ordered = TRUE)
labels$safe <- factor(labels$safe, levels = agree_vector, ordered = TRUE)
labels$wanttest <- factor(labels$wanttest, levels = agree_vector, ordered = TRUE)
labels$price <- factor(labels$price, levels = agree_vector, ordered = TRUE)
labels$tradein <- factor(labels$tradein, levels = agree_vector, ordered = TRUE)
labels$handson <- factor(labels$handson, levels = agree_vector, ordered = TRUE)
labels$checkit <- factor(labels$checkit, levels = agree_vector, ordered = TRUE)
labels$marital <- factor(labels$marital, levels = marital_vector)
labels$educate <- factor(labels$educate, levels = education_vector)
labels$race <- factor(labels$race, levels = race_vector)
labels$income <- factor(labels$income, levels = income_vector, ordered = TRUE)
labels$gender <- factor(labels$gender, levels = gender_vector)
labels$children <- factor(labels$children, levels = children_vector, ordered = TRUE)

data <- labels
data_v <- values

summary_df = as.data.frame.matrix(summary(data), row.names = F)
write.csv(summary_df, "summary.csv", row.names = F, na = "")

drop = c('qnrnumb', 'visit', 'better', 'howmuch')
#----------------------------------------------------------------------------

data = data[,!(names(data) %in% drop)]
data_v = data_v[,!(names(data_v) %in% drop)]

attach(data)
#----------------------------------------------------------------------------
drop1 = c('age', 'sticker', 'actual', 'worth', 'noweeks', 'visits')
data_categ = data[,!(names(data) %in% drop1)]


names = colnames(data_categ)

for(i in 1:ncol(data_categ)) {
  print(paste("Frequency table for variable:", names[i], sep = " "))
  print(freq(data_categ[ , i]))
}
#----------------------------------------------------------------------------
par(las = 1)
for(i in 1:ncol(data_categ)) {
  count = prop.table(table(data_categ[ , i]))
  barplot(count, main = names[i],cex.names=0.75)
}
#----------------------------------------------------------------------------

detach("package:psych", unload = TRUE)
library(plotrix)

create_3dpie <- function(col, title) {
  values <- as.numeric(table(col))
  labels <- names(table(col))
  percetages <- round(values/sum(values)*100)
  labels <- paste(labels, percetages)
  labels <- paste(labels, "%", sep = "")
  pie3D(values, labels = labels, theta = 1.25, explode = 0.1, main = title)
}

print (ncol(data_categ))

for(i in 2:31) {
  create_3dpie(data_categ[ , i], names[i])
}

for(i in 33:37) {
  create_3dpie(data_categ[ , i], names[i])
}

#----------------------------------------------------------------------------
create_hist <- function(col, xLabel, title) {
  x <- col
  range = max(col, na.rm=TRUE) - min(col, na.rm=TRUE)
  if (range > 75) {
    range = 75
  }
  h <- hist(x, breaks = range/2, col="red", xlab = xLabel, main = paste("Histogram of", xLabel, sep = " "))
  xfit <- seq(min(x), max(x), length = 40)
  yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
  yfit <- yfit * diff(h$mids[1:2]) * length(x)
  lines(xfit, yfit, col = "blue", lwd = 2)
}

create_hist(age, "Age")
create_hist(actual, "Actual")
create_hist(visits, "Visits")

boxplot(age, xlab = "age")
boxplot(actual, xlab = "actual")
boxplot(visits, xlab = "visits")

 create_qqplot <- function(col, var) {
  print(summary(col))
  qqnorm(col, col = 'Red', main = paste('Q-Q Plot: ', var, sep = " "))
  qqline(col, col = 'Blue', lwd = 2)
}

create_qqplot(data$age, 'age')
create_qqplot(data$actual, 'actual')
create_qqplot(data$visits, 'visits')

create_qqplot(log(data$age), 'log(age)')
create_qqplot(log(data$actual), 'log(actual)')
create_qqplot(log(data$visits), 'log(visits)')

library(psych)
#----------------------------------------------------------------------------
describe(data[, c('age', 'actual', 'visits')])
describe_df = as.data.frame.matrix(describe(data[, c('age', 'actual', 'visits')]), row.names = F)
write.csv(describe_df, "describtion.csv", row.names = F, na = "")
#----------------------------------------------------------------------------

#ci for "actual"
len = length(data$actual)
a = numeric(100000)
for(j in 1:100000) {
  a[j] = mean(sample(data$actual, len, replace = T))
}
hist(a, main = 'Confidence intervals for actual')
abline(v = quantile(a,0.025), col='red')
abline(v = quantile(a,0.975), col='blue')

detach("package:psych", unload = TRUE)
library(DescTools)
#---------------------------------------------------------------------------
#question 1

#names = colnames(data)

#for(i in 1:10){
#  plot(data[,i], didbuy, xlab= names[i])
#}

#for(i in 20:25){
#  plot(data[,i], didbuy, xlab= names[i])
#}

for(i in 1:10){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(summary(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

for(i in 20:25){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(summary(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

print(PostHocTest(aov(data_v$didbuy~goodtool)))
goodtool2 = factor(goodtool)
levels(goodtool)
levels(goodtool2)
levels(goodtool2)[c(1,2,3)] = "Disagree"
levels(goodtool2)[c(2,3)] = "Agree"
levels(goodtool2)
print(PostHocTest(aov(data_v$didbuy~goodtool2)))

plot(goodtool, didbuy, xlab = "goodtool")
plot(goodtool2, didbuy, xlab = "goodtool2")

#for(i in 1:10){
#  print("-----------------------------------------------------------------")
#  print(paste(names[i], i, sep=" "))
#  print(PostHocTest(aov(data_v$didbuy~data[,i])))
#  print("-----------------------------------------------------------------")
#}

for(i in 21:24){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(PostHocTest(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

helpful2 = factor(helpful)
levels(helpful)
levels(helpful2)
levels(helpful2)[c(1,2)] = "Disagree"
levels(helpful2)[c(2,3,4)] = "Agree"
levels(helpful2)
print(PostHocTest(aov(data_v$didbuy~helpful2)))

plot(helpful, didbuy, xlab = "helpful")
plot(helpful2, didbuy, xlab= "helpful")

positive2 = factor(positive)
levels(positive)
levels(positive2)
levels(positive2)[c(1,2)] = "Disagree"
levels(positive2)[c(2,3)] = "Agree"
levels(positive2)
print(PostHocTest(aov(data_v$didbuy~positive2)))

plot(positive, didbuy, xlab = "positive")
plot(positive2, didbuy, xlab= "positive2")

useonly2 = factor(useonly)
levels(useonly)
levels(useonly2)
levels(useonly2)[c(1,2,3)] = "Disagree"
levels(useonly2)[c(2,3)] = "Agree"
levels(useonly2)
print(PostHocTest(aov(data_v$didbuy~useonly2)))

plot(useonly, didbuy, xlab = "useonly")
plot(useonly2, didbuy, xlab = "useonly2")

influenc2 = factor(influenc)
levels(influenc)
levels(influenc2)
levels(influenc2)[c(1,2)] = "Disagree"
levels(influenc2)
print(PostHocTest(aov(data_v$didbuy~influenc2)))

plot(influenc, didbuy, xlab = "influenc")
plot(influenc2, didbuy, xlab = "influenc2")

model1 = lm(data_v$didbuy ~ goodtool2 + helpful2 + positive2 + useonly2 + influenc2 + 
               goodtool2:helpful2 + goodtool2:positive2 + goodtool2:useonly2 + goodtool2:influenc2 +
               helpful2:positive2 + helpful2:useonly2 + helpful2:influenc2 + 
               positive2:useonly2 + positive2:influenc2 + useonly2:influenc2)

summary(model1)

model12 = lm(data_v$didbuy ~ influenc2 + helpful2:useonly2)

summary(model12)

#model13.dim = ols_step_both_p(model12)

#---------------------------------------------------------------------------
#question 2

#names = colnames(data)

#for(i in 27:32){
#  plot(data[,i], didbuy, xlab= names[i])
#}

for(i in 27:32){
  print(paste(names[i], i, sep=" "))
  print(summary(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

#for(i in 27:32){
#  print("-----------------------------------------------------------------")
#  print(paste(names[i], i, sep=" "))
#  print(PostHocTest(aov(data_v$didbuy~data[,i])))
#  print("-----------------------------------------------------------------")
#}

print(PostHocTest(aov(data_v$didbuy~safe)))
safe2 = factor(safe)
levels(safe)
levels(safe2)
levels(safe2)[c(2,3,4,5)] = "Not Disagree"
levels(safe2)
print(PostHocTest(aov(data_v$didbuy~safe2)))

plot(safe, didbuy, xlab= "safe")
plot(safe2, didbuy, xlab= "safe2")

print(PostHocTest(aov(data_v$didbuy~price)))
price2 = factor(price)
levels(price2)[c(2,3)] = "Neutral"
levels(price)
levels(price2)
print(PostHocTest(aov(data_v$didbuy~price2)))

plot(price, didbuy, xlab= "price")
plot(price2, didbuy, xlab= "price2")

model2 = lm(data_v$didbuy ~ safe2*price2)
summary(model2)
#model22.dim = ols_step_both_p(model2)
#---------------------------------------------------------------------------
#question 3

names = colnames(data)

for(i in 37:43){
  plot(data[,i], actual, xlab= names[i])
}

for(i in 37:43){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(summary(aov(actual~data[,i])))
  print("-----------------------------------------------------------------")
}

print(cor.test(actual, age, method = 'pearson'))

for(i in 38:43){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(PostHocTest(aov(actual~data[,i])))
  print("-----------------------------------------------------------------")
}

#---------------------------------------------------------------------------
#question 4

names = colnames(data)

for(i in 11:19){
  plot(data[,i], didbuy, xlab= names[i])
}

for(i in 11:19){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(summary(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

for(i in 11:19){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(PostHocTest(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

#---------------------------------------------------------------------------

model = lm(actual ~ income)
summary(model)
plot(model)
