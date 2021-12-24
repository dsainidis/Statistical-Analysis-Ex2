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

data <- labels
data_v <- values

summary(data)

drop = c('qnrnumb', 'visit', 'better', 'howmuch')
#----------------------------------------------------------------------------

data = data[,!(names(data) %in% drop)]
data_v = data_v[,!(names(data_v) %in% drop)]

summary(data)
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
  barplot(count, main = names[i])
}
#----------------------------------------------------------------------------

for(i in 1:ncol(data_categ)) {
  count = prop.table(table(data_categ[ , i]))
  pie(count, main = names[i])
}

#----------------------------------------------------------------------------
hist(data$age, main = "Histogram of age")
hist(data$sticker, main = "Histogram of sticker")
hist(data$worth, main = "Histogram of worth")
hist(data$actual, main = "Histogram of actual")
hist(data$noweeks, main = "Histogram of noweeks")
hist(data$visits, main = "Histogram of visits")

boxplot(data$age, xlab = "age")
boxplot(data$sticker, xlab = "sticker")
boxplot(data$worth, xlab = "worth")
boxplot(data$actual, xlab = "actual")
boxplot(data$noweeks, xlab = "noweeks")
boxplot(data$visits, xlab = "visits")

create_qqplot <- function(col, var) {
  print(summary(col))
  qqnorm(col, col = 'Red', main = paste('Q-Q Plot: ', var, sep = " "))
  qqline(col, col = 'Blue', lwd = 2)
}

create_qqplot(data$age, 'age')
create_qqplot(data$sticker, 'sticker')
create_qqplot(data$worth, 'worth')
create_qqplot(data$actual, 'actual')
create_qqplot(data$noweeks, 'noweeks')
create_qqplot(data$visits, 'visits')

create_qqplot(log(data$age), 'log(age)')
create_qqplot(log(data$sticker), 'log(sticker)')
create_qqplot(log(data$worth), 'log(worth)')
create_qqplot(log(data$actual), 'log(actual)')
create_qqplot(log(data$noweeks), 'log(noweeks)')
create_qqplot(log(data$visits), 'log(visits)')

#----------------------------------------------------------------------------
describe(data[, c('age','sticker', 'worth', 'actual', 'noweeks', 'visits')])
#----------------------------------------------------------------------------

#ci for "actual"
len = length(data$actual)
a = numeric(100000)
for(j in 1:100000) {
  a[j] = mean(sample(data$actual, len, replace = T))
}
a
hist(a)
abline(v = quantile(a,0.025), col='red')
abline(v = quantile(a,0.975), col='blue')

detach("package:psych", unload = TRUE)
library(DescTools)
#---------------------------------------------------------------------------
#question 1

names = colnames(data)

for(i in 1:10){
  plot(data[,i], didbuy, xlab= names[i])
}

for(i in 20:25){
  plot(data[,i], didbuy, xlab= names[i])
}

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

for(i in 1:10){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(PostHocTest(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

for(i in 20:25){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(PostHocTest(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

#---------------------------------------------------------------------------
#question 2

names = colnames(data)

for(i in 27:32){
  plot(data[,i], didbuy, xlab= names[i])
}

for(i in 27:32){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(summary(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

for(i in 27:32){
  print("-----------------------------------------------------------------")
  print(paste(names[i], i, sep=" "))
  print(PostHocTest(aov(data_v$didbuy~data[,i])))
  print("-----------------------------------------------------------------")
}

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

#to be changed
for(i in 37:43){
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



