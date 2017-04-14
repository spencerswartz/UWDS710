#####Spencer Swartz####
####1.a####
usnews_clean <- read.csv("~/Data Science MS/Programing for Data Science/HW9/usnews_clean.csv",header = TRUE)
head(usnews_clean)
attach(usnews_clean)
####1.b####
summary(Graduation.rate)
hist(Graduation.rate)
boxplot(Graduation.rate)
which(Graduation.rate>100)
#772
Graduation.rate[772]
College.Name[772]
Graduation.rate[772] = NA
####1.c####
private = Pct.alumni.who.donate[which(College.Type == 'private')]
private = private[which(!is.na(private))]
public = Pct.alumni.who.donate[which(College.Type == 'public')]
public = public[which(!is.na(public))]
mean(private)
mean(public)
####1.d####
t.test(x = private, y = public, alternative = 'greater')
####1.e####
usnews_clean$Graduation.rate <- Graduation.rate
write.csv(usnews_clean, file = '~/Data Science MS/Programing for Data Science/HW9/usnews_clean_R.csv')
####2.a####
cps <- read.csv("~/Data Science MS/Programing for Data Science/HW9/cps.csv",header = TRUE)
attach(cps)
plot(educ,wage)
####2.b####
log_wage = log(wage)
plot(educ,log_wage)
model <- lm(log_wage ~ educ)
abline(model)
plot(model)
####2.c####
time <- 1/wage
####2.d####
plot(educ,time)
####2.e####
log_time <- log(time)
plot(educ,log_time)
model2 <- lm(log_time ~educ)
abline(model2)
####2.f####
plot(model2)
model3 <- lm(time ~ educ)
plot(model3)
removepoints <- c(507,216,518)
time_remove <- time[-removepoints]
educ_remove <- educ[-removepoints]
plot(educ_remove,time_remove)
model4 <- lm(time_remove~educ_remove)
abline(model4)
