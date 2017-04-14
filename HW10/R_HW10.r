#Spencer Swartz
######1.a######
usnews_clean_R <- read.csv("~/Data Science MS/Programing for Data Science/HW10/usnews_clean_R.csv", header = TRUE)
attach(usnews_clean_R)
#Instructional.expenditure.per.student > Out.of.state.tuition
#length which version
length(which(Instructional.expenditure.per.student > Out.of.state.tuition))
#control flow version
control_flow <- function(data){
  count=0
  for(i in (1:length(data$Instructional.expenditure.per.student))){
    if ((!is.na(data$Instructional.expenditure.per.student[i])) & 
        (!is.na(data$Out.of.state.tuition[i])) & 
        (data$Instructional.expenditure.per.student[i] > data$Out.of.state.tuition[i])){
      count = count+1
    }
  }
  return(count)
}
control_flow(usnews_clean_R)
######1.b######
system.time(for(i in 1:100) length(which(Instructional.expenditure.per.student > Out.of.state.tuition)))
system.time(for(i in 1:100) control_flow(usnews_clean_R))
######1.c.i######
usnews_clean_R =usnews_clean_R[,3:length(usnews_clean_R)]
usnews_nums <- usnews_clean_R[,!names(usnews_clean_R) %in% c('College.Name','College.Type','State')]
#i.	Using apply() and the built-in function mean()
apply(usnews_nums, 2, mean, na.rm=TRUE)
######1.c.ii######
#ii.	Using apply() and a function you write, called mymean(), 
#which takes the sum of all of the non-missing values and divides by the number of non-missing values
mymean <- function(data_col){
  sum(data_col,na.rm = TRUE)/length(which(!is.na(data_col)))
}
apply(usnews_nums, 2, mymean)
######1.c.iii######
#iii.	Using a for() loop to iterate over the numeric columns, 
#and a for() loop inside it to iterate over the values within that column
mymeanfor <- function(data){
  avgs=c()
  for(i in 1:dim(data)[2]){
    sumnum=0
    count=0
    for(j in (1:dim(data)[1])){
      if(!is.na(as.numeric(data[j,i]))){
        sumnum=sumnum+as.numeric(data[j,i])
        count=count+1
      }
    }
    avg=sumnum/count
    avgs[i] <- avg
  }
  return(avgs)
}
mymeanfor(usnews_nums)
######1.d######
library(microbenchmark)
microbenchmark(apply(usnews_nums, 2, mean, na.rm=TRUE),apply(usnews_nums, 2, mymean),mymeanfor(usnews_nums), times = 25)