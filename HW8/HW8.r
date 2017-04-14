#SPencer Swartz
#########problem1#########
#read csv
#The original data was transposed so that the colums are now rows and visa-versa
Best.Cities <- read.csv("~/GitHub/ds710fall2016assignment8/Best Cities Transpose.csv", header = TRUE)
#create fucntion to score citys
CityScore<-function(x){
  score = as.numeric(x[2])/10
  if(x[5] < 20){ score = score + 5 }
  else if(x[5] < 25){ score = score + 3 }
  if((x[4] < 100 )&(x[11]>20)){ score = score + 5 }
  else if(x[11]>20){ score = score + 3 }
  if(as.numeric(x[7])/as.numeric(x[8]) > 26){ score = score + 5 }
  return(score)
} # end of function CityScore
#apply the fuction to the Best citys dataset
Scores = apply(Best.Cities, 1, CityScore)
Scores
Best.Cities[which.max(Scores),1] #Austin City scored the highest
#########Problem2#########
mourner <- c(3,7,8,3,7,3,3,6,2,3,3,2,3,4,3,8,10,2,3,3,7,
             4,2,10,6,3,4,9,3,6,4,2,4,2,6,4,3,7,5,2,5,4,
             8,11,2,6,4,4,3,3,7,2,7,3,4,2,11,2,6,5,4,8,
             2,3,7,2,4,6,4,3,5,6,2,3,5,10,5,6,5,4,8,8,7,
             2,3,8,7,2,3,6,3,6,2,3,9,3,6,4,3,3,7,3,5,2,
             9,3,8,8,2,6,4,3,4,5,2,3,3,4,2,7,5,6,8,4,3,
             7,6,6,5,2,3,6,12,6,6,2,5,5,5,6,2,5,2,3,1,7,
             6,3,5,4,4,1,6,3,1,7)
mournerAvg <- mean(mourner)
mournerSD <- sd(mourner)
mournerSize <- length(mourner)
handcockAvg <- 4.69
handcockSD <- 2.6
handcockSize <- 121
#create t.test2 function
# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE.
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}
#test mourner
t.test2(m1 = mournerAvg,m2 = handcockAvg,s1 = mournerSD,s2 = handcockSD,n1 = mournerSize,n2 = handcockSize)
#########Problem3#########
sentence_lengths <- read.csv("~/GitHub/ds710fall2016assignment8/sentence_lengths.csv", header = TRUE)
attach(sentence_lengths)
###Part a###
plot(avg_word_length,word_count)
###Part c###
plot(log(avg_word_length),word_count)
model <- lm(word_count ~ log(avg_word_length))
summary(model)
abline(model)
###Part d###
plot(model)