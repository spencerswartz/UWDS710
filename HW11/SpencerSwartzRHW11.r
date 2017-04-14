#Spencer Swartz
####1.a####
data=scan("~/Data Science MS/Programing for Data Science/HW11/Amazon_reviews.csv",skip = 1,
          nlines = 570000,sep = ',',
          what=list('numeric','character','numeric','numeric','numeric','numeric','numeric','numeric'))
####1.b####
datamatrix=t(matrix(data,nr=8))
dataheader=scan("~/Data Science MS/Programing for Data Science/HW11/Amazon_reviews.csv",
                nlines = 1,what = character(), sep = ",")
colnames(datamatrix) = dataheader
####1.c####
totalvotes = as.numeric(unlist(datamatrix[5]))
reviewlength = as.numeric(unlist(datamatrix[6]))
exclamation = as.numeric(unlist(datamatrix[7]))
helpfullfrac = as.numeric(unlist(datamatrix[8]))
####1.d####
hist(helpfullfrac)
max(helpfullfrac)
min(helpfullfrac)
####1.e####
####1.f####
#which(as.numeric(unlist(helpfullfrac))>.5)
has.reviews = helpfullfrac[which(!is.na(helpfullfrac))]
helpfull.reviews = numeric(length(has.reviews))
#condition=as.numeric(unlist(has.reviews))
for(i in 1:length(has.reviews)){
  if(has.reviews[i]>.5){
    helpfull.reviews[i]='helpfull'
  }
  else{
    helpfull.reviews[i]='not'
  }
  if(i %% 100 == 0){
    print(i)
    flush.console()
  }
}
####1.g####
mean(reviewlength[which(helpfull.reviews=='helpfull')])
mean(reviewlength[which(helpfull.reviews=='not')])
boxplot(reviewlength[which(!is.na(helpfullfrac))] ~ helpfull.reviews)
boxplot(log(reviewlength[which(!is.na(helpfullfrac))]) ~ helpfull.reviews)
t.test(reviewlength[which(helpfull.reviews=='helpfull')],reviewlength[which(helpfull.reviews=='not')],
       alternative = "greater")
####1.h####
product = as.character(unlist(datamatrix[2]))
a= rep(1, length(totalvotes))
maxall = tapply(totalvotes,a,max)
maxvotes =tapply(totalvotes,product,max)
####1.i####
numprodreview =tapply(a,product,sum)
plot(numprodreview~maxvotes)
####1.j####
sub.numprodreview = numprodreview[which(maxvotes!=0)]
sub.maxvotes = maxvotes[which(maxvotes!=0)]
plot(log(sub.numprodreview)~log(sub.maxvotes))
