#load the clean data set
CFP <- read.csv("~/Data Science MS/Programing for Data Science/FinalProject/CFBClean.csv", header = TRUE, stringsAsFactors = FALSE)
CFP$created_at <- as.POSIXct(CFP$created_at,format="%Y-%m-%d %H:%M:%S")
attach(CFP)
#examin the time data and plot chart of tweets and retweets
library('ggplot2')
CFP$hour <- as.POSIXct(cut(CFP$created_at, breaks = "hour"),format="%Y-%m-%d %H:%M:%S")
CFP$count <- 1
max(created_at)
min(created_at)
max(created_at)-min(created_at)
ggplot(data = CFP,
       aes(hour, count, group=retweeted_status, colour=retweeted_status)) +
  stat_summary(fun.y = sum, 
               geom = "line", size=2) +
  ggtitle("Number of Tweets overtime") +
  labs(x="Hour",y="# Tweets") 
#Chart tweets realted to each tweet_source
ggplot(data = CFP,
       aes(hour, count, group=tweetsource, colour=tweetsource)) +
  stat_summary(fun.y = sum, 
               geom = "line", size=1)+
  ggtitle("Tweets overtime by team tag and hash") +
  labs(x="Hour",y="# Tweets") 
CFP$team <- ifelse(tweetsource =='#ALLIN-Clemson'|tweetsource=='@ClemsonFB','Clemson',
                   ifelse(tweetsource =='#GoBlue-Michigan' | tweetsource=='@UMichFootball','Michigan',
                          ifelse(tweetsource =='#GoBucks-OhioState' | tweetsource=='@OhioStateFB','Ohio State',
                                 ifelse(tweetsource=="#OnWisconsin-Wisconsin" ,'Wisconsin',
                                        ifelse(tweetsource=='#PSUnrivaled-PennState' | tweetsource=='@PennStateFball','Penn State',
                                               ifelse(tweetsource=='#PurpleReign-Washington' | tweetsource=='@UW_Football', 'Washington',
                                                      ifelse(tweetsource=="@BadgerFootball",'Wisconsin',
                                                             ifelse(tweetsource=="@AlabamaFTBL",'Alabama',
                                                                    ifelse(tweetsource=="@OU_Football",'Oklahoma','zzz')))))))))
ggplot(data = CFP,
       aes(hour, count, group=team, colour=team)) +
  stat_summary(fun.y = sum, 
               geom = "line", size=1)+
  ggtitle("Tweets overtime by team name") +
  labs(x="Hour",y="# Tweets") 
#Compute avgs combined sentiment for each team
aggregate(CFP[,10:13], list(CFP$team,CFP$retweeted_status), mean)
aggregate(CFP[,10:13], list(CFP$team), mean)
#visualize the four sentiment columns
ggplot(data = CFP,
       aes(hour, compound, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1) +
  ggtitle("Avg.Sentiment by hour") +
  labs(x="Hour",y="Sentiment") +
  theme(legend.position="none")
ggplot(data = CFP,
       aes(hour, pos, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1) +
  ggtitle("Avg. Positive Sentiment by hour") +
  labs(x="Hour",y="+Sentiment")+
  theme(legend.position="none")
ggplot(data = CFP,
       aes(hour, neg, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1)+
  ggtitle("Avg. Negitive Sentiment by hour") +
  labs(x="Hour",y="-Sentiment")+
  theme(legend.position="none")
#Visualize moving average of sentiment
comp_moving_mean <- function(data){
  count=0
  sumlist=0
  outvect=c()
  for(i in data){
    #print(i)
    count=count+1
    sumlist=sumlist+as.numeric(i)
    moveavg=sumlist/count
    outvect[count]=moveavg
   
  }
  return(outvect)
}
CFP <- CFP[order(CFP$team,CFP$created_at),]
bama <- CFP[CFP$team=='Alabama',"compound"]
clem <- CFP[CFP$team=='Clemson',"compound"]
mich <- CFP[CFP$team=='Michigan',"compound"]
ohio <- CFP[CFP$team=='Ohio State',"compound"]
okle <- CFP[CFP$team=='Oklahoma',"compound"]
penn <- CFP[CFP$team=='Penn State',"compound"]
wash <- CFP[CFP$team=='Washington',"compound"]
wisc <- CFP[CFP$team=='Wisconsin',"compound"]
bamacompmoving <- comp_moving_mean(bama)
clemcompmoving <- comp_moving_mean(clem)
michcompmoving <- comp_moving_mean(mich)
ohiocompmoving <- comp_moving_mean(ohio)
oklecompmoving <- comp_moving_mean(okle)
penncompmoving <- comp_moving_mean(penn)
washcompmoving <- comp_moving_mean(wash)
wisccompmoving <- comp_moving_mean(wisc)
CFP$compound_moving_avg <- c(bamacompmoving,clemcompmoving,michcompmoving,ohiocompmoving,
               oklecompmoving,penncompmoving,washcompmoving,wisccompmoving)

ggplot(data = CFP,
       aes(hour, compound_moving_avg, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1)+
  theme(legend.position="none") +
  ggtitle("Running Avg. Sentiment Over Time") +
  labs(x="Hour",y="Sentiment") 
############################################
CFP <- CFP[order(CFP$team,CFP$created_at),]
bama <- CFP[CFP$team=='Alabama',"neg"]
clem <- CFP[CFP$team=='Clemson',"neg"]
mich <- CFP[CFP$team=='Michigan',"neg"]
ohio <- CFP[CFP$team=='Ohio State',"neg"]
okle <- CFP[CFP$team=='Oklahoma',"neg"]
penn <- CFP[CFP$team=='Penn State',"neg"]
wash <- CFP[CFP$team=='Washington',"neg"]
wisc <- CFP[CFP$team=='Wisconsin',"neg"]
bamacompmoving <- comp_moving_mean(bama)
clemcompmoving <- comp_moving_mean(clem)
michcompmoving <- comp_moving_mean(mich)
ohiocompmoving <- comp_moving_mean(ohio)
oklecompmoving <- comp_moving_mean(okle)
penncompmoving <- comp_moving_mean(penn)
washcompmoving <- comp_moving_mean(wash)
wisccompmoving <- comp_moving_mean(wisc)
CFP$neg_moving_avg <- c(bamacompmoving,clemcompmoving,michcompmoving,ohiocompmoving,
                             oklecompmoving,penncompmoving,washcompmoving,wisccompmoving)
ggplot(data = CFP,
       aes(hour, neg_moving_avg, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1)+
  theme(legend.position="none") +
  ggtitle("Running Avg. Negitive Sentiment Over Time") +
  labs(x="Hour",y="-Sentiment") 
###############################################################
CFP <- CFP[order(CFP$team,CFP$created_at),]
bama <- CFP[CFP$team=='Alabama',"pos"]
clem <- CFP[CFP$team=='Clemson',"pos"]
mich <- CFP[CFP$team=='Michigan',"pos"]
ohio <- CFP[CFP$team=='Ohio State',"pos"]
okle <- CFP[CFP$team=='Oklahoma',"pos"]
penn <- CFP[CFP$team=='Penn State',"pos"]
wash <- CFP[CFP$team=='Washington',"pos"]
wisc <- CFP[CFP$team=='Wisconsin',"pos"]
bamacompmoving <- comp_moving_mean(bama)
clemcompmoving <- comp_moving_mean(clem)
michcompmoving <- comp_moving_mean(mich)
ohiocompmoving <- comp_moving_mean(ohio)
oklecompmoving <- comp_moving_mean(okle)
penncompmoving <- comp_moving_mean(penn)
washcompmoving <- comp_moving_mean(wash)
wisccompmoving <- comp_moving_mean(wisc)
CFP$pos_moving_avg <- c(bamacompmoving,clemcompmoving,michcompmoving,ohiocompmoving,
                        oklecompmoving,penncompmoving,washcompmoving,wisccompmoving)
ggplot(data = CFP,
       aes(hour, pos_moving_avg, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1) +
  theme(legend.position="none")+
  ggtitle("Running Avg. Positive Sentiment Over Time") +
  labs(x="Hour",y="-Sentiment")
#####################################
CFPnoRe <- CFP[CFP$retweeted_status=='False',]
ggplot(data = CFPnoRe,
       aes(hour, compound_moving_avg, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1) +
  theme(legend.position="none")+
  ggtitle("Running Avg. Sentiment Over Time (Excluding Retweets)") +
  labs(x="Hour",y="Sentiment")
ggplot(data = CFPnoRe,
       aes(hour, pos_moving_avg, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1) +
  theme(legend.position="none") +
  ggtitle("Running Avg. Positive Sentiment Over Time (Excluding Retweets)") +
  labs(x="Hour",y="+Sentiment")
ggplot(data = CFPnoRe,
       aes(hour, neg_moving_avg, group=team, colour=team)) +
  stat_summary(fun.y = mean, 
               geom = "line", size=1) +
  theme(legend.position="none") +
  ggtitle("Running Avg. Negitive Sentiment Over Time (Excluding Retweets)") +
  labs(x="Hour",y="-Sentiment")
