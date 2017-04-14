#Spencer Swartz
#1.a
TableFarm_Combined <- c(103000,108000,101000,108000,89000,101000,90000,101000,91000,73000,93000,109000,114000,140000,140000,145000,76000,154000,112000,129000,155000,132000,147000,124000)

#1.b
month <- c(1:24)
plot(month,TableFarm_Combined)
abline(v= 13, col = 'red')
legend("topleft", legend=c("Start of New Marketing"),col = 'red', lwd=1)
#1.c
TableFarm_marketing <- c("OldMarketing","OldMarketing","OldMarketing","OldMarketing","OldMarketing","OldMarketing","OldMarketing","OldMarketing","OldMarketing","OldMarketing","OldMarketing","OldMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing","NewMarketing")
boxplot(TableFarm_Combined ~ TableFarm_marketing)
#1.g
TableFarm_current <- c(103000,108000,101000,108000,89000,101000,90000,101000,91000,73000,93000,109000)
TableFarm_future <- c(114000,140000,140000,145000,76000,154000,112000,129000,155000,132000,147000,124000)
t.test(TableFarm_future,TableFarm_current, paired = T, alternative = "two.sided")
#2.a
nobel <- read.csv("~/GitHub/ds710fall2016assignment6/nobel.csv",header = TRUE)
attach(nobel)
nobelwinners <- ne
chocolateconsumtion <- c
plot(chocolateconsumtion,nobelwinners)
#2.b
m1 = lm(nobelwinners~chocolateconsumtion)
summary(m1)
abline(lm(nobelwinners~chocolateconsumtion))
#2.e
plot(m1)
#3.a
encryptedA <- read.csv("~/GitHub/ds710fall2016assignment6/encryptedA.csv",header = TRUE)
attach(encryptedA)
encrypt_order <-  order(value)
barplot( value[encrypt_order], names.arg = key[encrypt_order] )
#3.b
Freq <- read.csv("~/GitHub/ds710fall2016assignment6/Letter Frequencies.csv",header = TRUE)
attach(Freq)
#3.c
par( mfrow= c(2,1))
freq_order <-  order(English)
barplot( value[encrypt_order], names.arg = key[encrypt_order], main = 'Encrypted A' )
barplot( English[freq_order], names.arg = Letter[freq_order], main = 'English Frequency'  )
#3.e
ordered_encrypta <- value[encrypt_order]
ordered_freq <- English[freq_order]
#3.f
ordered_encrypta <- c( rep(0, 6), ordered_encrypta )
#3.h
encrypta_mulitfreq <- ordered_freq*sum(ordered_encrypta)
#3.i
sortEnglish_combined = c( sum(encrypta_mulitfreq[1:4]), encrypta_mulitfreq[5:26] )
sortEnglish_combined_prob = c( sum(ordered_freq[1:4]), ordered_freq[5:26] )
sortencrypta_combined = c( sum(ordered_encrypta[1:4]), ordered_encrypta[5:26] )
#3.j
chisq.test(sortencrypta_combined, p=sortEnglish_combined_prob)
#3.l
#repeat for welsh in encryptA
freq_order_W <-  order(Welsh)
ordered_freq_W<- Welsh[freq_order_W]
welsh_multifreq <- ordered_freq_W*sum(ordered_encrypta)
sortWelsh_combined = c( sum(encrypta_mulitfreq[1:7]), encrypta_mulitfreq[8:26] )
sortWelsh_combined_prob = c( sum(ordered_freq_W[1:7]), ordered_freq_W[8:26] )
sortencrypta_combined_W = c( sum(ordered_encrypta[1:7]), ordered_encrypta[8:26] )
chisq.test(sortencrypta_combined_W, p=sortWelsh_combined_prob)
#repeat for english in encryptB
encryptedB <- read.csv("~/GitHub/ds710fall2016assignment6/encryptedB.csv",header = TRUE)
keyB <- encryptedB$key
valueB <- encryptedB$value
encryptB_order <-  order(valueB)
ordered_encryptB <- valueB[encryptB_order]
English_multifreqB <- ordered_freq*sum(ordered_encryptB)
sortEnglish_combinedB = c( sum(English_multifreqB[1:4]), English_multifreqB[5:26] )
sortEnglish_combinedB_prob = c( sum(ordered_freq[1:4]), ordered_freq[5:26] )
sortencryptB_combined = c( sum(ordered_encryptB[1:4]), ordered_encryptB[5:26] )
chisq.test(sortencryptB_combined, p=sortEnglish_combinedB_prob)
#repeat for welsh in encryptB
welsh_multifreqB <- ordered_freq_W*sum(ordered_encryptB)
sortWelsh_combinedB = c( sum(welsh_multifreqB[1:7]), welsh_multifreqB[8:26] )
sortWelsh_combinedB_prob = c( sum(ordered_freq_W[1:7]), ordered_freq_W[8:26] )
sortencryptB_combined_Welsh = c( sum(ordered_encryptB[1:7]), ordered_encryptB[8:26] )
chisq.test(sortencryptB_combined_Welsh, p=sortWelsh_combinedB_prob)
