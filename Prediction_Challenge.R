Hire <- read.csv('HireTrainApr10.csv')
head(Hire)
unique(Hire$Coding)
unique(Hire$Impression)
unique(Hire$Major)
unique(Hire$College)
unique(Hire$Hired)
nrow(Hire)
table(Hire$Hired)
table(Hire$Hired, Hire$Coding)
table(Hire$Hired, Hire$Impression)
table(Hire$Hired, Hire$College)
table(Hire$Hired, Hire$Major)
chisq.test(table(Hire$Hired, Hire$College))
chisq.test(table (Hire$Hired, Hire$Major))
chisq.test(table(Hire$Hired, Hire$Coding))
chisq.test(table(Hire$Hired, Hire$Impression))
table(Hire$Hired,Hire$College)
Yes <- Hire[Hire$Hired=='Yes',]
table(Yes$Coding)
table(Yes$Impression)
table(Yes$Major)
table(Yes$College)
table(Hire[Hire$College=='Peters',]$Impression,Hire[Hire$College=='Peters',]$Hired)
table(Hire[Hire$College=='Redbrick',]$Impression,Hire[Hire$College=='Redbrick',]$Hired)
table(Hire[Hire$College=='BYU',]$Coding,Hire[Hire$College=='BYU',]$Hired)

stat1 <- Hire[Hire$Coding=='Excellent' & Hire$Impression=='Outgoing',]
barplot(table(stat1$Hired))
stat2 <- Hire[Hire$Major=='CS'&Hire$College=='Redbrick',]
barplot(table(stat3$Hired))
stat3 <- Hire[Hire$Coding=='Excellent' & Hire$Impression=='Confident',]
barplot(table(stat3$Hired))
stat4 <- Hire[Hire$Coding=='Excellent'&Hire$Impression=='Shy',]
barplot(table(stat4$Hired))
stat5 <- Hire[Hire$Coding=='OK'&(Hire$Impression=='Outgoing'|Hire$Impression=='Confident'|Hire$Impression=='Shy'),]
barplot(table(stat5$Hired))
stat6 <- Hire[Hire$College=='Redbrick'&(Hire$Coding=='Excellent'|Hire$Coding=='OK'),]
barplot(table(stat6$Hired))
stat7 <- Hire[Hire$College=='Peters' & (Hire$Impression=='Outgoing'|Hire$Impression=='Nerdy'),]
barplot(table(stat7$Hired))
stat8 <- Hire[Hire$College=='Redbrick'& Hire$Impression=='Nerdy',]
barplot(table(stat8$Hired))
stat9 <- Hire[Hire$College=='Peters'&(Hire$Coding=='Excellent'|Hire$Coding=='OK'),]
barplot(table(stat9$Hired))
stat10 <- Hire[Hire$College=='BYU'&(Hire$Coding=='Excellent'|Hire$Coding=='OK'),]
barplot(table(stat10$Hired))

v<-sample(1:nrow(Hire))
v[1:5]
trainScrambled<-Hire[v, ]
trainSample<-trainScrambled[nrow(trainScrambled)-10:nrow(trainScrambled), ]
myprediction<-trainSample
decision <- rep('No',nrow(myprediction))
decision[myprediction$Coding=='Excellent'&myprediction$Impression=='Outgoing'] <- 'Yes'
decision[myprediction$Major=='CS'&myprediction$College=='Redbrick'] <- 'Yes'
decision[myprediction$Coding=='Excellent' & myprediction$Impression=='Confident']<- 'Yes'
decision[myprediction$Coding=='Excellent'&myprediction$Impression=='Shy']<-'Yes'
decision[myprediction$Coding=='OK'&(myprediction$Impression=='Shy'|myprediction$Impression=='Confident'|myprediction$Impression=='Outgoing')] <- 'Yes'
decision[myprediction$College=='Redbrick'&(myprediction$Coding=='Excellent'|myprediction$Coding=='OK')]<-'Yes'
decision[myprediction$College=='Peters'&(myprediction$Impression=='Nerdy'|myprediction$Impression=='Outgoing')]<-'Yes'
decision[myprediction$College=='Redbrick'&myprediction$Impression=='Nerdy']<-'Yes'
decision[myprediction$College=='Peters'&(myprediction$Coding=='Excellent'|myprediction$Coding=='OK')]<-'Yes'
decision[myprediction$College=='BYU'&(myprediction$Coding=='Excellent'|myprediction$Coding=='OK')]<-'Yes'
myprediction$Hired <-decision
correct <- sum(myprediction$Hired==trainSample$Hired)
accuracy <- correct/nrow(trainSample)
accuracy
