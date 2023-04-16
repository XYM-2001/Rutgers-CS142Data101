sleep<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/SleepPrediction2.csv")
summary(sleep)
colnames(sleep)
nrow(sleep)
head(sleep)
tapply(sleep$ExerciseCal, sleep$Sleep, mean)
Prior<-nrow(sleep[sleep$Sleep =='Deep',])/nrow(sleep)
Prior
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds
TruePositive<-round(nrow(sleep[sleep$Sleep=='Deep'& sleep$LastSleep=='Shallow',])/nrow(sleep[sleep$Sleep=='Deep',]),2)
TruePositive
FalsePositive<-round(nrow(sleep[sleep$Sleep!='Deep'& sleep$LastSleep=='Shallow',])/nrow(sleep[sleep$Sleep!='Deep',]),2)
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior
mean(sleep[sleep$Sleep=='Deep',]$OnComputer)
mean(sleep[sleep$Sleep=='Shallow',]$OnComputer)
HypothesisTesting::permutation_test(sleep, "Sleep", "OnComputer",10000, "Deep", "Shallow")

vote<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/Voting1.csv")
colnames(vote)
nrow(vote)
summary(vote)
head(vote)
tapply(vote$Age, vote$Party, mean)
table(vote[vote$LeafBlowers=='None',]$Party)
table(vote[vote$Party=='KnowNothings',]$SpeedLimit)
max(vote[vote$Party=='KnowNothings',]$Age)
table(vote[vote$SpeedLimit =='NoLimits'&vote$CBD=='NoRestrictions'& vote$LeafBlowers=='None', ]$Party)
table(vote[vote$LiquerStores=='HardLiquerOnly', ]$Party)
Prior<-nrow(vote[vote$Party =='Royalists',])/nrow(vote)
Prior
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds
TruePositive<-round(nrow(vote[vote$Party=='Royalists'& vote$Age>65,])/nrow(vote[vote$Party=='Royalists',]),2)
TruePositive
FalsePositive<-round(nrow(vote[vote$Party!='Royalists'& vote$Age>65,])/nrow(vote[vote$Party!='Royalists',]),2)
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior
mean(vote[vote$Party=='Anarchists',]$Age)
mean(vote[vote$Party=='KnowNothings',]$Age) 
HypothesisTesting::permutation_test(vote, "Party", "Age",10000, "Anarchists", "KnowNothings")
table(vote[vote$Party=='Anarchists',]$SpeedLimit)
table(vote[vote$LeafBlowers=='ElectricOnly',]$Party)

party<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/Partyb.csv")
colnames(party)
nrow(party)
summary(party)
head(5)
table(party[party$WasNotThere=='Angela',]$Party)
Prior<-nrow(party[party$Party =='Fun',])/nrow(party)
Prior
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds
TruePositive<-round(nrow(party[party$Party=='Fun'& party$WasNotThere=='Vladimir',])/nrow(party[party$Party=='Fun',]),2)
TruePositive
FalsePositive<-round(nrow(party[party$Party!='Fun'& party$WasNotThere=='Vladimir',])/nrow(party[party$Party!='Fun',]),2)
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior
mean(party[party$Party=='Fun',]$CaloriesDanc)
mean(party[party$Party=='Boring',]$CaloriesDanc)
Permutation(party, "Party", "CaloriesDanc",10000, "Fun", "Boring")

hire <- read.csv('HireWide.csv')
head(hire)
unique(hire$Coding)
unique(hire$Impression)
unique(hire$Major)
unique(hire$Hired)
unique(hire$SPORTSPLAYED)
unique(hire$TIKTOK)
hire$Hired
hire[hire$Hired == 'Yes',]
Prior<-nrow(hire[hire$Coding == 'Weak',])/nrow(hire)
Prior
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds
TruePositive<-round(nrow(hire[hire$Coding == 'Weak' & hire$Hired == 'Yes',])/nrow(hire[hire$Coding == 'Weak',]),2)
TruePositive
TruePositive<-round(nrow(hire[hire$Coding != 'Weak' & hire$Hired == 'Yes',])/nrow(hire[hire$Coding != 'Weak',]),2)
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior
Permutation(party, "Party", "CaloriesDanc",10000, "Fun", "Boring")
hire[hire$Coding=='Weak']
PermutationTestSecond::Permutation(hire, 'Coding', 'Age', 10000, 'Excellent', 'Weak')
