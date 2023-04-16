moody <- read.csv('moodyJanuary31b.csv')
head(moody)
nrow(moody)
table(moody$Texting)
table(moody$Grade)
table(moody$Asking)
table(moody$Dozing)

max(moody[moody$Grade=='B',]$Score)
mean(moody[moody$GPA<1.9,]$Score)

barplot(table(moody$Grade), main='Frequency of Grades', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))

boxplot(moody$Score~moody$Grade, main='Distribution of Scores by Grade', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))

barplot(table(moody$Asking), main='Frequency of Student Asking Questions', xlab='Questions', ylab='Frequency', col=c('red','yellow','blue'))

barplot(table(moody[moody$Asking=='Often',]$Grade), main='Frequency of Grades of students who often ask questions', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))
barplot(table(moody[moody$Asking=='Sometimes',]$Grade), main='Frequency of Grades of students who sometimes ask questions', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))
barplot(table(moody[moody$Asking=='Never',]$Grade), main='Frequency of Grades of students who never ask questions', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))

barplot(tapply(moody$Score,moody$Asking,median), main='Median Score for Questions', xlab='Questions', ylab='Median Score', col=c('red','yellow','blue'))

barplot(tapply(moody$GPA,moody$Asking,median), main='Median GPA for Questions', xlab='Questions', ylab='Median GPA', col=c('red','yellow','blue'))

barplot(table(moody$Texting), main='Frequency of Texting', xlab='Texting', ylab='Frequency', col=c('red','yellow','blue','green'))

barplot(table(moody[moody$Texting=='Always',]$Grade), main='Frequency of Grades of students who Always text', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))
barplot(table(moody[moody$Texting=='Often',]$Grade), main='Frequency of Grades of students who often text', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))
barplot(table(moody[moody$Texting=='Sometimes',]$Grade), main='Frequency of Grades of students who Sometimes text', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))
barplot(table(moody[moody$Texting=='Never',]$Grade), main='Frequency of Grades of students who Never text', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))

barplot(tapply(moody$Score,moody$Texting,median), main='Median Score for Texting', xlab='Texting', ylab='Median Score', col=c('red','yellow','blue','green'))

barplot(tapply(moody$GPA,moody$Texting,median), main='Median GPA for Texting', xlab='Texting', ylab='Median GPA', col=c('red','yellow','blue','green'))

barplot(table(moody$Dozing), main='Frequency of Dozing', xlab='Dozing', ylab='Frequency', col=c('red','yellow','blue','green'))

barplot(table(moody[moody$Dozing=='Always',]$Grade), main='Frequency of Grades of students who always doze', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))
barplot(table(moody[moody$Dozing=='Sometimes',]$Grade), main='Frequency of Grades of students who sometimes doze', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))
barplot(table(moody[moody$Dozing=='Never',]$Grade), main='Frequency of Grades of students who never doze', xlab='Grade', ylab='Frequency', col=c('red','green','yellow','blue'))

barplot(tapply(moody$Score,moody$Dozing,median), main='Median Score for Dozing', xlab='Dozing', ylab='Median Score', col=c('red','yellow','blue','green'))

barplot(tapply(moody$GPA,moody$Dozing,median), main='Median GPA for Dozing', xlab='Dozing', ylab='Median GPA', col=c('red','yellow','blue','green'))

boxplot(moody[moody$Asking=='Often',]$Score~moody[moody$Asking=='Often',]$Grade, main='Distribution of Scores by Grade who often ask questions', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))
boxplot(moody[moody$Asking=='Sometimes',]$Score~moody[moody$Asking=='Sometimes',]$Grade, main='Distribution of Scores by Grade who sometimes ask questions', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))
boxplot(moody[moody$Asking=='Never',]$Score~moody[moody$Asking=='Never',]$Grade, main='Distribution of Scores by Grade who never ask questions', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))

boxplot(moody[moody$Texting=='Always',]$Score~moody[moody$Texting=='Always',]$Grade, main='Distribution of Scores by Grade who Always text', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))
boxplot(moody[moody$Texting=='Often',]$Score~moody[moody$Texting=='Often',]$Grade, main='Distribution of Scores by Grade who often text', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))
boxplot(moody[moody$Texting=='Sometimes',]$Score~moody[moody$Texting=='Sometimes',]$Grade, main='Distribution of Scores by Grade who sometimes text', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))
boxplot(moody[moody$Texting=='Never',]$Score~moody[moody$Texting=='Never',]$Grade, main='Distribution of Scores by Grade who never text', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))

boxplot(moody[moody$Dozing=='Always',]$Score~moody[moody$Dozing=='Always',]$Grade, main='Distribution of Scores by Grade who always doze', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))
boxplot(moody[moody$Dozing=='Sometimes',]$Score~moody[moody$Dozing=='Sometimes',]$Grade, main='Distribution of Scores by Grade who sometimes doze', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))
boxplot(moody[moody$Dozing=='Never',]$Score~moody[moody$Dozing=='Never',]$Grade, main='Distribution of Scores by Grade who never doze', xlab='Grade', ylab='Score',col=c('red','green','yellow','blue'))

movies <- read.csv('Movies2022F-4.csv')
head(movies)

HypothesisTesting::z_test_from_agg(mean_a = 2.1, mean_b = 2.8, sd_a = 3, 
                                   sd_b = 1, n_a = 50, n_b = 100)
