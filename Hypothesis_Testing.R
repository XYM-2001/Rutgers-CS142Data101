movies <- read.csv('Movies2022F-4.csv')
head(movies)
barplot(tapply(movies[movies$content=='R',]$imdb_score, movies[movies$content=='R',]$Budget, mean), main='imdb score with budgets', xlab='budget', ylab='mean imdb score', col=c('red','yellow','blue','green'))
barplot(tapply(movies$imdb_score, movies$genre, mean), main='imdb score with genre', xlab='genre', ylab='mean imdb score')
barplot(tapply(movies$imdb_score, movies$Gross, mean), main='imdb score with gross', xlab='gross', ylab='mean imdb score')
library(BSDA)
z.test(x = movies[movies$content=='R' & movies$genre=='Action',]$imdb_score, y = movies[movies$content=='PG' & movies$genre=='Comedy',]$imdb_score, alternative = 'greater', mu=0, sigma.x=sd(movies[movies$content=='R' & movies$genre=='Action',]$imdb_score), sigma.y=sd(movies[movies$content=='PG' & movies$genre=='Comedy',]$imdb_score), conf.level = 0.95)

low_budget<-movies[movies$content=='R' & movies$Budget=='Low',]
high_budget<-movies[movies$content=='R' & movies$Budget=='High',]
z.test(x=low_budget$imdb_score,y=high_budget$imdb_score,
       alternative='greater', sigma.x=sd(low_budget$imdb_score), sigma.y=sd(high_budget$imdb_score))
table(movies$genre)
Drama<-movies[movies$genre=='Drama',]
Sci_Fi <- movies[movies$genre=='Sci-Fi',]

z.test(x=Drama$imdb_score,y=Sci_Fi$imdb_score,
       alternative='greater', sigma.x=sd(Drama$imdb_score), 
       sigma.y=sd(Sci_Fi$imdb_score))

High_gross<-movies[movies$Gross=='High',]
Low_gross<-movies[movies$Gross=='Low',]
z.test(x=Low_gross$imdb_score, y=High_gross$imdb_score,
       alternative='greater', sigma.x=sd(Low_gross$imdb_score),
       sigma.y = sd(High_gross$imdb_score))
