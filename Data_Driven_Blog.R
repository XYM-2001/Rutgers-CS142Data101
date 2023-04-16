data <- read.csv('2019.csv')
summary(data)
hist(tapply(data$Score,data$Country.or.region,mean), main = '')
hist(data$Score, main = 'Distribution of Happiness Scores', xlab = 'Score')
top <- data[data$Healthy.life.expectancy > quantile(data$Healthy.life.expectancy,
                                                       prob = 1-10/100),]
bot <- data[data$Healthy.life.expectancy < quantile(data$Healthy.life.expectancy,
                                                       prob = 1-90/100),]
barplot(tapply(top$Score, top$Country.or.region, sum), 
        main = 'happiness scores for countries with top 10% healthy life expectancy', 
        xlab = 'Country or Region', ylab = 'Happiness Score')
barplot(tapply(bot$Score, bot$Country.or.region, sum), 
        main = 'happiness scores for countries with bottom 10% healthy life expectancy', 
        xlab = 'Country or Region', ylab = 'Happiness Score')
boxplot(Score ~ Healthy.life.expectancy, top, 
        main = 'boxplot for Happiness Score vs Healthy life expectancy for top 10% life')
boxplot(Score ~ Healthy.life.expectancy, bot, 
        main = 'boxplot for Happiness Score vs Healthy life expectancy for bot 10% life')
top$Group <- c('top')
bot$Group <- c('bot')
all <- rbind(top, bot)
all
PermutationTestSecond::Permutation(all, 'Group', 'Score', 10000, 'bot', 'top')

data$GDP.per.capita
data$Social.support

top_gdp_social <- data[data$GDP.per.capita > 
                         quantile(data$GDP.per.capita,
                         prob = 1-10/100) 
                       & data$Social.support > 
                         quantile(data$Social.support,
                         prob = 1-10/100),]
bot_gdp_social <- data[data$GDP.per.capita < 
                         quantile(data$GDP.per.capita,
                                  prob = 1-90/100) 
                       & data$Social.support <
                         quantile(data$Social.support,
                                  prob = 1-90/100),]
barplot(tapply(top_gdp_social$Score, top_gdp_social$Country.or.region, sum), 
        main = 'happiness scores for countries with top 10% gdp and social support', 
        xlab = 'Country or Region', ylab = 'Happiness Score')
barplot(tapply(bot_gdp_social$Score, bot_gdp_social$Country.or.region, sum), 
        main = 'happiness scores for countries with bottom 10% gdp and social support', 
        xlab = 'Country or Region', ylab = 'Happiness Score')
boxplot(Score ~ GDP.per.capita+Social.support, top_gdp_social, 
        main = 'boxplot for Happiness Score vs gdp+social support for top 10% gdp and social support')
  boxplot(Score ~ GDP.per.capita+Social.support, bot_gdp_social, 
        main = 'boxplot for Happiness Score vs gdp+social support for bot 10% gdp and social support')
top_gdp_social$Group <- c('top')
bot_gdp_social$Group <- c('bot')
all <- rbind(top_gdp_social, bot_gdp_social)
all
PermutationTestSecond::Permutation(all, 'Group', 'Score', 10000, 'bot', 'top')
