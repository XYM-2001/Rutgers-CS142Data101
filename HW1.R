airbnb <- read.csv('airbnb.csv')
movies <- read.csv('Movies2022F-4.csv')
head(airbnb)
nrow(airbnb[airbnb$floor>20,])

nrow(airbnb[airbnb$neighbourhood_group == 'Manhattan' & airbnb$floor > 15,])
max <- max(airbnb[airbnb$neighbourhood_group == 'Manhattan',]$price)
max
airbnb[airbnb$neighbourhood_group == 'Manhattan' & airbnb$price == max,]
boxplot(airbnb[airbnb$room_type=='Private room' & airbnb$neighbourhood_group=='Manhattan',]$price)
HypothesisTesting::z_test_from_agg(mean_a = 56.4, mean_b = 47.2, sd_a = 5.9, 
                                   sd_b = 6.4, n_a = 15072, n_b = 14921)

head(airbnb)
airbnb$temp <- airbnb$price-airbnb$floor