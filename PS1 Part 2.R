library(tidyverse)
library(tidyr)
library(FNN)

# Deleting unnecessary columns from the dataset
keep = c("trim", "mileage", "price")
sclass = sclass[keep]

# Focus on 2 trim levels: 350 and 65 AMG
sclass350 = subset(sclass, trim == '350')
dim(sclass350)

sclass65AMG = subset(sclass, trim == '65 AMG')
summary(sclass65AMG)

# Look at price vs mileage for each trim level
plot(price ~ mileage, data = sclass350)
plot(price ~ mileage, data = sclass65AMG)


#####
# Splitting into training and testing data sets
#####

# Make a train and test split for each class, 350 and 65AMG
N_350 = nrow(sclass350)
N_train_350 = floor(0.8*N_350)
N_test_350 = N_350 - N_train_350

N_65 = nrow(sclass65AMG)
N_train_65 = floor(0.8*N_65)
N_test_65 = N_65 - N_train_65

# Randomly sample set of data points to include in the data set
train_ind_350 = sample.int(N_350, N_train_350, replace = FALSE)
train_ind_65AMG = sample.int(N_65, N_train_65, replace = FALSE)

# Define training and testing datasets
D_train_350 = sclass350[train_ind_350,]
D_test_350 = sclass350[-train_ind_350,]

D_train_65AMG = sclass65AMG[train_ind_65AMG,]
D_test_65AMG = sclass65AMG[-train_ind_65AMG,]

# Reordering the rows by the mileage variable
D_test_350 = arrange(D_test_350, mileage)
head(D_test_350)

D_test_65AMG = arrange(D_test_65AMG, mileage)
head(D_test_65AMG)

# Splitting each into features (x, mileage) and outcomes (y, price)
X_train_350 = select(D_train_350, mileage)
Y_train_350 = select(D_train_350, price)
X_test_350 = select(D_test_350, mileage)
Y_test_350 = select(D_test_350, price)

X_train_65AMG = select(D_train_65AMG, mileage)
Y_train_65AMG = select(D_train_65AMG, price)
X_test_65AMG = select(D_test_65AMG, mileage)
Y_test_65AMG = select(D_test_65AMG, price)


#####
# Testing/fitting the KNN models for sclass350
#####


#KNN 3
knn3_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 3)
names(knn3_350)

#KNN 5
knn5_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 5)
names(knn5_350)

#KNN 10
knn10_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 10)
names(knn10_350)

#KNN 15
knn15_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 15)
names(knn15_350)

#KNN 20
knn20_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 20)
names(knn20_350)

#KNN 25
knn25_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 25)
names(knn25_350)

#KNN 50
knn50_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 50)
names(knn50_350)

#KNN 100
knn100_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 100)
names(knn100_350)

#KNN 150
knn150_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 150)
names(knn150_350)

#KNN 250
knn250_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 250)
names(knn250_350)


######
# Compare the models with RMSE_out for sclass 350
######


rmse = function(y, ypred) {
  sqrt(mean(data.matrix((y-ypred)^2)))
}

ypred_knn3_350 = knn3_350$pred
ypred_knn5_350 = knn5_350$pred
ypred_knn10_350 = knn10_350$pred
ypred_knn15_350 = knn15_350$pred
ypred_knn20_350 = knn20_350$pred
ypred_knn25_350 = knn25_350$pred
ypred_knn50_350 = knn50_350$pred
ypred_knn100_350 = knn100_350$pred
ypred_knn150_350 = knn150_350$pred
ypred_knn250_350 = knn250_350$pred

rmse(Y_test_350, ypred_knn3_350)
rmse(Y_test_350, ypred_knn5_350)
rmse(Y_test_350, ypred_knn10_350)
rmse(Y_test_350, ypred_knn15_350)
rmse(Y_test_350, ypred_knn20_350)
rmse(Y_test_350, ypred_knn25_350)
rmse(Y_test_350, ypred_knn50_350)
rmse(Y_test_350, ypred_knn100_350)
rmse(Y_test_350, ypred_knn150_350)
rmse(Y_test_350, ypred_knn250_350)

#####
# Plot the fit
#####

D_test_350$ypred_knn3_350 = ypred_knn3_350
D_test_350$ypred_knn5_350 = ypred_knn5_350
D_test_350$ypred_knn10_350 = ypred_knn10_350
D_test_350$ypred_knn15_350 = ypred_knn15_350
D_test_350$ypred_knn20_350 = ypred_knn20_350
D_test_350$ypred_knn25_350 = ypred_knn25_350
D_test_350$ypred_knn50_350 = ypred_knn50_350
D_test_350$ypred_knn100_350 = ypred_knn100_350
D_test_350$ypred_knn150_350 = ypred_knn150_350
D_test_350$ypred_knn250_350 = ypred_knn250_350

p_test = ggplot(data = D_test_350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='blue') + 
  theme_bw(base_size=18)  +
  ylim(6000, 100000)
p_test

p_test + geom_point(aes(x = mileage, y = ypred_knn3_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn5_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn10_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn15_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn20_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn25_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn50_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn100_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn150_350), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn250_350), color='red')

#####
# Creating a dataframe for the KNN amounts and their respective RMSE 
k = c(3, 5, 10, 15, 20, 25, 50, 100, 150, 250)
rmse_results = c(11443.88, 10514, 9968.258, 9663.655, 10010.36, 
                 10226.5, 10291.22, 11553.27, 14237.3, 18598.42)
k_rmse_350 = data.frame(k, rmse_results)

ggplot(data = K_rmse_results_350) +
  geom_line(mapping = aes(x = k1, y = rmse_results1), 
            color = 'blue', size = 1.25) +
  xlim(300, 1) +
  theme_bw(base_size=18)
       
       