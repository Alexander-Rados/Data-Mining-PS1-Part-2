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

# Creating rmse function
rmse = function(y, ypred) {
  sqrt(mean(data.matrix((y-ypred)^2)))
}

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
# Testing/fitting the KNN models for sclass65AMG
#####

#KNN 3
knn3_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 3)

#KNN 5
knn5_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 5)

#KNN 10
knn10_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 10)

#KNN 15
knn15_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 15)

#KNN 20
knn20_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 20)

#KNN 25
knn25_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 25)

#KNN 50
knn50_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 50)

#KNN 100
knn100_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 100)

#KNN 150
knn150_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 150)

#KNN 233
knn233_65AMG = knn.reg(train = X_train_65AMG, test = X_test_65AMG, y = Y_train_65AMG, k = 233)

######
# Compare the models with RMSE_out for sclass 65AMG
#####

#KNN 3
ypred_knn3_65AMG = knn3_65AMG$pred
rmse(Y_test_65AMG, ypred_knn3_65AMG)

#KNN 5
ypred_knn5_65AMG = knn5_65AMG$pred
rmse(Y_test_65AMG, ypred_knn5_65AMG)

#KNN 10
ypred_knn10_65AMG = knn10_65AMG$pred
rmse(Y_test_65AMG, ypred_knn10_65AMG)

#KNN 15
ypred_knn15_65AMG = knn15_65AMG$pred
rmse(Y_test_65AMG, ypred_knn15_65AMG)

#KNN 20
ypred_knn20_65AMG = knn20_65AMG$pred
rmse(Y_test_65AMG, ypred_knn20_65AMG)

#KNN 25
ypred_knn25_65AMG = knn25_65AMG$pred
rmse(Y_test_65AMG, ypred_knn25_65AMG)

#KNN 50
ypred_knn50_65AMG = knn50_65AMG$pred
rmse(Y_test_65AMG, ypred_knn50_65AMG)

#KNN 100
ypred_knn100_65AMG = knn100_65AMG$pred
rmse(Y_test_65AMG, ypred_knn100_65AMG)

#KNN 150
ypred_knn150_65AMG = knn150_65AMG$pred
rmse(Y_test_65AMG, ypred_knn150_65AMG)

#KNN 233
ypred_knn233_65AMG = knn233_65AMG$pred
rmse(Y_test_65AMG, ypred_knn233_65AMG)

#####
# Plot the fit
#####

# Attaching predictions to the dataset
D_test_65AMG$ypred_knn3_65AMG = ypred_knn3_65AMG
D_test_65AMG$ypred_knn5_65AMG = ypred_knn5_65AMG
D_test_65AMG$ypred_knn10_65AMG = ypred_knn10_65AMG
D_test_65AMG$ypred_knn15_65AMG = ypred_knn15_65AMG
D_test_65AMG$ypred_knn20_65AMG = ypred_knn20_65AMG
D_test_65AMG$ypred_knn25_65AMG = ypred_knn25_65AMG
D_test_65AMG$ypred_knn50_65AMG = ypred_knn50_65AMG
D_test_65AMG$ypred_knn100_65AMG = ypred_knn100_65AMG
D_test_65AMG$ypred_knn150_65AMG = ypred_knn150_65AMG
D_test_65AMG$ypred_knn233_65AMG = ypred_knn233_65AMG

# Plotting the predictions on the dataset
p_test = ggplot(data = D_test_65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='blue') + 
  theme_bw(base_size=18)  +
  ylim(25000, 230000)
p_test

p_test + geom_path(aes(x = mileage, y = ypred_knn3_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn5_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn10_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn15_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn20_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn25_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn50_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn100_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn150_65AMG), color='red')
p_test + geom_path(aes(x = mileage, y = ypred_knn233_65AMG), color='red')

#####
# Creating a dataframe for the KNN amounts and their respective RMSE 
#####

k_65AMG = c(3, 5, 10, 15, 20, 25, 50, 100, 150, 233)
rmse_65AMG_results = c(26450.24, 25463.92, 21912.28, 22035.38, 22008.06, 
                     22927.64, 28431.34, 39614.56, 54354.64, 80257.82)
k_rmse_65AMG = data.frame(k_65AMG, rmse_65AMG_results)

ggplot(data = k_rmse_65AMG) +
  geom_line(mapping = aes(x = k_65AMG, y = rmse_65AMG_results), 
            color = 'blue', size = 1.25) +
  xlim(250, 1) +
  theme_bw(base_size=18)

#####
# Testing/fitting the KNN models for sclass350
#####

#KNN 3
knn3_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 3)

#KNN 5
knn5_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 5)

#KNN 10
knn10_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 10)

#KNN 15
knn15_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 15)

#KNN 20
knn20_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 20)

#KNN 25
knn25_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 25)

#KNN 50
knn50_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 50)

#KNN 100
knn100_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 100)

#KNN 150
knn150_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 150)

#KNN 250
knn250_350 = knn.reg(train = X_train_350, test = X_test_350, y = Y_train_350, k = 250)


######
# Compare the models with RMSE_out for sclass 350
######

#KNN 3
ypred_knn3_350 = knn3_350$pred
rmse(Y_test_350, ypred_knn3_350)

#KNN 5
ypred_knn5_350 = knn5_350$pred
rmse(Y_test_350, ypred_knn5_350)

#KNN 10
ypred_knn10_350 = knn10_350$pred
rmse(Y_test_350, ypred_knn10_350)

#KNN 15
ypred_knn15_350 = knn15_350$pred
rmse(Y_test_350, ypred_knn15_350)

#KNN 20
ypred_knn20_350 = knn20_350$pred
rmse(Y_test_350, ypred_knn20_350)

#KNN 25
ypred_knn25_350 = knn25_350$pred
rmse(Y_test_350, ypred_knn25_350)

#KNN 50
ypred_knn50_350 = knn50_350$pred
rmse(Y_test_350, ypred_knn50_350)

#KNN 100
ypred_knn100_350 = knn100_350$pred
rmse(Y_test_350, ypred_knn100_350)

#KNN 150
ypred_knn150_350 = knn150_350$pred
rmse(Y_test_350, ypred_knn150_350)

#KNN 250
ypred_knn250_350 = knn250_350$pred
rmse(Y_test_350, ypred_knn250_350)

#####
# Plot the fit
#####

# Attaching predictions to the testing dataframe
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

# Plotting the predictions on the dataset
p_test = ggplot(data = D_test_350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='blue') + 
  theme_bw(base_size=18)  +
  ylim(6000, 100000)
p_test

p_test + geom_path(aes(x = mileage, y = ypred_knn3_350), color='red')
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
#####


k_350 = c(3, 5, 10, 15, 20, 25, 50, 100, 150, 250)
rmse_350_results = c(11443.88, 10514, 9968.258, 9663.655, 10010.36, 
                 10226.5, 10291.22, 11553.27, 14237.3, 18598.42)
k_rmse_350 = data.frame(k_350, rmse_350_results)

ggplot(data = k_rmse_350) +
  geom_line(mapping = aes(x = k_350, y = rmse_350_results), 
            color = 'blue', size = 1.25) +
  xlim(300, 1) +
  theme_bw(base_size=18)       
