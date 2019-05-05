library(tidyverse)
library(readxl)
library(tidyr)
library(tidytext)
library(MASS)
library(Matrix)

customer_demo <- read_excel('Data/Customer_Demographics.xlsx')
customer_trans <- read_excel('Data/Customer_Transaction.xlsx')
store_master <- read_excel('Data/Store_Master.xlsx')

test_data <- read_excel('Data/Test_Set.xlsx')

glimpse(customer_demo)

sapply(customer_demo, function(x) sum(is.na(x)))


#Lets Implement SVD based Classification

#For SVD Based CLassification we need Cust_ID, Store_Code and 
#Check if the customers have made a purchase in store or not
set.seed(3993)
cust_store_1 <- customer_trans %>% group_by(Customer_ID,Store_Code) %>% 
  summarise(P=sum(Revenue)) #%>% mutate(P = if_else(P > 0,1,0))

cust_store_2 <- test_data %>% mutate(P=0)

cust_store_ <- bind_rows(cust_store_1,cust_store_2)


#Now lets convert the data to sparse matrix
SVD_data <- cust_store_ %>% cast_dfm(Customer_ID,Store_Code,P)
SVD_data <- as.data.frame(as.matrix(SVD_data))



#Now Lets Implement Single Value Decomposition - 
a.SVD <- svd(SVD_data)


#Lets Plot the Singular Value
singular_sq <- sum(a.SVD$d^2)
perc_vec <- NULL
for (i in 1:length(a.SVD$d)) {
  perc_vec[i] <- sum(a.SVD$d[1:i]^2) / singular_sq
}

#Checking the Dimensions
plot(perc_vec, pch=20, col = "blue", cex = 1.5, xlab='Singular Value', ylab='% of Sum of Squares of Singular Values', main = "Choosing k for Dimensionality Reduction")
lines(x = c(0,100), y = c(.90, .90))



#Diagonal Matrix
sig <- Diagonal(x = a.SVD$d)
u <- a.SVD$u
v <- t(a.SVD$v)


predicted <- u %*% sig %*% v
dim(predicted)
predicted <- as.matrix(predicted)
predicted <- as.tibble(predicted)
colnames(predicted) <- as.character(colnames(SVD_data))

final_pred <- predicted[,33:36] %>% mutate(Customer_ID = as.numeric(rownames(SVD_data)))
final_pred <- final_pred %>% mutate(`60298` = if_else(`60298`>0,1,0),
                      `60299`=if_else(`60299`>0,1,0),
                      `60300`=if_else(`60300`>0,1,0),
                      `60301`=if_else(`60301`>0,1,0))
final_pred <- final_pred %>% gather(Store_Code,Prediction,`60298`,`60299`,`60300`,`60301`)
final_pred$Store_Code <- as.numeric(final_pred$Store_Code)
final_pred$Prediction <- as.numeric(final_pred$Prediction)
final_pred <- final_pred %>% arrange(Customer_ID,Store_Code)


filename <- paste('SVD_great_zero_3',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(final_pred,paste0(filename,'.csv',collapse = ''),row.names = FALSE)
