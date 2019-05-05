library(tidyverse)
library(caret)
library(readxl)
library(VIM)
library(leaflet)
library(tidyr)
library(lubridate)
library(chron)
library(geosphere)
library(tidytext)
library(MASS)
library(animation)
library(Matrix)


customer_demo <- read_excel('Data/Customer_Demographics.xlsx')
customer_trans <- read_excel('Data/Customer_Transaction.xlsx')
store_master <- read_excel('Data/Store_Master.xlsx')

glimpse(customer_demo)

aggr(customer_demo)
aggr(store_master)
aggr(customer_trans)

#Preprocess the data



## customer_demographics

sapply(customer_demo, function(x) sum(is.na(x)))

customer_demo %>% group_by(Birth_date,Age) %>% count(sort=TRUE)
#add default datte to missing values

customer_demo %>% ggplot(aes(Territory))+geom_bar(stat="count") #remove territory

customer_demo %>% group_by(Nationality) %>% count() %>% ungroup()%>% top_n(30,n) %>%
  ggplot(aes(Nationality,n))+geom_col()+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=0.9)) 
#either group other countries above the threshold or remove this data

customer_demo %>% group_by(Job_Type) %>% count(sort=TRUE)

customer_demo %>% filter(!(Job_Type %in% c("Unspecified","Others",'Services'))) %>% ggplot(aes(Job_Type))+geom_bar(stat="count")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=0.9))
#we see that majority of the data is unspecified. here unspecified means missing data. Either remove this column,
#or combine them together


customer_demo %>% ggplot(aes(Marital_Status))+geom_bar(stat="count")

customer_demo %>% group_by(Marital_Status) %>% count(Marital_Status,sort=TRUE)
#check their relelvence with spending and cluster them if future

customer_demo %>% ggplot(aes(State))+geom_bar(stat="count")

customer_demo %>% ggplot(aes(Income_Range))+geom_bar(stat="count")

customer_demo %>% ggplot(aes(Language))+geom_bar(stat="count")

 customer_demo %>% ggplot(aes(Loyalty_Status))+geom_bar(stat="count")


customer_demo <- customer_demo %>%
  replace_na(list(Birth_date = "01JAN1900:00:00:00")) %>%
  separate(Birth_date,c("Birth_date_date"),sep=":")%>%
  separate(First_txn_dt,c("First_txt_date"),sep=":")%>%
  separate(Last_accr_txn_dt,c("Last_accrt_date"),sep=":")%>% #,"Last_accr_hrs","Last_accr_min","Last_accr_sec"
  separate(Last_rdm_txn_dt,c("Last_rdm_date"),sep=":")%>% #,"Last_rdm_hrs","Last_rdmt_min","Last_rdm_sec"
  mutate(First_txt_date = as.Date(First_txt_date,format="%d%b%Y"),
         Last_accrt_date = as.Date(Last_accrt_date,format="%d%b%Y"),
         Last_rdm_date = as.Date(Last_rdm_date,format="%d%b%Y"),
         Birth_date_date = as.Date(Birth_date_date,format="%d%b%Y"),
         Job_Type = ifelse(Job_Type=="PRESIDENT/SECY/TREASURER TO CLUB/ASSOC/SOCIETY","Society Leaders",Job_Type),
         Territory = NULL
         # First_txt_day = day(First_txt_date),
         # First_txt_month = month(First_txt_date),
         # First_txt_year = year(First_txt_date),
         # First_txt_week = week(First_txt_date),
         # First_txt_wend = wday(First_txt_date),
         # First_txt_qtr = quarter(First_txt_date),
         # Last_accrt_day = day(Last_accrt_date),
         # Last_accrt_month = month(Last_accrt_date),
         # Last_accrt_year = year(Last_accrt_date),
         # Last_accrt_week = week(Last_accrt_date),
         # Last_accrt_wend = wday(Last_accrt_date),
         # Last_accrt_qtr = quarter(Last_accrt_date),
         # Last_rdm_day = day(Last_rdm_date),
         # Last_rdm_month = month(Last_rdm_date),
         # Last_rdm_year = year(Last_rdm_date),
         # Last_rdm_week = week(Last_rdm_date),
         # Last_rdm_wend = wday(Last_rdm_date),
         # Last_rdm_qtr = quarter(Last_rdm_date),
  )


  
customer_demo <- customer_demo %>% 
  group_by(Nationality) %>% 
  mutate(N_Nationality = n(),n.Nationality = ifelse(N_Nationality < 800,'Others',Nationality),
         n.Nationality = case_when(n.Nationality=="Unspecified"~"UNDEFINED",
                                   TRUE~n.Nationality),
         Income_Range = ifelse(Income_Range=="Unknown","Unspecified",Income_Range)) %>%
  ungroup()



customer_demo %>% filter(is.na(First_txt_date)) %>% summary()


customer_demo %>% ggplot(aes(x=Age,y=n.Nationality))+
  geom_point(position = "jitter")

customer_demo %>% filter(Income_Range!="Unspecified") %>% ggplot(aes(x=Age,y=Income_Range))+
  geom_point(position = "jitter")


customer_demo %>% filter(Job_Type!="Unspecified") %>% ggplot(aes(x=Age,y=Job_Type))+
  geom_point(position = "jitter")

customer_demo %>% glimpse()


#impute last_accurate_date based on the last transaction or redeem date from the transaction table

######Store master
sapply(store_master, function(x) sum(is.na(x)))

store_master %>% View()

store_master %>% group_by(Store_Format,Mall_Name) %>% filter(is.na(Store_Format)) %>% count(sort=TRUE)

store_master %>% group_by(Geo_Field,Train_Test_Store) %>% filter(is.na(Geo_Field)) %>% count()



#remove geo_field for now

store_master <- store_master %>% 
  mutate(
    n.Store_Format =  case_when(Mall_Name=="STANDALONE"~"Stand Alone Store",
                                TRUE~"Mall Store"),
    Store_Format = NULL,
    Region = gsub("MX - ","",Region),
    #as.Date(First_txt_date,format="%d%b%Y")
    Geo_Field = NULL,
    Business = NULL,
    Territory = NULL,
    Mall_Name = NULL,
    Store_Name = NULL,
    n.Store_Age = as.numeric(as.Date("2018-03-01") - as.Date(Store_Launch_Date)),
    n.Rev_Age = Total_Revenue /  n.Store_Age,
    n.Rev_Sqft = Sales_Per_Day / Store_Size_Sq_Ft
  )

sapply(store_master, function(x) sum(is.na(x)))

store_master %>% glimpse()

store_master %>% ggplot(aes(n.Store_Age,n.Rev_Age,color=Train_Test_Store))+geom_point()

# store_cluster <- store_master %>% dplyr::select(ends_with("Code"),Sales_Per_Day,Store_Size_Sq_Ft) %>% 
#   mutate(Region_Code = factor(Region_Code))
# rownames(store_cluster) <- store_cluster$Store_Code
# 
# store_prepro= preProcess(store_cluster[,3:4],"range")
# store_prepro = predict(store_prepro,store_cluster[,3:4])
# 
# store_prepro2= preProcess(store_cluster[,3:4],"scale")
# store_prepro2 = predict(store_prepro2,store_cluster[,3:4])
# 
# 
# 
# Region_Code.1 <- dummyVars('~Region_Code',data=store_cluster)
# Region_Code.1 <- as.tibble(predict(Region_Code.1, newdata = store_cluster))
# 
# Region_Code.1
# store_cluster <- bind_cols(store_cluster,Region_Code.1,store_prepro,store_prepro2)
# store_cluster %>% glimpse()
# #range
# clu <- hclust(dist(store_cluster[,c(5:12)]),"complete")
# plot(clu)
# clu <- hclust(dist(store_cluster[,c(5:12)]),"average")
# plot(clu)
# clu <- hclust(dist(store_cluster[,c(5:12)]),"single")
# plot(clu)
# #scale
# clu <- hclust(dist(store_cluster[,c(5:10,13:14)]),"complete")
# plot(clu)
# clu <- hclust(dist(store_cluster[,c(5:10,13:14)]),"average")
# plot(clu)
# clu <- hclust(dist(store_cluster[,c(5:10,13:14)]),"single")
# plot(clu)

Onehot.1 <- dummyVars('~Region+n.Store_Format',data=store_master)
Onehot.1 <- as.tibble(predict(Onehot.1, newdata = store_master))

store_prepro= preProcess(store_master[,c(4,7,11:12)],"range")
store_prepro = predict(store_prepro,store_master[,c(4,7,11:12)])

store_prepro2= preProcess(store_master[,c(4,7,11:12)],"scale")
store_prepro2 = predict(store_prepro2,store_master[,c(4,7,11:12)])

Onehot.1
store_master <- bind_cols(store_master,Onehot.1)
store_master <- bind_cols(store_master,store_prepro,store_prepro2)
store_master %>% glimpse()
store_master %>% summary()

clu <- hclust(dist(store_master[,c(22:25)]),"complete")

plot(clu)


#Kmeans

kmeans.ani(store_master[,c(22:25)],3)

kmean_withinss <- function(k) {
  cluster <- kmeans(store_master[,c(14:25)], k)
  return (cluster$tot.withinss)
}#try with both 14:25 and 14:21,26:29
kmean_withinss(3)
# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)
elbow <-data.frame(2:max_k, wss)
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))


kmean_C <- kmeans(store_master[,c(14:25)],5,nstart=20)
kmean_C
Clusters.Lable <- as.factor(kmean_C$cluster)
store_master %>% ggplot(aes(Region, Sales_Per_Day, color = Clusters.Lable)) + 
  geom_point(position = "jitter",size=4)+ggtitle('cluster')+facet_wrap(~Train_Test_Store)

store_master %>% ggplot(aes(Region, Store_Size_Sq_Ft, color = Clusters.Lable)) + 
  geom_point(position = "jitter",size=4)+ggtitle('cluster')+facet_wrap(~Train_Test_Store)

store_master <- store_master %>% mutate(Kmeans_5 = Clusters.Lable)

## Customer Transaction

sapply(customer_trans, function(x) sum(is.na(x)))
customer_trans %>% glimpse()

#we already have region store code and also store format so we can merge store_master data
customer_trans <- merge(customer_trans,store_master[,c("Region","n.Store_Format","Store_Code","Kmeans_5","Sales_Per_Day")],by="Store_Code")
customer_trans <- merge(customer_trans,customer_demo[,c("Loyalty_Status","Points","State","n.Nationality","Customer_ID")],by="Customer_ID")

customer_trans %>% group_by(Return_Reason) %>% count(Return_Reason,sort = TRUE) %>% ungroup() %>% 
  ggplot(aes(Return_Reason,n))+geom_col()+theme(axis.text.x = element_text(angle=45,hjust=1,vjust=0.9))

customer_trans %>% ggplot(aes(Territory))+geom_bar(stat="count")
customer_trans %>% ggplot(aes(Business))+geom_bar(stat="count")
customer_trans %>% ggplot(aes(Region))+geom_bar(stat="count") #majority from abudhabi dubai sharjah

customer_trans <- customer_trans %>% filter(Year %in% c(2016,2017)) %>% mutate(
  City_Name =NULL,
  Store_Type = NULL,
  #Return_Reason = NULL,
  Business = NULL,
  Territory = NULL,
  Return_Reason = replace_na(Return_Reason,"No Return"),
  n.Return_Reason = fct_collapse(Return_Reason,Size.Problem=c("Size Problem","Ordered wrong size","Fit Problem"),
                                 Mfg.Defect = c("Mfg. Defect","Adhesive","Accessory/Trim","Zip Defect","Warranty","Squins/Button Detachment","Handling Defect","Motif/Print Defect","Material/Fabric Defect","Appearance Change After wash"),
                                 Others = c("Gift Exchange","Color Exchange")),
  Year_Week = as.Date(str_c(paste(Year,Week,sep="-"),"-1"),format="%Y-%W-%w"),#ISO WEEK
  Year_Week = case_when(is.na(Year_Week)&Year==2016&Week==0~as.Date("2016-01-04"),
                        is.na(Year_Week)&Year==2017&Week==53~as.Date("2017-12-25"),
                        TRUE~Year_Week),
  Year_Quarter = paste(year(Year_Week),quarter(Year_Week),sep="-"),
  Y_M = format(Year_Week,"%Y-%m"),
  Return = ifelse(Transaction_Type=="Return",1,0),
  Extra_Items_count = Units_Sold - Item_Count,
  Revenue_Disc = Revenue + Discount
)
customer_trans %>% glimpse()

customer_trans %>% summary()



# customer_trans %>% select(Store_Code,Year_Quarter,Transaction_Type,Revenue,Discount) %>% group_by(Store_Code,Year_Quarter,Transaction_Type) %>% 
#   summarise(TT = mean(Revenue+Discount)) %>% unite(Qua_Tr,Year_Quarter,Transaction_Type) %>% spread(Qua_Tr,TT) %>%
#   rename_at(vars(-Store_Code),funs(paste0("n.AVG_",.)))

## Quaterly User Purchase Return
customer_trans %>% group_by(Customer_ID,Year_Quarter,Transaction_Type) %>% count(sort=TRUE) %>% 
  unite(count.Q_T,Year_Quarter,Transaction_Type) %>%
  spread(count.Q_T,n) %>% replace(.,is.na(.),0)



customer_trans %>% group_by(Customer_ID,Year_Quarter,Transaction_Type) %>% count(sort=TRUE) %>% 
  unite(count.Q_T,Year_Quarter,Transaction_Type) %>%
  spread(count.Q_T,n) %>% replace(.,is.na(.),0)



#Calculate Total and Average Invoice, Discounts, Revenue, Reason to Return, No of Transaction, 
# Purchases, Item COunt Sold pur pruchase, Item Count returned. 

#Total Purchase by City
store_purchase <- customer_trans %>% group_by(Region,Year,Transaction_Type) %>% summarise(n.Qua_Rev=sum(Revenue+Discount)) %>% 
  unite(c.Year_Tr,Year,Transaction_Type) %>% spread(c.Year_Tr,n.Qua_Rev)

customer_trans %>% group_by(n.Return_Reason) %>% summarise(Total_Return = sum(Revenue+Discount)) %>% 
  arrange(Total_Return)

#Average Return Rate Per City
store_return_avg <- customer_trans %>% filter(Transaction_Type=="Return") %>% group_by(Region,Year_Quarter,n.Return_Reason) %>% 
  summarise(Total_Return = sum(Item_Count)) %>% mutate(Percent = prop.table(Total_Return)) %>% 
  unite(YQ_RR,Year_Quarter,n.Return_Reason) %>% spread(YQ_RR,Percent) %>% replace(.,is.na(.),0)

#Return Reason Percentage in Each City
store_return_city_p <- customer_trans %>% filter(Transaction_Type=="Return") %>% group_by(Region,n.Return_Reason) %>% 
  summarise(Total_Return = sum(Item_Count)) %>% mutate(Percent = prop.table(Total_Return),Total_Return=NULL) %>% 
  spread(n.Return_Reason,Percent) %>% replace(.,is.na(.),0)

total_rev_itemscustomer_trans %>% group_by(Customer_ID,Store_Code) %>% 
  summarise(Rev = sum(Revenue + Discount),Extra_Items = sum(Units_Sold - Item_Count))

customer_trans %>% group_by(State,Region) %>% filter(Region %in% c("Abu Dhabi","Sharjah","Dubai")) %>% 
  count(sort = TRUE)
