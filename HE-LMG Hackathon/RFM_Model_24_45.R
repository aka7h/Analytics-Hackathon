library(tidyverse)
library(readxl)
library(tidyr)
library(tidytext)
library(Rtsne)


customer_demo <- read_excel('Data/Customer_Demographics.xlsx')
customer_trans <- read_excel('Data/Customer_Transaction.xlsx')
store_master <- read_excel('Data/Store_Master.xlsx')
test_data <- read_excel('Data/Test_Set.xlsx')

sapply(customer_demo, function(x) sum(is.na(x)))

#Implement a RFM model
#Consisting of recency - frequency - monatary
#we add additional data where if user buy in other store or not
#if user buys in store in other city
#if user has more reward points
#if user bought bulk order or not

customer_demo <- customer_demo %>%
  replace_na(list(Birth_date = "01JAN1900:00:00:00")) %>%
  separate(Birth_date,c("Birth_date_date"),sep=":")%>%
  separate(First_txn_dt,c("First_txt_date","First_txt_hrs","First_txt_min","First_txt_sec"),sep=":")%>%
  separate(Last_accr_txn_dt,c("Last_accrt_date","Last_accr_hrs","Last_accr_min","Last_accr_sec"),sep=":")%>%
  separate(Last_rdm_txn_dt,c("Last_rdm_date","Last_rdm_hrs","Last_rdmt_min","Last_rdm_sec"),sep=":")%>%
  mutate(First_txt_date = as.Date(First_txt_date,format="%d%b%Y"),
         Last_accrt_date = as.Date(Last_accrt_date,format="%d%b%Y"),
         Last_rdm_date = as.Date(Last_rdm_date,format="%d%b%Y"),
         Birth_date_date = as.Date(Birth_date_date,format="%d%b%Y")
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

customer_demo %>% filter(is.na(Last_accrt_date)) %>% glimpse()
# these users are not useful, so lets add min date

customer_demo %>% filter(is.na(First_txt_date)) %>% glimpse()


#Recency
#Last Date Available  - 
Last_Transaction <- max(customer_demo$Last_accrt_date,na.rm = TRUE)+1

Cust_Recency <- customer_demo %>% 
  mutate(Recency = as.numeric(Last_Transaction - Last_accrt_date),Recency = ifelse(is.na(Recency),max(Recency,na.rm=TRUE),Recency)) %>% 
  dplyr::select(Customer_ID, Recency)

Cust_Recency %>% summary()

sapply(customer_trans,function(x) sum(is.na(x)))

customer_trans %>% group_by(Customer_ID) %>% count(sort=TRUE)
customer_trans %>% group_by(Customer_ID) %>% count(Store_Code,sort=TRUE)

Cust_Freq <- customer_trans %>% group_by(Customer_ID) %>% 
  mutate(Item_Count = replace(Item_Count,Transaction_Type=="Return",-Item_Count)) %>% 
  summarise(Item = sum(Item_Count,na.rm=TRUE))
sapply(Cust_Freq,function(x) sum(is.na(x)))

Cust_Freq %>% summary()


Cust_Mon <- customer_trans %>% group_by(Customer_ID) %>% summarise(Total = sum((Revenue)))

Cust_Mon %>% summary()


RFM_DATA <- left_join(Cust_Recency,Cust_Freq,by="Customer_ID") %>% left_join(.,Cust_Mon,by="Customer_ID")

RFM_DATA %>% summary()

RFM_DATA %>% ggplot(aes(Recency))+geom_histogram()
RFM_DATA %>% ggplot(aes(Item))+geom_histogram()
RFM_DATA %>% ggplot(aes(Total))+geom_histogram()
quantile(RFM_DATA$Recency)
quantile(RFM_DATA$Item)
quantile(RFM_DATA$Total)

RFM_DATA <- RFM_DATA %>% mutate(R = ntile(Recency,4),F=ntile(desc(Item),4),M=ntile(desc(Total),4),
                                RFM = as.numeric(paste(R,F,M,sep="")),
                                AggScore = rowMeans(.[,c('R','F','M')]),
                                Prediction = case_when(R >3 | F>3 | M>3~0,
                                                  TRUE~1)
                                ) 
RFM_DATA %>% summary()

RFM_DATA %>% ggplot(aes(Score))+geom_histogram()

## Since the store is active only in Abudhabi and sharja,
## Lets check if the user is buying stores cross country

#Cleaning Store Data
# store_master <- store_master %>% 
#   mutate(
#     n.Store_Format =  case_when(Mall_Name=="STANDALONE"~"Stand Alone Store",
#                                 TRUE~"Mall Store"),
#     Store_Format = NULL,
#     Region = gsub("MX - ","",Region),
#     #as.Date(First_txt_date,format="%d%b%Y")
#     Geo_Field = NULL,
#     Business = NULL,
#     Territory = NULL,
#     Mall_Name = NULL,
#     Store_Name = NULL,
#     n.Store_Age = as.numeric(Last_Transaction - as.Date(Store_Launch_Date)),
#     n.Rev_Age = Total_Revenue /  n.Store_Age
#   )
# 
# test_data <- test_data %>% mutate(Customer_ID=factor(Customer_ID))


rfm_submission <- merge(x=test_data,y=RFM_DATA[,c('Customer_ID','Prediction')],by="Customer_ID",all.x=TRUE)
rfm_submission$Store_Code <- as.numeric(rfm_submission$Store_Code)
rfm_submission$Prediction <- as.numeric(rfm_submission$Prediction)
rfm_submission <- rfm_submission %>% arrange(Customer_ID,Store_Code)
filename <- paste('RFM_4q_R3M3F3',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(rfm_submission,paste0(filename,'.csv',collapse = ''),row.names = FALSE)
