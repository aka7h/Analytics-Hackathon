library(tidyr)
library(tidyverse)
library(caret)
library(h2o)
library(lubridate)
library(keras)
library(chron)
library(magrittr)


train <- read_csv('train.csv')
test <- read_csv('test.csv')
sample <- read_csv('sample_submission.csv')

summary(train)

train$is_train <- 1
test$is_train <- 0
test$amount_spent_per_room_night_scaled <- NA
full <- bind_rows(train,test)

sapply(train,function(x)sum(is.na(x)))
sapply(test,function(x)sum(is.na(x)))

full <- full %>% mutate(booking_date=as.Date(booking_date,"%d/%m/%y"),
                        checkin_date=as.Date(checkin_date,"%d/%m/%y"),
                        checkout_date=as.Date(checkout_date,"%d/%m/%y"),
                        advance_booking = as.numeric(checkin_date-booking_date),
                        resort_id=str_trunc(resort_id,10),
                        memberid=str_trunc(memberid,10))

#advance bookings
full %>% group_by(memberid) %>% mutate(d=n(),r=sum(amount_spent_per_room_night_scaled)) %>% 
  ungroup() %>% filter(advance_booking<0)  %>% View()

#total of 14 members. here we cannot remove these users id because, 
#there are 2 users in test with negative advance booking date. 
#take either same day booking, 15 days mode and 33 days median
full %>% group_by(advance_booking) %>% count(sort=TRUE)
#since 0 is 3rd highest, lets take same day booking

full <- full %>% mutate(booking_date=case_when(advance_booking<0~checkin_date,
                                               TRUE~booking_date),
                        advance_booking=case_when(advance_booking<0~0,
                                                  TRUE~advance_booking))


full <- full %>% mutate(booking_month=month(booking_date),booking_day=day(booking_date),
                        checkin_month=month(checkin_date),checkin_day=day(checkin_date),
                        booking_d=weekdays(booking_date),checkin_d=weekdays(checkin_date),
                        checkout_d=weekdays(checkout_date),checkin_quarter=quarter(checkin_date),
                        checkin_weekend=is.weekend(checkin_date))
full %>% group_by(total_pax) %>% count(sort=TRUE)

full %>% filter(total_pax>11) %>% View()

full <- full %>% mutate(pax=case_when(total_pax>4~'others',
                                      total_pax<2~'others',
                                      total_pax==2~'couple',
                                      TRUE~'family'))
full %>% group_by(pax) %>% count(sort=TRUE)

full %>% ggplot(aes(factor(roomnights),advance_booking))+geom_boxplot()

full %>% group_by(roomnights) %>% count(sort=TRUE)
full %>% filter(roomnights<1) %>% View()
#cleaning the room night stay
full <- full %>% mutate(roomnights=case_when(roomnights<1~as.integer(checkout_date-checkin_date),
                                             TRUE~roomnights))

full <- full %>% mutate(room_stay=case_when(roomnights<=2~'VShort',
                                            roomnights>2&roomnights<=4~'Normal',
                                            roomnights>4&roomnights<8~'Week',
                                            roomnights>7~'Long'))

full %>% group_by(room_stay) %>% count(sort=TRUE)


full %>% ggplot(aes(season_holidayed_code,fill=factor(checkin_quarter)))+geom_bar(stat='count')

top_season_quaterly <- full %>% filter(!is.na(season_holidayed_code)) %>% group_by(checkin_quarter,season_holidayed_code) %>% 
  tally() %>%  top_n(1) 

full <- full %>% mutate(season_holidayed_code=case_when(
                              is.na(season_holidayed_code)&checkin_quarter==2~as.integer(2),
                              is.na(season_holidayed_code)&checkin_quarter==3~as.integer(4),
                              is.na(season_holidayed_code)&checkin_quarter==4~as.integer(2),
                              TRUE~season_holidayed_code))

full <- full %>% mutate(adv_booking=case_when(advance_booking<=2~'Sudden',
                                              advance_booking>2&advance_booking<=15~'Fortnite',
                                              advance_booking>15&advance_booking<=31~'Amonth',
                                              advance_booking>31~"Long Booking"))

full %>% filter(!is.na(amount_spent_per_room_night_scaled))%>% 
  group_by(adv_booking) %>% summarise(mean(amount_spent_per_room_night_scaled)) %>% View()

full %>% group_by(state_code_residence,) %>% count(sort=TRUE)
full %>% ggplot(aes(factor(state_code_residence),fill=factor(cluster_code)))+
  geom_bar(stat='count')

full <- full %>% mutate(state_code_residence=replace_na(state_code_residence,8))

#now lets get the count

full <- full %>% 
  group_by(memberid) %>% mutate(max_total_pax=max(total_pax),min_total_pax=min(total_pax),
                                max_roomnight=max(roomnights),min_roomnight=min(roomnights),
                                total_visit=n(),max_adult=max(numberofadults),
                                min_adult=min(numberofadults),max_children=max(numberofchildren),
                                               min_children=min(numberofchildren)) %>% ungroup()

full <- full %>% group_by(memberid,resort_region_code,resort_type_code) %>%
  mutate(max_roomnight_rrc=max(roomnights),min_roomnight_rrc=min(roomnights),total_visit_rrc=n(),
         min_adult_rrc=max(numberofadults),min_adult_rrc=min(numberofadults),
         max_children_rrc=max(numberofchildren),min_children_rrc=min(numberofchildren)) %>% ungroup()

prac <- full %>% filter(is_train==1) %>% group_by(pax,room_stay,adv_booking,checkin_d) %>%
  summarize(prac_spent=n(),
         prac_min=min(amount_spent_per_room_night_scaled)/prac_spent,
         prac_max=max(amount_spent_per_room_night_scaled)/prac_spent,
         prac_avg=mean(amount_spent_per_room_night_scaled)/prac_spent) %>% ungroup() 
  
cmpr <- full %>% filter(is_train==1) %>% 
  group_by(cluster_code,member_age_buckets,pax,room_type_booked_code) %>%
  summarise(cmpr_spent=n(),
         cmpr_min=min(amount_spent_per_room_night_scaled)/cmpr_spent,
         cmpr_max=max(amount_spent_per_room_night_scaled)/cmpr_spent,
         cmpr_avg=mean(amount_spent_per_room_night_scaled)/cmpr_spent) %>% ungroup()

sssr <- full %>% filter(is_train==1) %>% 
  group_by(season_holidayed_code,state_code_residence,state_code_resort,reservationstatusid_code) %>%
  summarise(sssr_spent=n(),
            sssr_min=min(amount_spent_per_room_night_scaled)/sssr_spent,
            sssr_max=max(amount_spent_per_room_night_scaled)/sssr_spent,
            sssr_avg=mean(amount_spent_per_room_night_scaled)/sssr_spent) %>% ungroup()



# #lets get mode details on memeber
# full %>% group_by(memberid) %>% count(sort=TRUE)
# full %>% group_by(memberid,persontravellingid) %>% count(sort=TRUE)
# 
# full %>% ggplot(aes(factor(resort_region_code),amount_spent_per_room_night_scaled))+geom_boxplot()
# 
# full %>% ggplot(aes(factor(main_product_code),amount_spent_per_room_night_scaled))+geom_boxplot()
# 
# full %>% filter(total_pax>12) %>% group_by(memberid) %>% count(sort=TRUE)
# full %>% ggplot(aes(factor(main_product_code),total_pax))+geom_boxplot()
# full %>% ggplot(aes(factor(channel_code),total_pax))+geom_boxplot()
# 
# full %>% filter(total_pax>15)%>% group_by(main_product_code,total_pax) %>% count()
# 
# 
# full %>% group_by(resort_id,checkin_month) %>% count(sort=TRUE)
# full %>% group_by(resort_id,season_holidayed_code) %>% count(sort=TRUE)
# 
# train %>% ggplot(aes(resort_region_code))+geom_bar(stat='count')
# train %>% ggplot(aes(resort_type_code))+geom_bar(stat='count')
# train %>% ggplot(aes(season_holidayed_code))+geom_bar(stat='count')
# 
# train %>% group_by(season_holidayed_code) %>% count(sort=TRUE)
# 
# full %>% filter(is_train==1) %>% ggplot(aes(resort_region_code,fill=factor(cluster_code)))+
#   geom_bar(stat="count")
# 
# full %>% filter(is_train==1) %>% ggplot(aes(resort_type_code,fill=factor(cluster_code)))+
#   geom_bar(stat="count")
# full %>% filter(is_train==1) %>% ggplot(aes(resort_type_code,fill=factor(resort_region_code)))+
#   geom_bar(stat="count")
# 
# full %>% filter(is_train==1) %>% ggplot(aes(cluster_code,fill=factor(state_code_resort)))+
#   geom_bar(stat="count")
# 
# train %>% group_by(resort_id) %>% count(sort = TRUE)

full <- full %>% left_join(cmpr)
full <- full %>% left_join(sssr) %>% left_join(.,prac)
full <- full %>% mutate(channel_code=factor(channel_code),
                        main_product_code = factor(main_product_code),
                        persontravellingid=factor(persontravellingid),
                        resort_region_code=factor(resort_region_code),
                        resort_type_code=factor(resort_type_code),
                        season_holidayed_code=factor(season_holidayed_code),
                        state_code_residence=factor(state_code_residence),
                        state_code_resort=factor(state_code_resort),
                        room_type_booked_code=factor(room_type_booked_code),
                        member_age_buckets=factor(member_age_buckets),
                        cluster_code=factor(cluster_code),memberid=factor(memberid),
                        reservationstatusid_code = factor(reservationstatusid_code))

colls <- c('booking_month','booking_day','checkin_month','checkin_day','booking_d','checkin_d',
           'checkout_d','checkin_quarter','checkin_weekend','pax','room_stay','adv_booking')
full %<>% mutate_at(colls,funs(factor(.)))


X <- colnames(full)[c(5:16,18,19,21,22,26:53,55:57,59:61,63:65)]
Y <- colnames(train)[c(24)]
str(full[,X])
X1 <- X[c(13,14,15,16,18,20,22:53)]
X2 <- X[c(27:53)]

tr<- full %>% filter(is_train==1)
te <- full %>% filter(is_train==0)


h2o.init()

tr <- as.h2o(tr[c(X,Y)])
te <- as.h2o(te[c(X)])

almname <- paste('ak_h2o_automl',format(Sys.time(),"%d%H%M%S"),sep = '_')
autoML <- h2o.automl(X,Y,training_frame = tr,seed=2019,stopping_metric=c("RMSE"),max_models = 10)
autoML


glmML <- h2o.glm(X,Y,training_frame = tr,seed=2019,nfolds = 5,family = "gaussian",lambda_search = T,
                 standardize = F,remove_collinear_columns = T,alpha = 0)
glmML
save(glmML,file="glmmlv3.rda")

g2<-h2o.gbm(X,Y,training_frame = tr,ntrees = 400
            ,score_tree_interval = 50 #,nfolds = 4,stopping_rounds = 4,stopping_tolerance = 0
            ,learn_rate = 0.001,max_depth = 8,sample_rate = 0.6,col_sample_rate = 0.6
            ,model_id = "g2")

g2

save(g2, file="gbmv2.rda")
aml_test_pred <- h2o.predict(autoML,te)
prediction <- as.vector(aml_test_pred)

glm_pred <- h2o.predict(glmML,te)
glm_pred <- as.vector(glm_pred)

g2_pred <- h2o.predict(g2,te)
g2_pred <- as.vector(g2_pred)

sample$amount_spent_per_room_night_scaled <- g2_pred*0.55+prediction*0.4+glm_pred*0.05
filename <- paste('ak_ensemble_4',format(Sys.time(),"%Y%m%d%H%M%s"),sep = '_')
write.csv(sample,paste0(filename,'.csv',collapse = ''),row.names = FALSE)


