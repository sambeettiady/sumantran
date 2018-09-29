rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(forecast)
library(tseries)
library(xgboost)
library(xts)
library(caret)
library(dummies)
library(Boruta)

setwd("data/sumantran/")

#Read data
suman_data = read_csv(file = 'Sumantran_Dataset.csv')
#nrow(suman_data)
#nrow(unique(suman_data))
suman_data = suman_data %>% unique(.)

names(suman_data) = tolower(names(suman_data))

#Create all date time variables
suman_data$start_date = mdy(suman_data$start_date)
suman_data$end_date = mdy(suman_data$end_date)
suman_data = unite(data = suman_data,col = start_ts,sep = ' ',remove = F,start_date,start_time)
suman_data = unite(data = suman_data,col = end_ts,sep = ' ',remove = F,end_date,end_time)
suman_data = unite(data = suman_data,col = worker_pos_id,sep = '_',remove = T,worker_id,position_id)
suman_data$year = year(suman_data$start_date)
suman_data$month = month(suman_data$start_date)
suman_data$day_of_month = day(suman_data$start_date)
suman_data$day_of_week = wday(suman_data$start_date,label = T)
suman_data$shift_time_hours = difftime(suman_data$end_ts,suman_data$start_ts,units = 'hours')
suman_data$shift_time_hours = as.numeric(suman_data$shift_time_hours)
suman_data$interval = interval(suman_data$start_ts,suman_data$end_ts)
# Precautionary step 
suman_data = suman_data[suman_data$start_ts <= suman_data$end_ts,]


colnames(suman_data)


#suman_data[5:12,]
#colnames(suman_data)
#modified_df = subset(suman_data, select=c(2,3,4,5,7,14,15))
#colnames(modified_df)

#z1 = modified_df[5:12,]


overlaps<- function(intervals){
  cat ("`\n gsl ", groupSizeList)
  #Sys.sleep(1)
  groupSizeList_1<-as.numeric(groupSizeList[1])
  output_vec = numeric(length = groupSizeList_1)
  if(groupSizeList[1] == 1){
    flag = FALSE
    output_vec[1] = flag
  } else {
    for(i in 1:(groupSizeList[1]-1)){
      flag = FALSE
      output_vec[i] = flag
      for(j in (i+1):groupSizeList[1]){
        if(int_overlaps(intervals[i],intervals[j])){
          flag = TRUE
          output_vec[i] = flag
          break
        }
      }
    }  
  }
  nextrowid<-groupSizeList[1]+as.numeric(groupedData$rowid[[1]])
  
  groupedData  <<- groupedData[groupedData$rowid>=nextrowid & groupedData$rowid<=lastrowid,]
  groupSizeList <<- groupSizeList[2:length(groupSizeList)]
  return(output_vec)
  
}

x = suman_data
groupedData <<- x %>%
  group_by(department_id, worker_pos_id) 
groupedData$rowid = row.names(groupedData)
groupedData$rowid = as.numeric(groupedData$rowid)
groupSizeList <<- group_size(groupedData)
lastrowid<<-nrow(groupedData)
length(groupSizeList)
groupSizeList[1]


overlapData <- groupedData %>%
  mutate(ovl <- overlaps(groupedData$interval))

#intervals = groupedData$interval
overlapData <- as.data.frame(overlapData)
overlapData

#groupedData <- as.data.frame(groupedData)
#groupedData

save(overlapData, file = "overlapData.Rda")


#Start here
load(file = 'overlapData.Rda')

colnames(overlapData)
names(overlapData)[names(overlapData) == 'ovl <- overlaps(groupedData$interval)'] <- 'overlapIndicator'
overlapData = as.data.frame(overlapData)
head(overlapData)

finalData <- overlapData[overlapData$overlapIndicator == 0,]
nrow(finalData)

summary(finalData)

#Function to create timeseries from timestamp dataframe
create_ts = function(ts_data = finalData,ts = F){
    filldata = data.frame(timestamp = format(seq(from=ISOdate(2011,5,2,8,30),to=ISOdate(2014,1,1,19,0), by="30 min"), "%Y-%m-%d %H:%M:%S"))
    start_ts = as.xts(order.by = ymd_hms(ts_data$start_ts),rep(1,nrow(ts_data)))
    end_ts = as.xts(order.by = ymd_hms(ts_data$end_ts),rep(-1,nrow(ts_data)))
    fill_ts = as.xts(order.by = ymd_hms(filldata$timestamp),rep(0,nrow(filldata)))

    merged_ts = rbind.xts(start_ts,end_ts,fill_ts)
    temp_df = data.frame(date=index(merged_ts), workers = coredata(merged_ts)) %>% group_by(date) %>%
    summarise(workers = sum(workers)) %>% mutate(workers = cumsum(workers))
    if(ts){
        firstHour <- 48*(as.Date("2011-05-02 08:30:00")-as.Date("2011-1-1 00:00:00"))
        ts_final = ts(data = temp_df$workers,start = c(2011,firstHour),frequency = 48*365)
        return(ts_final)
    }else{
        return(temp_df)
    }
}

###EDA###
dept_id_list = unique(finalData$department_id)

for(dept in dept_id_list){
df_eda = create_ts(ts_data = finalData[finalData$department_id == dept,])
df_eda$hour_of_day = hour(df_eda$date) + minute(df_eda$date)/60
filtered_data = finalData[finalData$department_id == dept,]

#Plot Time Series
ts_plot = ggplot(data = df_eda,mapping = aes(x = date,y = workers)) + geom_line(col = 'steelblue3') + 
    xlab('Time Stamp') + ylab('Number of workers') + ggtitle(paste('Time Series - ',dept,sep = ''));ts_plot
ggsave(filename = paste('time_series_',dept,'.png',sep = ''),plot = ts_plot,width = 4,height = 4)

#Shift Time Distribution - Boxplot
shift_dist = ggplot(data = filtered_data,mapping = aes(x=0,y = shift_time_hours)) + 
    geom_boxplot(fill = 'steelblue3') + ylab('Shift Time') + ggtitle(paste('Shift Time Distribution - ',dept));shift_dist
ggsave(filename = paste('shift_time_distribution_',dept,'.png',sep = ''),plot = shift_dist,width = 4,height = 4)

#Seasonality Plots
#Yearly
yd = filtered_data %>% group_by(year) %>% summarise(man_hours = sum(shift_time_hours)/length(unique(start_date))) %>% 
    filter(year != 2014)
yearly_trend = ggplot(yd,aes(x = year,y = man_hours)) + geom_bar(stat = 'identity',fill = 'steelblue3') + 
    xlab('Year') + ylab('Average Man Hours per day') + ggtitle(paste('Yearly Trend -',dept,sep = ''));yearly_trend
ggsave(filename = paste('yearly_',dept,'.png',sep = ''),plot = yearly_trend,width = 4,height = 4)

#Monthly
md = filtered_data %>% mutate(month = month.abb[month]) %>% group_by(month) %>% summarise(man_hours = sum(shift_time_hours)/length(unique(start_date)))
monthly_trend = ggplot(md,aes(x = month,y = man_hours)) + geom_bar(stat = 'identity',fill = 'steelblue3') + 
    xlab('Month') + ylab('Average Man Hours per day') + scale_x_discrete(limits=month.abb) +
    ggtitle(paste('Monthly Seasonality -',dept,sep = ''));monthly_trend
ggsave(filename = paste('monthly_',dept,'.png',sep = ''),plot = monthly_trend,width = 4,height = 4)

#Day of Week
wd = filtered_data %>% group_by(day_of_week) %>% summarise(man_hours = sum(shift_time_hours)/length(unique(start_date)))
weekly_trend = ggplot(wd,aes(x = day_of_week,y = man_hours)) + geom_bar(stat = 'identity',fill = 'steelblue3') + 
    xlab('Day of Week') + ylab('Average Man Hours per day') + ggtitle(paste('Within Week Seasonality -',dept,sep = '')) + 
    scale_x_discrete(limits=c('Mon','Tue','Wed','Thu','Fri','Sat','Sun'));weekly_trend
ggsave(filename = paste('day_of_week_',dept,'.png',sep = ''),plot = weekly_trend,width = 4,height = 4)

#Day of Month
dmd = filtered_data %>% group_by(day_of_month) %>% summarise(man_hours = sum(shift_time_hours)/length(unique(start_date)))
day_of_month_trend = ggplot(dmd,aes(x = day_of_month,y = man_hours)) + geom_line(col = 'steelblue3') + 
    xlab('Day of Month') + ylab('Average Man Hours per day') + ggtitle(paste('Within Month Seasonality -',dept,sep = ''));day_of_month_trend
ggsave(filename = paste('day_of_month_',dept,'.png',sep = ''),plot = day_of_month_trend,width = 4,height = 4)

#Within Day Demand
wdd = df_eda %>% group_by(hour_of_day) %>% summarise(avg_num_workers = mean(workers),gs = n()) %>% filter(gs >= 100)
within_day_trend = ggplot(wdd,aes(x = hour_of_day,y = avg_num_workers)) + geom_line(col = 'steelblue3') + 
    xlab('Time of Day') + ylab('Average Number of Workers') + ggtitle(paste('Within Day Seasonality - ',dept,sep = ''));within_day_trend
ggsave(filename = paste('time_of_day_',dept,'.png',sep = ''),plot = within_day_trend,width = 4,height = 4)
}

dept_wise_man_hours = finalData %>% mutate(year_mon = paste(year,month)) %>% group_by(department_id,year_mon) %>% 
    summarise(pct_man_hours = sum(shift_time_hours)) %>% group_by(department_id) %>%
    summarise(avg_monthly_man_hours = sum(pct_man_hours)/n())
#    mutate(pct_man_hours = round(100*pct_man_hours/sum(pct_man_hours),0))

dept_bar_plot = ggplot(dept_wise_man_hours,aes(x = factor(department_id),y = avg_monthly_man_hours)) + 
    geom_bar(stat = 'identity',fill = 'steelblue3') + xlab('Department ID') + ylab('Average Monthly Man Hours') + 
    ggtitle('Average Monthly Man Hours - Dept. Wise');dept_bar_plot
ggsave(filename = 'Average Monthly Man Hours - Dept. Wise.png',plot = dept_bar_plot,width = 8,height = 8)

#Dept-wise
dept_id_list = unique(finalData$department_id)
df_list = vector(mode = 'list',length = length(dept_id_list))
names(df_list) = dept_id_list

for(dept in dept_id_list){
print(paste('Department ID:',dept))
ts_obj = create_ts(ts_data = finalData[finalData$department_id == dept,])
ts_obj$year = year(ts_obj$date)
ts_obj$month = month(ts_obj$date,label = T)
ts_obj$day_of_month = day(ts_obj$date)
ts_obj$day_of_week = wday(ts_obj$date,label = T)
ts_obj$hour_of_day = hour(ts_obj$date) + minute(ts_obj$date)/60
ts_obj$train_ind = ifelse(ts_obj$date < as.Date('2013-11-01'),1,0)
#ts_obj = ts_obj[,-1]
ts_obj$store_closed = ifelse(ts_obj$hour_of_day < 8.5,1,ifelse(ts_obj$hour_of_day >= 19,1,0))
ohe_df = dummy.data.frame(data.frame(ts_obj),names=c('month','day_of_week'),sep='_',dummy.classes = F)
ohe_df = ohe_df %>% mutate(workers_log = log1p(workers)) %>% select(-workers,-month_Dec,-day_of_week_Sun) 

train_df = ohe_df[ohe_df$train_ind == 1,] %>% select(-train_ind)
test_df = ohe_df[ohe_df$train_ind == 0,] %>% select(-train_ind)

print('Data Frame Created!')
df_list[as.character(dept)][[1]] = vector(mode = 'list',length = 3)
df_list[as.character(dept)][[1]][[1]] = train_df

#Run xgboost
grid = expand.grid(nrounds = 500, lambda = 0.0001, alpha = 0.00001, eta = 0.1)
model = train(workers_log ~ . - date, data = train_df, method = "xgbLinear", tuneGrid = grid, gamma = 0.1)
print('Model Done!')
print(model$results)

test_df$pred = predict(object = model,newdata = test_df)
test_df$pred = ifelse(test_df$pred < 0,0,round(exp(test_df$pred) - 1,0))
test_df$workers = round(exp(test_df$workers_log) - 1,0)
df_list[as.character(dept)][[1]][[2]] = test_df
df_list[as.character(dept)][[1]][[3]] = model
print(paste('RMSE: ',RMSE(pred = test_df$pred,obs = test_df$workers)))
print('-----Next Iteration Starts-----')
}

save(df_list, file = "dept_wise_models.rds")

#Load results list
load(file = 'dept_wise_models.rds')

model_1177 = df_list['1177'][[1]][[3]]
model_1183 = df_list['1183'][[1]][[3]]
model_1590 = df_list['1590'][[1]][[3]]
plot(varImp(model_1177),top = 10,main = 'Variable Importance - xgBoost - Dept. 1177')    
plot(varImp(model_1183),top = 10)    
plot(varImp(model_1590),top = 10)    
print(model_1177$results)

time_series_obj = create_ts(ts_data = finalData,ts = T)
stl_obj = stl(log1p(time_series_obj),s.window = 'periodic')
plot(stl_obj,main = 'Time Series Decompostion',col = 'steelblue3')

ts_obj = create_ts(ts_data = finalData)
ts_obj$year = year(ts_obj$date)
ts_obj$month = month(ts_obj$date,label = T)
ts_obj$day_of_month = day(ts_obj$date)
ts_obj$day_of_week = wday(ts_obj$date,label = T)
ts_obj$hour_of_day = hour(ts_obj$date) + minute(ts_obj$date)/60
ts_obj$train_ind = ifelse(ts_obj$date < as.Date('2013-11-01'),1,0)
#ts_obj = ts_obj[,-1]
ts_obj$store_closed = ifelse(ts_obj$hour_of_day < 8.5,1,ifelse(ts_obj$hour_of_day >= 19,1,0))
ohe_df = dummy.data.frame(data.frame(ts_obj),names=c('month','day_of_week'),sep='_',dummy.classes = F)
ohe_df = ohe_df %>% mutate(workers_log = log1p(workers)) %>% select(-workers,-month_Dec,-day_of_week_Sun) 

train_df = ohe_df[ohe_df$train_ind == 1,] %>% select(-train_ind)
test_df = ohe_df[ohe_df$train_ind == 0,] %>% select(-train_ind)

borutaFit <- Boruta(workers_log ~ . - date, data = train_df,maxRuns = 11)    # data set

getSelectedAttributes(borutaFit)                         # extract important variables
attStats(borutaFit)                                      # full finalBoruta statistics
borutaFit                                                # importance of variables (values)
plot(borutaFit)                                          # importance of variables (visual)

#Get results for scheduling algo
for(dept in dept_id_list){
    temp = df_list[as.character(dept)][[1]][[2]]
    temp$dept_id = dept
    temp = temp %>% select(dept_id,date,workers,pred)
    if(dept == 1148){overall = temp
    }else{overall = rbind(overall,temp)}
}

write.csv(overall,'dept_wise_prediction.csv',row.names = F)

#Time Series Model
ts_obj = create_ts(ts_data = finalData,ts = T)
train_ts = window(x = ts_obj,start = c(2011,48*(as.Date("2011-05-02 08:30:00")-as.Date("2011-01-01 00:00:00"))),
                  end = c(2013,48*(as.Date("2013-11-01 00:00:00")-as.Date("2013-01-01 00:00:00"))))
test_ts = window(x = ts_obj,start = c(2013,48*(as.Date("2013-11-01 00:00:00")-as.Date("2013-01-01 00:00:00"))))

decomposed_ts = decompose(x = log1p(train_ts))
plot(decomposed_ts)

seas_adj = seasadj(decomposed_ts)
plot(seas_adj)

holt_forecast <- HoltWinters(x = seas_adj,beta = F,gamma = F)
holt_forecast
plot(holt_forecast)
holt_forecast$SSE

plot(seas_adj-holt_forecast$fitted[,1])

ts_obj = create_ts(ts_data = finalData)
ts_obj$year = year(ts_obj$date)
ts_obj$month = month(ts_obj$date,label = T)
ts_obj$day_of_month = day(ts_obj$date)
ts_obj$day_of_week = wday(ts_obj$date,label = T)
ts_obj$hour_of_day = hour(ts_obj$date) + minute(ts_obj$date)/60
ts_obj$train_ind = ifelse(ts_obj$date < as.Date('2013-11-01'),1,0)
#ts_obj = ts_obj[,-1]
ts_obj$store_closed = ifelse(ts_obj$hour_of_day < 8.5,1,ifelse(ts_obj$hour_of_day >= 19,1,0))
ohe_df = dummy.data.frame(data.frame(ts_obj),names=c('month','day_of_week'),sep='_',dummy.classes = F)
ohe_df = ohe_df %>% mutate(workers_log = log1p(workers)) %>% select(-workers,-month_Dec,-day_of_week_Sun) 

train_df = ohe_df[ohe_df$train_ind == 1,] %>% select(-train_ind)
test_df = ohe_df[ohe_df$train_ind == 0,] %>% select(-train_ind)

print('Data Frame Created!')
df_list[as.character(dept)][[1]] = vector(mode = 'list',length = 3)
df_list[as.character(dept)][[1]][[1]] = train_df

#Run xgboost
grid = expand.grid(nrounds = 500, lambda = 0.0001, alpha = 0.00001, eta = 0.1)
model = train(workers_log ~ . - date, data = train_df, method = "xgbLinear", tuneGrid = grid, gamma = 0.1)
print('Model Done!')
print(model$results)

test_df$pred = predict(object = model,newdata = test_df)
test_df$pred = ifelse(test_df$pred < 0,0,round(exp(test_df$pred) - 1,0))
test_df$workers = round(exp(test_df$workers_log) - 1,0)
df_list[as.character(dept)][[1]][[2]] = test_df
df_list[as.character(dept)][[1]][[3]] = model
print(paste('RMSE: ',RMSE(pred = test_df$pred,obs = test_df$workers)))

#Check for seasonality
stl_obj = stl(log1p(train_ts),s.window = 'period')
plot(stl_obj)
forecast_stl = forecast(object = stl_obj,h = 48*62)
ts.plot(train_ts,exp(forecast_stl$mean) - 1,test_ts,col=c(1:3))

stl_obj
resids = ifelse(forecast_stl$mean < 0,0,round(exp(forecast_stl$mean) - 1,0)) - test_ts
sqrt(mean(resids^2))

tot_workers = vector(mode = 'numeric',length = 2967)
pred_workers = vector(mode = 'numeric',length = 2967)

for(dept in dept_id_list){
    df_test = df_list[as.character(dept)][[1]][[2]]
    tot_workers = tot_workers + df_test$workers
    pred_workers = tot_workers + df_test$pred
}

RMSE(obs = tot_workers,pred = pred_workers)
