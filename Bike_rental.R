data <- day_data
View(day_data)
library(dplyr)
library(corrplot)
library(ggplot2)
library(stats)
library(GGally)
library(broom)
library(tidyverse)
library(magrittr)
library(lmtest)
library(orcutt)
library(modelr)
library(olsrr)
library(Metrics)
library(base)
library(fastDummies)

dim(data)
names(data)
data <- data.frame(data)
str(data)


data %>% select(season,holiday,weekday,mnth,workingday,weathersit,temp,atemp,hum,windspeed,cnt) %>%
  ggpairs()

model1 <- lm(cnt ~ factor(season)+factor(holiday) + factor(weekday)+factor(workingday)+factor(weathersit)+temp+hum+windspeed, data = data)
model1
summary(model1)

model2 <- lm(cnt ~ factor(season)+factor(holiday)+factor(weathersit)+temp+hum+windspeed, data = data)
model2
summary(model2)


Residuals <- resid(model2)
Residuals
#produce residual vs. fitted plot
plot(fitted(model2), Residuals,col='purple4')

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm((Residuals),col=' purple4')

#add a straight diagonal line to the plot
qqline(Residuals) 

plot(hist(Residuals),col='purple')

shapiro.test(Residuals)

data1 %>%
  add_predictions(model2) %>%
  summarise(MSE = mean((cnt - pred)^2, na.rm=TRUE))

pred2 <- data %>% mutate(pred2 = predict(model3))
pred2
predict(model4,n.ahead=367)

view(pred2)
data <- data

data %>%
  add_predictions(model2) %>%
  summarise(MSE = mean((cnt - pred)^2, na.rm=TRUE))

ols_step_best_subset(model4)

# Studentized residuals (internally studentized residuals)
# r_i
ols_plot_resid_stand(model4)

# Studentized Deleted residual plot (bar)
ols_plot_resid_stud(model4)


#Deleted Studentized Residual vs Fitted Values Plot
ols_plot_resid_stud_fit(model3)



#Cooks Distance (Bar chart)
ols_plot_cooksd_bar(model2)

#Cooks Distance (line chart)
ols_plot_cooksd_chart(model3)

cooksd <- cooks.distance(model2)

 # Removing Outliers
  # influential row numbers
  influential <- as.numeric(names(cooksd)[(cooksd > 0.011)])
  
# Alternatively, you can try to remove the top x outliers to have a look
# top_x_outlier <- 2
# influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))

bike_data <- data[-influential, ]
view(bike_data)
model3 <- lm(cnt ~ factor(season)+factor(holiday)+factor(weathersit)+temp+hum+windspeed, data =bike_data )
model3
summary(model3)
length(bike_data$hum)



ols_plot_cooksd_chart(model)
Residuals <- resid(model3)
Residuals
#produce residual vs. fitted plot
plot(fitted(model3), Residuals,col='dark blue')

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(Residuals,col='dark blue')

#add a straight diagonal line to the plot
qqline(Residuals) 

plot(hist(Residuals),col='light blue')

shapiro.test(Residuals)
acf(Residuals)
pacf(Residuals)


bgtest(model3)
model4 <- cochrane.orcutt(model3)
model4
summary(model4)
predict(model5)
view(predict(model5))
residuals <- residuals(model4)
mse <- mean(residuals ^2 )
x1<- bike_data %>%
  add_predictions(model4) %>% 
  summarise(MSE = mean((cnt - pred)^2, na.rm=TRUE))
x1


test <- bike1
x<- test %>%
  add_predictions(model5) %>%
summarise(MSE = mean((cnt - pred)^2, na.rm=TRUE))
x

#produce residual vs. fitted plot
plot(fitted(model4), residuals,col='hot pink')

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm((residuals),col='hot pink')

#add a straight diagonal line to the plot
qqline(residuals) 

plot(hist(residuals),col='hot pink')

shapiro.test(residuals)


#exploratory analysis


h <- hist(data$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = '#00C1AA' )

xfit <- seq(min(data$cnt),max(data$cnt), length = 50)
yfit <- dnorm(xfit, mean =mean(data$cnt),sd=sd(data$cnt))
yfit <- yfit*diff(h$mids[1:2])*length(data$cnt)
lines(xfit,yfit, col='purple', lwd= 3)


Cor_actual_temp<-cor(x = data$temp, y = data$cnt)
Cor_actual_feel_temp <- cor(x = data$atemp, y =data$cnt)
bk_sh_dy_cor<- data %>% select (cnt,temp,hum,windspeed)
bk_sh_dy_cor<- data.frame(bk_sh_dy_cor)

colnames(data)[1] <- "Total Number of Bike Rentals"
colnames(data)[2] <- "Temperature"
colnames(data)[3] <- "Humidity"
colnames(data)[4] <- "Windspeed"

cor(bk_sh_dy_cor)

corplot_bk_sh <- cor(bk_sh_dy_cor)
corrplot(corplot_bk_sh, method="number")

data %>% select(temp,atemp,hum,windspeed,cnt) %>%
  ggpairs()

data %>% select(season,mnth,holiday,weekday,workingday,weathersit,cnt) %>%
  ggpairs(color='blue')




#scatterplots
library(ggplot2)

ggplot_Temp_Rent<- ggplot(data,aes(x=temp,y=cnt))+ geom_point(shape=8,color='green')+xlab(" Temp. in Celcius")+ylab("Bike Rentals")
ggplot_Temp_Rent

ggplot_hum_Rent<- ggplot(data, aes(x=hum,y=cnt))+ geom_point(shape=8,color='blue')+xlab(" Humidity")+ylab("Bike Rentals")
ggplot_hum_Rent

ggplot_atemp_Rent<- ggplot(data, aes(x=atemp,y=cnt))+ geom_point(shape=8,color='purple')+xlab(" feeling temp. in Celsius")+ylab("Bike Rentals")
ggplot_atemp_Rent

ggplot_windspeed_Rent<- ggplot(data, aes(x=windspeed,y=cnt))+ geom_point(shape=8,color='hot pink')+xlab("windspeed")+ylab("Bike Rentals")
ggplot_windspeed_Rent

ggplot__Rent<- ggplot(data, aes(x=windspeed,y=cnt))+ geom_point(shape=8,color='navy blue')+xlab("windspeed")+ylab("Bike Rentals")
ggplot_winspeed_Rent

#boxplots


boxplot(data$cnt ~ data$season,
        data = data,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals",
        col = c("coral", "coral1", "coral2", "coral3")) 


boxplot(data$cnt ~ data$holiday,
        data = data,
        main = "Total Bike Rentals Vs Holiday",
        xlab = "Holiday",
        ylab = "Total Bike Rentals",
        col = c("#00BBE5","#00A5FF")) 

boxplot(data$cnt ~ data$weathersit,
        data = data,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink2","pink3")) 

boxplot(data$cnt ~ data$weekday,
        data = data,
        main = "Total Bike Rentals Vs Weekday",
        xlab = "Weekday",
        ylab = "Total Bike Rentals",
        col = c("purple", "purple1", "purple2", "purple3","purple4","blueviolet")) 

plot(data$dteday,data$cnt,type = "p",
     main = "Total Bike Rentals Vs DateDay",
     xlab = "Year",
     ylab = "Total Bike Rentals",
     col  = "#00A5FF",
     pch  = 19)

p1 <- ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~treatment)

data %>% pivot_wider(names_from =temp,
                     values_from = breaks)
philip1 <- phi %>% 
  rename(Confirmed=confirmed, Death=death,Recovered=recovered) %>%
  pivot_longer(c("Confirmed","Death","Recovered"),names_to="type",values_to="cases")
view(philip1)

d1 <- data %>% pivot_longer(c("hum","temp","atemp","windspeed","cnt"),names_to="variables",values_to="count")
view(d1)
                   
boxplot(d1$counts,
        data = d1,
        ylab = "count")


boxplot(data$cnt ,
data = data,
ylab = "count",
col = 'blue')

boxplot(data$temp ,
        data = data,
        ylab = "Temperature",
        col = 'pink')
boxplot(data$atemp ,
        data = data,
        ylab = "Atemp",
        col = '#9590FF')

boxplot(data$hum ,
        data = data,
        ylab = "Humidity",
        col = '#00BF7D')
boxplot(data$hum ,
        data = data,
        ylab = "hum",
        col = c("purple", "purple1", "purple2", "purple3")) 
boxplot(data$windspeed ,
        data = data,
        ylab = "windspeed",
        col = '#5BB300')
