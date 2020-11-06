require(ggplot2)

str(diamonds)
View(diamonds)

?diamonds
help(diamonds)
diamond_df <- as.data.frame(diamonds)

sum(is.na(diamond_df))

str(diamond_df)

require(dplyr)
diamond_df <- mutate_if(diamond_df, is.factor, as.character)

str(diamond_df)

#getting all the uppercase
diamond_df <- mutate_if(diamond_df, is.character, toupper)

# checking and removing all the duplicated rows
sum(duplicated(diamond_df))

#diamond_df_1 <- diamond_df[which(!duplicated(diamond_df)), ]

diamond_df <- distinct(diamond_df)
sum(duplicated(diamond_df))


################### EDA begins ###############

###carat,depth, table , price , x , y , z # they are cont variables
#carat

summary(diamond_df$carat)

boxplot(diamond_df$carat)
#outlier are 
#q3+1.5*IQR
#q1-1.5*IQR

quantile(diamond_df$carat)

quantile(diamond_df$carat, c(0.10 , 0.28 , 0.83))


quantile(diamond_df$carat, seq(0, 1, by = 0.01))

plot(quantile(diamond_df$carat, seq(0, 1, by = 0.01)))

diamond_df$carat[which(diamond_df$carat > 2.18)] <- 2.18

plot(quantile(diamond_df$carat ,seq(0, 1, by = 0.01)))

#cut
#require(dummies)
#diamond_df <- dummy.data.frame(diamond_df_1)
#diamond_df
#depth

plot(quantile(diamond_df$depth ,seq(0,1,by = 0.01)))

quantile(diamond_df$depth ,seq(0,1,by = 0.01))


diamond_df$depth[diamond_df$depth > 65.6] = 65.6

diamond_df$depth[diamond_df$depth < 57.9] = 57.9


#table
quantile(diamond_df$table , seq(0,1,0.01))

plot(quantile(diamond_df$table , seq(0,1,0.01)))

diamond_df$table[diamond_df$table < 53.0] = 53.0

diamond_df$table[diamond_df$table > 64.0] = 64.0

#price


#x
quantile(diamond_df$x , seq(0,1,0.01))
plot(quantile(diamond_df$x , seq(0,1,0.01)))

diamond_df$x[diamond_df$x < 4.0200]= 4.0200
diamond_df$x[diamond_df$x > 8.3500]= 8.3500

#y

quantile(diamond_df$y , seq(0,1,0.01))
plot(quantile(diamond_df$y , seq(0,1,0.01)))

diamond_df$y[diamond_df$y < 4.0400]= 4.0400
diamond_df$y[diamond_df$y > 8.3300]= 8.3300

#z

quantile(diamond_df$z , seq(0,1,0.01))
plot(quantile(diamond_df$z , seq(0,1,0.01)))

diamond_df$z[diamond_df$z < 2.48]= 2.48
diamond_df$z[diamond_df$z > 5.15]= 5.15



require(dummies)

final_data <- mutate_if(diamond_df,is.numeric,scale)
#summary(diamond_df)
final_data <- dummy.data.frame(final_data)

######################################

#linear Regression / Model building starts

set.seed(123)
#sample(10,7)

i <- sample(nrow(final_data),0.75*nrow(final_data))

trn <- final_data[i,]
val <- final_data[-i,]


###### LR  model ######

#1
model_1 <- lm(price ~ .,data = trn)
summary(model_1)

#2
model_2 <- step(model_1)
summary(model_2)

require(car)

sort(vif(model_2))

#3
model_3 <- lm(formula = price ~ carat + cutFAIR + cutGOOD + cutIDEAL + cutPREMIUM + 
                colorD + colorE + colorF + colorG + colorH + colorI + clarityI1 + 
                clarityIF + claritySI1 + claritySI2 + clarityVS1 + clarityVS2 + 
                clarityVVS1 + depth + table + x + z, data = trn)

summary(model_3)

sort(vif(model_3))

#4
model_4 <- lm(formula = price ~ carat + cutFAIR + cutGOOD + cutIDEAL + cutPREMIUM + 
                colorD + colorE + colorF + colorG + colorH + colorI + clarityI1 + 
                clarityIF + claritySI1 + claritySI2 + clarityVS1 + clarityVS2 + 
                clarityVVS1 + depth + table + z, data = trn)

summary(model_4)

sort(vif(model_4))

#5
model_5 <- lm(formula = price ~ carat + cutFAIR + cutGOOD + cutIDEAL + cutPREMIUM + 
                colorD + colorE + colorF + colorG + colorH + colorI + clarityI1 + 
                clarityIF + claritySI1 + claritySI2 + clarityVS1 + clarityVS2 + 
                clarityVVS1 + depth + table , data = trn)

summary(model_5)

sort(vif(model_5))

#6
model_6 <- lm(formula = price ~ carat + cutFAIR + cutGOOD + cutIDEAL  + 
                colorD + colorE + colorF + colorG + colorH + colorI + clarityI1 + 
                clarityIF + claritySI1 + claritySI2 + clarityVS1 + clarityVS2 + 
                clarityVVS1 + depth + table , data = trn)

summary(model_6)

sort(vif(model_6))


################# NOW WE TEST/VALIDATE OUR MODEL ###################

val$pred_price <- predict(model_6,val)

require(forecast)

accuracy(val$pred_price,val$price)

# converting price from z score to price
# z =(x-u)/sd , u=3932 ,sd=3989

mean(diamond_df$price)
sd(diamond_df$price)

val$price = val$price * 3989
val$price = val$price + 3932
val$pred_price = val$pred_price * 3989
val$pred_price = val$pred_price +3932
View(val)

# checking correlation

cor(val$pred_price,val$price)
#cor(val$pred_price,val$price)^2 = R-square

