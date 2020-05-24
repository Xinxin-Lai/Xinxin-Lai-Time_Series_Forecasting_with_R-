library(fpp2)

# ---------------Chapter3 Exercise--------------
# 1. For the following series, find an appropriate Box-Cox transformation in order to stabilise the variance.
help("usnetelec")
lambda.usnetelec <- BoxCox.lambda(usnetelec)
print(c("Good value of lambda for usnetelec: ", 
        lambda.usnetelec))
autoplot(BoxCox(usnetelec,lambda.usnetelec))

help("usgdp")
lambda.usgdp <- BoxCox.lambda(usgdp)
print(c("Good value of lambda for usgdp: ", 
        lambda.usgdp))
autoplot(BoxCox(usgdp,lambda.usgdp))

help("mcopper")
lambda.mcopper <- BoxCox.lambda(mcopper)
print(c("Good value of lambda for mcopper: ", 
        lambda.mcopper))
autoplot(BoxCox(mcopper,lambda.mcopper))

help("enplanements")
lambda.enplanements <- BoxCox.lambda(enplanements)
print(c("Good value of lambda for enplanements: ", 
        lambda.enplanements))
autoplot(BoxCox(enplanements,lambda.enplanements))


# 2.Why is a Box-Cox transformation unhelpful for the cangas data?
help('cangas')
autoplot(cangas) # compare autoplot with the Box-cox plot
lambda.cangas <- BoxCox.lambda(cangas)
print(c("Good value of lambda for cangas: ", 
        lambda.cangas))
autoplot(BoxCox(cangas,lambda.cangas))
# can spot that Box-cox transformation doesn't product simpler model

# 3.What Box-Cox transformation would you select for your retail data (from Exercise 3 in Section 2.10)?
retaildata <- readxl::read_excel("retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349335T"],
           frequency=12, start=c(1982,4))

lambda.retail <- BoxCox.lambda(myts)
print(c("selected lambda: ", lambda.retail))

fc.retail <- rwf(myts, 
                 drift=TRUE,
                 lambda = lambda.retail,
                 h =50,
                 level = 80)
fc.retail.biasadj <- rwf(myts,
                         drift = TRUE,
                         lambda = lambda.retail,
                         h = 50,
                         level= 80,
                         biasadj = TRUE)
summary(fc.retail.biasadj)
autoplot(myts) + 
  autolayer(fc.retail, series = "Drift method with Box-Cox Transformation") +
  autolayer(fc.retail.biasadj$mean, series = 'Bias Adjusted') + 
  guides(color = guide_legend(title = 'Forecast'))
# It would be better to choose bias adjusted Box-Cox Transformation ith lambda  = 0194

# 4. For each of the following series, make a graph of the data. If transforming seems appropriate, 
#   do so and describe the effect. dole, usdeaths, bricksq
help(dole)
autoplot(dole)

lambda.dole <- BoxCox.lambda(dole)
autoplot(BoxCox(dole, lambda.dole))
# It looks like for dole data, transformation makes the data better to see the pattern

help(usdeaths)
autoplot(usdeaths)
lambda.usdeaths <- BoxCox.lambda(usdeaths)
autoplot(BoxCox(usdeaths, lambda.usdeaths))
# For usdeaths data, the plot did not change much excpet the scale,it seemed it's meaningless to transform it

help("bricksq")
autoplot(bricksq)
lambda.bricksq <- BoxCox.lambda(bricksq)
autoplot(BoxCox(bricksq, lambda.bricksq))
# Similar to usdeaths data, it's meaningless to transform it


# 5. Calculate the residuals from a seasonal naïve forecast applied to the quarterly Australian beer production data
#    from 1992. The following code will help.
help("ausbeer")
beer <- window(ausbeer, start=1992)
fc <- snaive(beer)
autoplot(fc)
res <- residuals(fc)
autoplot(res)

#Test if the residuals are white noise and normally distributed.
checkresiduals(fc)
# The ACF plot indicates that the residuals aren't white noise bc of significant
# spike in Lag4,and the Box-Ljung test shows that it's statistically signifincant. Therefore they aren't white noise 

# 6.Repeat the exercise for the WWWusage and bricksq data. Use whichever of naive() or snaive() is more appropriate in each case.
help("WWWusage")
View(WWWusage)

snaive.www <- snaive(WWWusage, h=15)
autoplot(snaive.www)
checkresiduals(snaive.www)

naive.www <- naive(WWWusage)
autoplot(naive.www)
checkresiduals(naive.www)
# The ACF plot indicates that the residuals aren't white noise bc of significant
# spikes in many lags,and their distribution of residuals are not normal , and the Box-Ljung test shows that it's
# statistically signifincant. Therefore they aren't white noise in both method
# nuive method is better since there is no particular pattern in the data

snaive.bricksq <- snaive(bricksq)
autoplot(snaive.bricksq)
checkresiduals(snaive.bricksq)

naive.bricksq <- naive(bricksq)
autoplot(naive.bricksq)
checkresiduals(naive.bricksq)
# For both methods, the Ljung-Box test shows the residuals aren't white noise, and there are major 
# large spikes and their distribution of residual are not normal either
# If I have to choose, the snaive method is better with a smaller Q value than what naive 
# method has, and there is some seasonality in the data


#7. Are the following statements true or false? Explain your answer.
#a. Good forecast methods should have normally distributed residuals.
#b. A model with small residuals will give good forecasts.
#c. The best measure of forecast accuracy is MAPE.
#d. If your model doesn't forecast well, you should make it more complicated.
#e. Always choose the model with the best forecast accuracy as measured on the test set.
# Answer:
#a. It's useful to have normally distributed residuals, but it isn't necessary for good forecast methods to have them
#b. Good forecast methods don't need to have small residuals. But their residuals should be uncorrelated and have zero mean
#c. The best measure of forecast accuracy is different on a case by case basis.
#d. More complicated forecast method does not guarantee better forecast.
#e. When choosing a forecast model, whether the residuals have zero mean and whether they are uncorrelated should also be considered. Simply choosing best accuracy model isn't good.

#8. For your retail time series (from Exercise 3 in Section 2.10):
# a. Split the data into two parts using
myts.train <- window(myts, end=c(2010,12))
myts.test <- window(myts, start=2011)

# b. Check that your data have been split appropriately by producing the following plot.
autoplot(myts) +
  autolayer(myts.train, series="Training") +
  autolayer(myts.test, series = "Test")

# c.Calculate forecasts using snaive applied to myts.train.
snaive.myts.train <- snaive(myts.train)

# d.Compare the accuracy of your forecasts against the actual values stored in myts.test.
accuracy(snaive.myts.train,myts.test)

# e. Check the residuals.
checkresiduals(snaive.myts.train)
# Do the residuals appear to be uncorrelated and normally distributed?
# Answer, no. From ACF, there are large amounts of spike and the residuals are not normally distributed either.

# f.How sensitive are the accuracy measures to the training/test split?
# I think the author trying to ask the error ratio of training set compared to test set. If that is the case, it looked like 
# ME, RMSE, MAE, MASE are sensitive while MAPE & ACF1 aren't much sensitive

# 9.visnights contains quarterly visitor nights (in millions) from 1998 to 2016 for twenty regions of Australia.
# a. Use window() to create three training sets for visnights[,"QLDMetro"], omitting the last 1, 2 and 3 years; call these train1, train2, and train3, respectively. For example 
# train1 <- window(visnights[, "QLDMetro"], end = c(2015, 4)).ights contains quarterly visitor nights (in millions) from 1998 to 2016 for twenty regions of Australia.
train1 <- window(visnights[,"QLDMetro"], end=c(2015,4))
train2 <- window(visnights[,"QLDMetro"], end=c(2014,4))
train3 <- window(visnights[,"QLDMetro"], end=c(2013,4))

# b. Compute one year of forecasts for each training set using the snaive() method. Call these fc1, fc2 and fc3, respectively.
fc1 <- snaive(train1,h=4)
fc2 <- snaive(train2,h=4)
fc3 <- snaive(train3,h=4)

#c. Use accuracy() to compare the MAPE over the three test sets. Comment on these.
test1 <- window(visnights[,"QLDMetro"], start = c(2016, 1), end=c(2016,4))
test2 <- window(visnights[,"QLDMetro"], start = c(2015, 1), end=c(2015,4))
test3 <- window(visnights[,"QLDMetro"], start = c(2014, 1), end=c(2014,4))

accuracy(fc1,test1)
writeLines("")
accuracy(fc2, test2)
writeLines("")
accuracy(fc3,test3)
# MAPE was smallest for 2015 prediction and biggest for 2014 prediction

# 10.Use the Dow Jones index (data set dowjones) to do the following:
# a. Produce a time plot of the series.
help("dowjones")
autoplot(dowjones)

# b.Produce forecasts using the drift method and plot them.
drift_dj <- rwf(dowjones, drift = TRUE)
autoplot(drift_dj)

#c. Show that the forecasts are identical to extending the line drawn between the first and last observations.
dj.x <- c(1,78)
dj.y <- c(dowjones[1], dowjones[78])
lm.dj <- lm(dj.y ~ dj.x)
summary(lm.dj)
# R^2 = 1 meaning that forecasts are identical to extending the line drawn between the first and last observations
autoplot(drift_dj)+
  geom_abline(intercept = lm.dj$coefficients[1],
              slope = lm.dj$coefficients[2],
              color='red')

autoplot(drift_dj) +
  geom_line(aes(x = c(1,78),
                y = dowjones[c(1,78)]),
            color = 'red')

# d.Try using some of the other benchmark functions to forecast the same data set. Which do you think is best? Why?
checkresiduals(drift_dj)

mean_dj <- meanf(dowjones)
autoplot(mean_dj)

naive_dj <- naive(dowjones)
autoplot(naive_dj)
checkresiduals(naive_dj)

snaive_dj <- snaive(dowjones, h= 10)
autoplot(snaive_dj)
checkresiduals(snaive_dj)
# I think the naive method is best bc it is really difficult to predict stock price wtih past observations. It would be safer to use last observation with naive method

# 11. Consider the daily closing IBM stock prices (data set ibmclose).
# a.Produce some plots of the data in order to become familiar with it.
help("ibmclose")

autoplot(ibmclose)

# b.Split the data into a training set of 300 observations and a test set of 69 observations.
View(ibmclose)
ibm.train <- window(ibmclose, end = 300)
ibm.test <- window(ibmclose, start = 301)

# c.Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?
ibm.drift <- rwf(ibm.train, drift = TRUE, h= 69)
autoplot(ibm.drift) + 
  autolayer(ibm.test)

snaive.ibm <- snaive(ibm.train, h = 69)
autoplot(snaive.ibm) + 
  autolayer(ibm.test)

naive.ibm <- naive(ibm.train, h = 69)
autoplot(naive.ibm) + 
  autolayer(ibm.test)

mean.ibm <- meanf(ibm.train, h = 69)
autoplot(mean.ibm) + 
  autolayer(ibm.test)

writeLines("\nDrift method")
accuracy(ibm.drift, ibm.test)

writeLines("Snaive method")
accuracy(snaive.ibm, ibm.test)

writeLines("\nNaive method")
accuracy(naive.ibm, ibm.test)

writeLines("\nMean method")
accuracy(mean.ibm, ibm.test)

e_snaive.ibm <- ibm.test - snaive.ibm$mean
e_naive.ibm <- ibm.test - naive.ibm$mean
e_ibm.drift <- ibm.test - ibm.drift$mean
e_mean.ibm <- ibm.test - mean.ibm$mean

autoplot(e_snaive.ibm^2, series = "snaive method") +
  autolayer(e_naive.ibm^2, series = "naive method") +
  autolayer(e_ibm.drift^2, series = "drift method") +
  autolayer(e_mean.ibm^2, series = "mean method") +
  guides(colour = guide_legend(title = "Forecast")) +
  ggtitle("Errors of the forecast of closing IBM stock price") +
  ylab(expression(paste("erro", r^{2})))
# From the plot, drift method did best with lowest error

# Time series cross-validation method of tsCV function don't use full data unless h = 1. 
# For example, if usable data has 100 points and h = 3, tsCV predicts 101st point with 98 points, 
# 102nd with 99 points and 103rd with 100 points. Therefore error result value of tsCV cannot 
# help differing from the values of accuracy function. Accuracy function always get result from full data.

# Time series cross-validation method of tsCV function don't use full data unless h = 1. For example, if usable data has 100 points and h = 3, tsCV predicts 101st point with 98 points, 102nd with 99 points and 103rd with 100 points. Therefore error result value of tsCV cannot help differing from the values of accuracy function. Accuracy function always get result from full data.
ibmclose %>% tsCV(forecastfunction = snaive, h = 69) ->  e_snaive_ibm_CV
ibmclose %>% tsCV(forecastfunction = naive, h = 69) ->  e_naive_ibm_CV
ibmclose %>% tsCV(forecastfunction = rwf, drift = TRUE, h = 69) ->  e_drift_ibm_CV
ibmclose %>% tsCV(forecastfunction = meanf, h = 69) ->  e_mean_ibm_CV
autoplot(subset(e_snaive_ibm_CV^2, start = 301), series = "snaive method") +
  autolayer(subset(e_naive_ibm_CV^2, start = 301), series = "naive method") +
  autolayer(subset(e_drift_ibm_CV^2, start = 301), series = "drift method") +
  autolayer(subset(e_mean_ibm_CV^2, start = 301), series = "mean method") +
  guides(colour = guide_legend(title = "Forecast")) +
  ggtitle("Errors of the forecast of closing IBM stock price",
          subtitle = "after using tsCV function") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ylab(expression(paste("erro", r^{2})))
#d.Check the residuals of your preferred method. Do they resemble white noise?
checkresiduals(ibm.drift)
checkresiduals(naive.ibm)
# No, neither of drift method or naive method's residuals are white noise

# 12.Consider the sales of new one-family houses in the USA, Jan 1973 – Nov 1995 (data set hsales).
# a.Produce some plots of the data in order to become familiar with it.
help(hsales)
View(hsales)
autoplot(hsales)

#b. Split the hsales data set into a training set and a test set, where the test set is the last two years of data.
hsales.train <- subset(hsales,end = length(hsales)-24)
hsales.test <- subset(hsales, start = length(hsales)-23)

#c. Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?
snaive.hsales <- snaive(hsales.train, h= 24)
naive.hsales <- naive(hsales.train, h=24)
drift.hsales <- rwf(hsales.train, drift=TRUE, h=24)
mean.hsales <- meanf(hsales.train, h=24)

autoplot(snaive.hsales) +
  autolayer(hsales.test)
autoplot(naive.hsales) +
  autolayer(hsales.test)
autoplot(drift.hsales) +
  autolayer(hsales.test)
autoplot(mean.hsales) +
  autolayer(hsales.test)

writeLines("Snaive method")
accuracy(snaive.hsales, hsales.test)
writeLines("\nNaive method")
accuracy(naive.hsales, hsales.test)
writeLines("\nDrift method")
accuracy(drift.hsales, hsales.test)
writeLines("\nMean method")
accuracy(mean.hsales, hsales.test)
# Seasonal naive method did the best if we compare MAPE & MASE

# d.Check the residuals of your preferred method. Do they resemble white noise?
checkresiduals(snaive.hsales)
# No its residuals are not white noise because there are many spikes periods and the the residuals are not normally distributed 

# References:
# 1.https://github.com/JehyeonHeo/Forecasting_with_R_practices/blob/master/Chapter3.rmd
# 2.https://otexts.com/fpp2/toolbox.html
