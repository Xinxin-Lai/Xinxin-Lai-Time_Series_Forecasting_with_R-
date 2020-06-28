# Chapter 6

library(fpp2)
library(seasonal)

# 1.Show that a 3*5 MA is equivalent to a 7-term weighted moving average with weights of 0.067, 0.133, 0.200, 0.200, 0.200, 0.133, and 0.067.
# 5-MA: (Y1+Y2+Y3+Y4+Y5)/5
#           (Y2+Y3+Y4+Y5+Y6)/5
#              (Y3+Y4+Y5+Y6+Y7)/5
# 3-MA: ((Y1+Y2+Y3+Y4+Y5)/5+(Y2+Y3+Y4+Y5+Y6)/5+(Y3+Y4+Y5+Y6+Y7)/5)/3
#      =1/15Y1+2/15Y2+3/15Y3+3/15Y4+3/15Y5+2/15Y6+1/15Y7
#      =0.067Y1+0.133Y2+0.2Y3+0.2Y4+0.2Y5+0.133Y6+0.067Y7

# 2.The plastics data set consists of the monthly sales (in thousands) of product A for a plastics manufacturer for five years.
#a.Plot the time series of sales of product A. Can you identify seasonal fluctuations and/or a trend-cycle?
help("plastics")
autoplot(plastics)
plot(plastics,xlab='Yr',ylab='Sale',main='Product A Sales') #a fancier way
# I can spot strong seasonal fluctuations and upward trend.

# b.Use a classical multiplicative decomposition to calculate the trend-cycle and seasonal indices.
decompose_plastics <- decompose(plastics,
                                type = 'multiplicative')
autoplot(decompose_plastics)
decompose_plastics$trend
decompose_plastics$seasonal

#c.Do the results support the graphical interpretation from part a?
#Yes. As the seasonal effect are all near 1 which indicates strong seasonal effects and upward trend as well.

#Compute and plot the seasonally adjusted data.
plot(seasadj(decompose_plastics))
# In a comparison way:
autoplot(plastics, series="Data") +
  autolayer(trendcycle(decompose_plastics), series="Trend") +
  autolayer(seasadj(decompose_plastics), series="Seasonally Adjusted") +
  xlab("Year") + ylab("Monthly Sales amount") +
  ggtitle("Sales of plastic product (in thousand)") +
  scale_color_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

# e.Change one observation to be an outlier (e.g., add 500 to one observation), and recompute the seasonally adjusted data. What is the effect of the outlier?
plastics_new <- plastics
plastics_new[20] <- plastics_new[20]+500
decompose_plastics_new <- decompose(
  plastics_new,
  type = 'multiplicative'
)

autoplot(plastics_new,series = 'Data')+
  autolayer(trendcycle(decompose_plastics_new),
            series = "Trend") +
  autolayer(seasadj(decompose_plastics_new),
            series = "Seasonally Adjusted") +
  xlab("Year") + 
  ylab("Monthly Sales amount") +
  ggtitle("Sales of plastic projuct with outlier") +
  scale_color_manual(values=c("gray", "blue", "red"),
                     breaks=c("Data", "Seasonally Adjusted", "Trend"))
# The outlier affects trend little, but affects the seasonally adjusted data severely. Seasonally adjusted data have errors like the original data have.

#f. Does it make any difference if the outlier is near the end rather than in the middle of the time series?
plastics_new[55] <- plastics_new[55] + 500
decompose_plastics_new <- decompose(
  plastics_new,
  type = "multiplicative"
)
autoplot(plastics_new, series = "Data") +
  autolayer(trendcycle(decompose_plastics_new),
            series = "Trend") +
  autolayer(seasadj(decompose_plastics_new),
            series = "Seasonally Adjusted") +
  xlab("Year") + ylab("Monthly Sales amount") +
  ggtitle("Sales of plastic projuct with outliers") +
  scale_color_manual(values=c("gray", "blue", "red"),
                     breaks=c("Data", "Seasonally Adjusted", "Trend"))
# If the spike is near the end, the effect to the trend decreases.

# 3.Recall your retail time series data (from Exercise 3 in Section 2.10). Decompose the series using X11. Does it reveal any outliers, or unusual features that you had not noticed previously?
#load the retail.xlsx data:
library(xlsx)
retail <- read.xlsx("retail.xlsx",
                    sheetIndex = 1,
                    startRow = 2)
head(retail)

# change the retail data to ts object:
ts_retail <- ts(retaildata[,"A3349335T"],
                frequency=12, 
                start=c(1982,4))

#plot the data to see if there is a trend /seasonality.
autoplot(ts_retail)
#There are strong upward trend and seasonality.

# Decompose the series using X11.
x11_retail <- seas(ts_retail,x11 = "")

autoplot(x11_retail)
# There were some outliers especially in 1986. And surprisingly the seasonal effect decreases as trend increases.

#4.Figures 6.16 and 6.17 show the result of decomposing the number of persons in the civilian labour force in Australia each month from February 1978 to August 1995.
#a.Write about 3–5 sentences describing the results of the decomposition. Pay particular attention to the scales of the graphs in making your interpretation.
stl_labour <- stl(labour, s.window = 13, robust = TRUE)

autoplot(stl_labour) + xlab("Year") +
  ggtitle("STL decomposition of civilian labour force")

# Second figure: ggsubseries plot of civilian labor force.
ggsubseriesplot(seasonal(stl_labour)) +
  ylab("Seasonal")

autoplot(labour, series="Data") +
  autolayer(trendcycle(stl_labour), series="Trend") +
  autolayer(seasadj(stl_labour), series="Seasonally Adjusted") +
  xlab("Year") + ylab("Number of people") +
  ggtitle("Number of people in the civilian labour force in Australia") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))
# Overall, the number of people in the civilian labor force in Australia increased over time. There were big recessions 
# around 1991 and can be observed by the seasonal adjusted data.

# b.Is the recession of 1991/1992 visible in the estimated components?
# It can be visible through seasonal adjusted data.

#5.This exercise uses the cangas data (monthly Canadian gas production in billions of cubic metres, January 1960 – February 2005).
#a. Plot the data using autoplot(), ggsubseriesplot() and ggseasonplot() to look at the effect of the changing seasonality over time. What do you think is causing it to change so much?
help("cangas")
autoplot(cangas)
ggsubseriesplot(cangas)
ggseasonplot(cangas) 
#The gas production increased in Winter and decreased in Summer. Maybe cold weather cause people to use more gas for heating. But overall gas production has increased and it may happen because of the wider spread of air conditioning.

# b.Do an STL decomposition of the data. You will need to choose s.window to allow for the changing shape of the seasonal component.
stl_cangas <- stl(cangas,s.window = 13, robust = TRUE)

# Show each STL decomposed component:
autoplot(stl_cangas) +
  ggtitle("Monethly Canadian Gas Production",
          subtitle = "STL decomposition")
# Can observe the size of seasonality increased in 19802 and decreased in 1990s.

# (STL decomposition) See trend-cycle component and seasonally adjusted data along with original data.
autoplot(cangas, series = "Data") +
  autolayer(seasadj(stl_cangas), series = "Seasonally Adjusted") +
  autolayer(trendcycle(stl_cangas), series = "Trend-cycle") +
  ggtitle("Monthly Canadian gas production(STL decomposition)") +
  ylab(expression(paste("Gas production (x", 10^{9}, m^{3}, ")"))) +
  scale_color_manual(values = c("gray", "blue", "red"),
                     breaks = c("Data", "Seasonally Adjusted", "Trend-cycle"))

#c.Compare the results with those obtained using SEATS and X11. How are they different?
x11_cangas <- seas(cangas, x11="")
seats_cangas <- seas(cangas)

# Show each X11 decomposed component:
autoplot(x11_cangas) +
  ggtitle("Monthly Canadian Gas Production",
          subtitle = "X11 decomposition")

# (X11 decomposition) See trend-cycle component and seasonally adjusted data along with original data.
autoplot(cangas, series = "Data") +
  autolayer(seasadj(x11_cangas), series = "Seasonally Adjusted") +
  autolayer(trendcycle(x11_cangas), series = "Trend-cycle") +
  ggtitle("Monthly Canadian gas production(X11 decomposition)") +
  ylab(expression(paste("Gas production (x", 10^{9}, m^{3}, ")"))) +
  scale_color_manual(values = c("gray", "blue", "red"),
                     breaks = c("Data", "Seasonally Adjusted", "Trend-cycle"))

# Show each SEATS decomposed component:
autoplot(seats_cangas) +
  ggtitle("Monthly Canadian Gas Production",
          subtitle = "SEATS decomposition")

# (SEATS decomposition) See trend-cycle component and seasonally adjusted data along with original data.
autoplot(cangas, series = "Data") +
  autolayer(seasadj(seats_cangas), series = "Seasonally Adjusted") +
  autolayer(trendcycle(seats_cangas), series = "Trend-cycle") +
  ggtitle("Monthly Canadian gas production(SEATS decomposition)") +
  ylab(expression(paste("Gas production (x", 10^{9}, m^{3}, ")"))) +
  scale_color_manual(values = c("gray", "blue", "red"),
                     breaks = c("Data", "Seasonally Adjusted", "Trend-cycle"))
# seas function did multiplicative decomposition. Therefore seasonal component and remainder component have mean at 1, not 0. And the proportion of seasonality to trend decreased, then increased, and then decreased again. 

#6.We will use the bricksq data (Australian quarterly clay brick production. 1956–1994) for this exercise.
# a.Use an STL decomposition to calculate the trend-cycle and seasonal indices. (Experiment with having fixed or changing seasonality.)
# STL with fixed seasonality
stl_brick_fixed_st <- stl(bricksq,s.window = "periodic", robust = TRUE)

# STL with changing seasonality:
stl_brick_changing_st <- stl(bricksq,s.window = 5, robust = TRUE)

# plot decomposed data
autoplot(stl_brick_fixed_st) +
  ggtitle("Brick production data decomposed by STL with fixed seasonality")
autoplot(stl_brick_changing_st) +
  ggtitle("Brick production data decomposed by STL with changing seasonality")
# I can spot the changes and smaller remainder in changing seasonality

# b.Compute and plot the seasonally adjusted data.
# Plot data which are decomposed by STL with fixed seasonality:
autoplot(bricksq, series = "Data") +
  autolayer(trendcycle(stl_brick_fixed_st),
            series = "Trend-cycle") +
  autolayer(seasadj(stl_brick_fixed_st),
            series = "Seasonally Adjusted Data") +
  ggtitle("Quarterly clay brick production in Australia",
          subtitle = "-decomposed by STL with fixed seasonality") +
  scale_color_manual(values = c("gray", "red", "blue"),
                     breaks = c("Data", "Trend-cycle", "Seasonally Adjusted Data"))

#Plot data which are decomposed by STL with changing seasonality:
autoplot(bricksq, series = "Data") +
  autolayer(trendcycle(stl_brick_changing_st),
            series = "Trend-cycle") +
  autolayer(seasadj(stl_brick_changing_st),
            series = "Seasonally Adjusted Data") +
  ggtitle("Quarterly clay brick production in Australia",
          subtitle = "-decomposed by STL with changing seasonality") +
  scale_color_manual(values = c("gray", "red", "blue"),
                     breaks = c("Data", "Trend-cycle", "Seasonally Adjusted Data"))
# With changing seasonality, the trend-cycle tend to be smoother and smaller 

# c.Use a naïve method to produce forecasts of the seasonally adjusted data.
stl_brick_fixed_st %>% seasadj() %>% naive() %>% autoplot() + 
  ggtitle(label = "Naive forecast of seasonally adjusted brick data",
          subtitle = "after STL decomposition with fixed seasonality")

stl_brick_changing_st %>% seasadj() %>% naive() %>% autoplot() + 
  ggtitle(label = "Naive forecast of seasonally adjusted brick data",
          subtitle = "after STL decomposition with changing seasonality")
# Can spot prediction for changing seasonality has slightly smaller range than the fixed rate, 
# and it happened because the variance of the remainder component decreased when the seasonality can be changed.

# d.Use stlf() to reseasonalise the results, giving forecasts for the original data.
stlf_brick <- stlf(bricksq)
autoplot(stlf_brick)

# e.Do the residuals look uncorrelated?
checkresiduals(stlf_brick)
# The residuals are correlated with each other.

# f.Repeat with a robust STL decomposition. Does it make much difference?
stlf_brick_robust <- stlf(bricksq, robust = TRUE)
autoplot(stlf_brick_robust)
checkresiduals(stlf_brick_robust)
# It seemed like the autocorrelations become lowe generally, but there are still some spikes left.

# g.Compare forecasts from stlf() with those from snaive(), using a test set comprising the last 2 years of data. Which is better?
trainset_brick <- subset(bricksq, 
                         end = length(bricksq) - 8)
testset_brick <- subset(bricksq,
                        start = length(bricksq) - 7)

snaive_brick <- snaive(trainset_brick)
stlf_brick_part <- stlf(trainset_brick,robust = TRUE)

# plot data and forecast results:
autoplot(bricksq, series = "Original data") +
  geom_line(size = 1) +
  autolayer(stlf_brick_part, PI = FALSE, size = 1,
            series = "stlf") +
  autolayer(snaive_brick, PI = FALSE, size = 1,
            series = "snaive") +
  scale_color_manual(values = c("gray50", "blue", "red"),
                     breaks = c("Original data", "stlf", "snaive")) +
  scale_x_continuous(limits = c(1990, 1994.5)) +
  scale_y_continuous(limits = c(300, 600)) +
  guides(colour = guide_legend(title = "Data")) +
  ggtitle("Forecast from stlf and snaive functions") +
  annotate(
    "rect",
    xmin=1992.75,xmax=1994.5,ymin=-Inf,ymax=Inf,
    fill="lightgreen",alpha = 0.3
  )
# stlf is better and more similar to original data than the forecast from snaive.

# 7.Use stlf() to produce forecasts of the writing series with either method="naive" or method="rwdrift", whichever is most appropriate. Use the lambda argument if you think a Box-Cox transformation is required.
help("writing")
str(writing)
head(writing)

autoplot(writing)
# can see that there are increasing trend in writing data. Therefore it would be better to use rwdrift method to forecast non-seasonal(seasonally adjusted) component.
# I think that it would be better to do Box-Cox transformation to make the size of the seasonal variation in the data about the same across the whole series.
stlf_writing <- stlf(writing,
                     s.window = 13,
                     robust = TRUE,
                     lambda = BoxCox.lambda(writing),
                     method = "rwdrift")
autoplot(stlf_writing)

# 8.Use stlf() to produce forecasts of the fancy series with either method="naive" or method="rwdrift", whichever is most appropriate. Use the lambda argument if you think a Box-Cox transformation is required.
str(fancy)
head(fancy)

autoplot(fancy)
# There is increasing trend that it would be better to use rwdrift method for forecasting non-seasonal component.
# And it would be better to do Box-Cox transformation to make the variation in the data about the same across the whole series.
stlf_fancy <- stlf(fancy,
                   s.window = 13,
                   robust = TRUE,
                   lambda = BoxCox.lambda(fancy),
                   method = "rwdrift")
autoplot(stlf_fancy)
# The prediction intervals increase dramatically because of Box-Cox transformation. But without the transformation, the forecasts are unreasonable.

# References:
# 1.https://github.com/JehyeonHeo/Forecasting_with_R_practices/blob/master/Chapter3.rmd
# 2.https://otexts.com/fpp2/toolbox.html
