# Chapter 8

library(fpp2)
library(xlsx)
# install.packages("rdatamarket")
# library(rdatamarket)
library(tseries)

# 1. a. For a series to be white noie we accept 95% of spikes within the bounds. For all these 3 cases we can
# spot 95% spikes are within the dash lines, so we expect each autocorrelatin to be close to zero. Although in case 2 there are some more amounts of 
# spikes but we still can consider them as less than 5%. THus, all the plots indicate white noise.
# 1. b.Critical values are defined by the T(the length of time series). In all three plots, bound length decreases as the length
# of time series increases. For all three cases the critical value is different due to different length of time series.

#2. A classic example of a non-stationary series is the daily closing IBM stock price series (data set ibmclose). Use R to plot the daily closing prices 
#for IBM stock and the ACF and PACF. Explain how each plot shows that the series is non-stationary and should be differenced.
help("ibmclose")
View(ibmclose)
ggtsdisplay(ibmclose)
#ACF plot shows that the autocorrelation values are bigger than critical value and decrease slowly. r1 is large(near to 1) and positive.
#It means that IBM stock data are non-stationary(i.e.,prediction using lagged values).
# PACF plot shows that there is a strong correlation between IBM stock data and their 1 lagged values. It means that IBM stock data can be predicted by 1 lagged values and they aren't stationary.
# To get stationary data, IBM stock data need differencing.

# 3.For the following series, find an appropriate Box-Cox transformation and order of differencing in order to obtain stationary data.
# a.usnetelec
autoplot(usnetelec)
# It is linearly increasing data. It looked like that the data only need first differencing.
Box.test(diff(usnetelec), type='Ljung-Box')
# 1st differenced usnetelec data can be thought of as a white noise series since p-value is large.
kpss.test(diff(usnetelec))
# kpss test result also shows that first differencing made the data stationary.

# b.usgdp
autoplot(usgdp)
#It's almost linear increasing data. It looked like that the data only need first order of differencing.
Box.test(diff(usgdp),type = "Ljung-Box")
#Since the p-value is tiny, 1st order differencing cannot be thought of as a white noise series.
kpss.test(diff(usgdp))
#kpss result also shows that 1st order differencing cannot make the data stationary. May need second order differencing.
ggtsdisplay(diff(usgdp))
#There is still a trend left in the differenced data. One more order differecing is neede:
ndiffs(usgdp)
autoplot(diff(diff(usgdp)))
# Ploe shows that the twice differenced data is like white noise series.
Box.test(diff(diff(usgdp)), type = "Ljung-Box")
# But it couldn't pass Ljung-Box test.
ggAcf(diff(diff(usgdp)))
# There are still some autocorrelations left.
kpss.test(diff(diff(usgdp)))
# But kps test result shows that 2nd order of differencing could make the data stationary.

#c. mcopper
autoplot(mcopper)
# mcopper data have increasing trend. And they have bigger variation for bigger prices.Thus, I will ust Box-Cox transformation before differecing.
mcopper_bc <- BoxCox.lambda(mcopper)
autoplot(diff(BoxCox(mcopper, mcopper_bc)))
autoplot(diff(BoxCox(mcopper, mcopper_bc)))
Box.test(diff(BoxCox(mcopper,mcopper_bc)),
         type = "Ljung-Box")

ggAcf(diff(BoxCox(mcopper, mcopper_bc)))
#It failed the test and the plot shows there are still some autocorrelations left.
kpss.test(diff(BoxCox(mcopper,mcopper_bc)))
# But kpss test result shows that differencing with Box-Cox transformation was enough to make the series white noise.
# But even if differencing with Box-Cox transormation didn't make the data like white noise, it can make it stationary.

# d.enplanements
help("enplanements")
autoplot(enplanements)

# enplanements data have seasonality and increasing trend even if the nubmer of enplanements fell in 2001. Therefore, I think that data need seasonal differencing. 

# The variations are bigger for bigger nubmers. Therefore I 'll use Box-Cox transformation before differencing.
lambda_enplanements <- BoxCox.lambda(enplanements)
ndiffs(enplanements)
nsdiffs(enplanements)
# the data need 1 order of differencing and 1 seasonal differencing.
autoplot(
  diff(
    diff(
      BoxCox(enplanements, lambda_enplanements),
      lag = 12
    )
  )
)
Box.test(
  diff(
    diff(
      BoxCox(enplanements, lambda_enplanements),
      lag = 12
    )
  ),
  type = "Ljung-Box"
)
# The plot shows that the BoxCox transformtion and multiple differncings made the data like white noise but the Box-Ljung test didn't.
ggAcf(
  diff(
    diff(
      BoxCox(enplanements,lambda_enplanements),
      lag = 12
    )
  )
)
# There are still some autocorrelations left.
kpss.test(
  diff(
    diff(
      BoxCox(enplanements, lambda_enplanements),
      lag=12
    )
  )
)
# But the kpss test result shows that the diferencing with Box-Cox Transformation was enough to make the data stationary.
# Thus we can conclude that the differencing with Box-Cox Transformation didn't make the data like white noise series but made it stationary.

#e.visitors
help("visitors")
autoplot(visitors)
# The plot shows that visitors data have seasonality with increasing trend and similar to enplanment we need to do Box-Cox transformation first.
lambda_visitors <- BoxCox.lambda(visitors)
ndiffs(visitors)
nsdiffs(visitors)
#visitors data need 1 order and 1 seasonal differencing.
autoplot(
  diff(
    diff(
      BoxCox(visitors,lambda_visitors),
      lag = 12
    )
  )
)
Box.test(
  diff(
    diff(
      BoxCox(visitors, lambda_visitors),
      lag = 12
    )
  ),
  type = "Ljung-Box"
)
# Plot result looked like BoxCox and multiple differencing made the data like white noise series.
ggAcf(
  diff(
    diff(
      BoxCox(visitors, lambda_visitors),
      lag = 12
    )
  )
)
# There are sutll some autocorrelations left.
kpss.test(
  diff(
    diff(
      BoxCox(visitors, lambda_visitors),
      lag = 12
    )
  )
)
# But the kpss test result shows that the diferencing with Box-Cox Transformation was enough to make the data stationary.
# Thus we can conclude that the differencing with Box-Cox Transformation didn't make the data like white noise series but made it stationary.

# 4.For the enplanements data, write down the differences you chose above using backshift operator notation.


#Reference:
# 1.http://rstudio-pubs-static.s3.amazonaws.com/479030_fdeee27ba11d4d65a4aa8c2256285ada.html
# 2. https://github.com/JehyeonHeo/Forecasting_with_R_practices/blob/master/Chapter8.rmd