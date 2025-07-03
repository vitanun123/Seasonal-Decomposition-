#project 1 
library(ggplot2)
library(zoo)
library(deseats)
library(forecast)
library(TSA)
library(seasonal)
library(ggpubr)
library(tidyr)
library(tseries)

#1.plot the figures
 PCE <- read_ts("Personal Consumtion Expenditures.csv",header = TRUE)
 Xt <- ts(PCE,start = c(1960,1), frequency = 4) 
Yt <- log(Xt) 
plot_PCE <- autoplot.zoo(Yt) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Log of Millions of USD") +
  ggtitle("Quarterly log-transformed Personal Consumtion Expanditures 
          Q1 1961 to Q4 2018")
plot_PCE

periodogram(Yt, main = "Periodogram of the US-Log-Personal Consumption Expanditures")

#2. using X12-ARIMA to decompose the not log-transformed series.
est_x12 <- seas(Xt, x11 = "")
trend_x12 <- est_x12$data[, "trend"]
season_x12 <- est_x12$data[, "seasonal"]
TS <- cbind(
  "Observations" = Xt, 
  "Trend" = trend_x12, 
  "Seasonality" = season_x12 * mean(Xt)
)
plot_X12 <- autoplot.zoo(TS, facets = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Millions of USD") +
  ggtitle("US PCE with trend and (rescaled) seasonality according to X-12-ARIMA") +
  scale_color_manual(name = "Series", values = c("grey60", "red", "blue"))

# using X12-ARIMA with log-tranformed series
log_trend_x12 <- log(est_x12$data[, "trend"])
log_season_x12 <- log(est_x12$data[, "seasonal"])
TS <- cbind(
  "Observations" = c(Yt), 
  "Trend" = log_trend_x12, 
  "Seasonality" = log_season_x12 + mean(Yt)
)
plot_log_X12 <- autoplot.zoo(TS, facets = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Log of Millions of USD") +
  ggtitle("Log-PCE with trend and (shifted) seasonality according to X-12-ARIMA") +
  scale_color_manual(name = "Series", values = c("grey60", "red", "blue"))

#plot not-log tranformed and log tranformed in a single plot to compare the trend
ggarrange(plot_X12, plot_log_X12,nrow = 1,ncol = 2)

# fit BV 4.1
est_bv41 <- BV4.1(Yt)
plot_bv41 <- deseats::autoplot(est_bv41, which = 5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Log of Millions of USD") +
  ggtitle("The US-Log-PCE with trend and (shifted) seasonality according to the BV4.1 base model")


# fit the log-tranformated series by using deseats package 
smoothing_options <- set_options(order_poly = 3)
est_deseats <- deseats(Yt, smoothing_options)
bwidth(est_deseats)
plot_de_PCE<- deseats::autoplot(est_deseats, which = 5) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Log of Millions of USD") +
  ggtitle("The US-Log-PCE with trend and (shifted) seasonality according to DeSeaTS")

# compare 4 plots in a single plot
ggarrange(plot_X12,plot_log_X12,plot_bv41,plot_de_PCE,nrow = 2,ncol = 2)

deseason_x12 <- est_x12$data[, "final"]
deseason_bv41 <- exp(deseasonalize(est_bv41))
deseason_deseats <- exp(deseasonalize(est_deseats))
deseason_all <- data.frame(
  "Year" = c(time(Xt)),
  "Observations" = c(Xt),
  "X-12-ARIMA" = c(deseason_x12),
  "BV4.1" = c(deseason_bv41),
  "DeSeaTS" = c(deseason_deseats)
)
head(deseason_all,5)


deseason_all_long <- pivot_longer(deseason_all, cols = 3:6,      names_to = "Method", values_to = "Value")
deseason_all_long$Method[deseason_all_long$Method == "X.12.ARIMA"] <- "X-12-ARIMA"
deseason_all_long$Method <- factor(deseason_all_long$Method,
                                   levels = c("X-12-ARIMA", "BV4.1", "DeSeaTS"))
plot_deseason <- ggplot(deseason_all_long, aes(x = Year, y = Observations)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_line(aes(color = "1")) +
  geom_line(aes(x = Year, y = Value, color = "2"), linewidth = 0.7) +
  facet_grid(Method ~ .) +
  scale_color_manual(name = "Series", values = c("1" = "grey50", "2" = "red"),
                     labels = c("1" = "NSA", "2" = "SA")) +
  ylab("Millions of USD") +
  ggtitle("Observed US-PCE together with various seasonally adjusted versions")
plot_deseason

parOld <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), cex = 1, mar = c(4, 4, 3, 2) + 0.1)
periodogram(deseason_all$X.12.ARIMA, main = "(a) X-12-ARIMA")
periodogram(deseason_all$BV4.1, main = "(c) BV4.1")
periodogram(deseason_all$DeSeaTS, main = "(d) DeSeaTS")
par(parOld)

res_x12 <- log(irregular(est_x12))
res_bv41 <- residuals(est_bv41)
res_deseats <- residuals(est_deseats)
res_all <- cbind(
  "X-12-ARIMA" = res_x12,
  "BV4.1" = res_bv41,
  "DeSeaTS" = res_deseats
)
plot_statio <- autoplot.zoo(res_all) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  ylab("Log of Millions of USD") +
  ggtitle("Stationarized Log-US-PCE according to various methods") +
  facet_free()
plot_statio

parOld <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), cex = 1, mar = c(4, 4, 3, 2) + 0.1)
periodogram(res_all[, "X-12-ARIMA"], main = "(b) X-12-ARIMA")
periodogram(res_all[, "BV4.1"], main = "(c) BV4.1")
periodogram(res_all[, "DeSeaTS"], main = "(d) DeSeaTS")
par(parOld)

acf_res_deseats <- ggAcf(c(residuals(est_deseats))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("ACF of the DeSeaTS residuals (additive model)")
acf_res_deseats

# fit. the best arma(p,q) for the residuals from the deseats decomposition
model <- s_semiarma(Yt, set_options(order_poly = 3))
model@par_model

# Jarque-Bera test for normality on the ARMA residuals
jarque.bera.test(model@par_model$residuals)

#Retransform your point and interval forecasts 
set.seed(123)
fc <- predict(model, n.ahead = 4, method = "boot")
fc@pred
fc@interv
deseats::autoplot(fc) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Log of Millions of USD") +
  ggtitle("Final part of the US-Log-CPE with point and interval forecasts") +
  coord_cartesian(xlim = c(1960, 2019), ylim = c(14.50, 15.20))

fc_retransf <- expo(fc)
deseats::autoplot(fc_retransf) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("Year") +
  ylab("Millions of USD") +
  ggtitle("Final part of the US-GDP with point and interval forecasts") +
  coord_cartesian(xlim = c(1960, 2019), ylim = c(3000000, 3700000))












