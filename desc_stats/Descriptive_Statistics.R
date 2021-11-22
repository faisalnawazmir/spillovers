## -------------------------------------------------------------------------------------------------------------------
rm(list = ls())
options(scipen = 100, digits = 8)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(dse)
library(readxl)
library(zoo)
library(tseries)
library(xts)
library(imputeTS)
library(ggpubr)
library(corrplot)


## -------------------------------------------------------------------------------------------------------------------
# lag function
lagpad <- function(x, k = 1) {
  i <- is.vector(x)
  if (is.vector(x)) x <- matrix(x) else x <- matrix(x, nrow(x))
  if (k > 0) {
    x <- rbind(matrix(rep(NA, k * ncol(x)), ncol = ncol(x)), matrix(x[1:(nrow(x) - k), ], ncol = ncol(x)))
  } else {
    x <- rbind(matrix(x[(-k + 1):(nrow(x)), ], ncol = ncol(x)), matrix(rep(NA, -k * ncol(x)), ncol = ncol(x)))
  }
  if (i) x[1:length(x)] else x
}


## -------------------------------------------------------------------------------------------------------------------
nat_gas_spot <- read_excel("data/NG_PRI_FUT_S1_D.xls", sheet = "Data 1", skip = 2) %>%
  rename("Natural_Gas" = 2)
crude_oil_spot <- read_excel("data/PET_PRI_SPT_S1_D.xls", sheet = "Data 1", skip = 2) %>%
  rename("Crude_Oil_WTI" = 2, "Crude_Oil_Europe_Brent" = 3)
gasoline <- read_excel("data/PET_PRI_SPT_S1_D.xls", sheet = "Data 2", skip = 2) %>%
  rename("NY_Gasoline" = 2, "Gulf_Gasoline" = 3)
heating_oil <- read_excel("data/PET_PRI_SPT_S1_D.xls", sheet = "Data 4", skip = 2) %>%
  rename("Heating_Oil" = 2)
diesel <- read_excel("data/PET_PRI_SPT_S1_D.xls", sheet = "Data 5", skip = 2) %>%
  select(-2, -3) %>%
  rename("Diesel" = 2)


## -------------------------------------------------------------------------------------------------------------------
dt <- nat_gas_spot %>%
  full_join(crude_oil_spot, by = "Date") %>%
  full_join(gasoline, by = "Date") %>%
  full_join(heating_oil, by = "Date") %>%
  full_join(diesel, by = "Date") %>%
  arrange(Date) %>%
  filter(Date >= "1997-01-07") %>%
  mutate(Date=as.Date(Date))

nat_gas_spot_ts <- xts(nat_gas_spot$Natural_Gas, frequency = 365, order.by = as.Date(nat_gas_spot$Date))
crude_oil_spot <- ts(crude_oil_spot[, 1:2], frequency = 7, start = as.Date("1997-01-07"), end = as.Date("2021-07-01"))
NY_Gasoline_ts <- ts(gasoline[, c(1, 2)], start = as.Date("1997-01-07"))
Gulf_Gasoline_ts <- ts(gasoline[, c(1, 3)], start = as.Date("1997-01-07"))
heating_oil_ts <- ts(heating_oil, start = as.Date("1997-01-07"))
diesel_ts <- ts(diesel, start = as.Date("1997-01-07"))
ts_list <- list(nat_gas_spot_ts, crude_oil_spot, NY_Gasoline_ts, Gulf_Gasoline_ts, heating_oil_ts, diesel_ts)


## -------------------------------------------------------------------------------------------------------------------
attr(nat_gas_spot_ts, "frequency") <- 7
nat_gas_spot_ts <- na.omit(nat_gas_spot_ts)
plot(decompose(as.ts(nat_gas_spot_ts))) 
# Dont know hot to use this decomposition, but might be a useful tool in the future.


## -------------------------------------------------------------------------------------------------------------------
colSums(is.na(dt))
nrow(dt) # about 2% NAs
aux <- dt[!complete.cases(dt), ]
head(aux)


## -------------------------------------------------------------------------------------------------------------------
dt <- dt %>% select(-Crude_Oil_Europe_Brent)
NA_dates <- dt %>%
  filter_at(vars(-Date), all_vars(is.na(.))) %>%
  pull(Date)
dt <- dt %>% filter(!Date %in% NA_dates)

aux <- dt[!complete.cases(dt), ]
aux[10:20, ]

print("Number of NAs in each column:")
colSums(is.na(aux))
print(paste("Number of rows in the dataframe:",nrow(dt)))
dt <- dt %>% mutate_all(~ replace(., is.na(.), mean(., na.rm = TRUE)))


## -------------------------------------------------------------------------------------------------------------------
long_dt <- melt(dt, id.vars = c("Date"))
print(head(long_dt))

ggplot(long_dt, aes(factor(variable), value)) + 
geom_boxplot() + 
facet_wrap(~variable, scale="free")

long_dt %>% group_by(as.factor(variable)) %>%
  get_summary_stats(type = "common")


## -------------------------------------------------------------------------------------------------------------------
cor_dt <- cor(dt[,-1])
corrplot(cor_dt, type="upper", order="hclust", tl.col="black", tl.srt=45)


## -------------------------------------------------------------------------------------------------------------------
dt[dt$Crude_Oil_WTI<0,"Crude_Oil_WTI"]
dt[dt$Date=="2020-04-20","Crude_Oil_WTI"]<-0.01


## -------------------------------------------------------------------------------------------------------------------
log_ret_dt <- tibble(dt[2:nrow(dt), "Date"])

for (i in 2:(ncol(dt))) {
  var_name <- paste(colnames(dt)[i], "log_ret", sep = "_")
  print(var_name)
  vals <- log(dt[, i]) %>% pull()
  lag_vals <- diff(vals, lag = 1)
  log_ret_dt[var_name] <- lag_vals
}

log_ret_dt[!complete.cases(log_ret_dt), ] #empty


## -------------------------------------------------------------------------------------------------------------------
long_log_ret <- melt(log_ret_dt, id.vars = c("Date"))
print(head(long_dt))

ggplot(long_log_ret, aes(factor(variable), value)) + 
geom_boxplot() + 
facet_wrap(~variable, scale="free")

long_log_ret %>% group_by(as.factor(variable)) %>%
  get_summary_stats(type = "common")


## -------------------------------------------------------------------------------------------------------------------
cor_long_dt <- cor(log_ret_dt[,-1])
corrplot(cor_long_dt, type="upper", order="hclust", tl.col="black", tl.srt=45)


## -------------------------------------------------------------------------------------------------------------------
ggplot(long_dt, aes(x = Date, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") +
  theme(axis.text.x = element_blank()) +
  ggtitle("Prices") +
  geom_vline(xintercept = as.POSIXct("2008-9-15"), color = "Red")


## -------------------------------------------------------------------------------------------------------------------
ggplot(long_log_ret, aes(x = Date, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free") +
  theme(axis.text.x = element_blank()) +
  ggtitle("Log Returns - May be stationary")


## -------------------------------------------------------------------------------------------------------------------
#Natural Gas
model <- lm(Natural_Gas_log_ret ~ I(lagpad(Natural_Gas_log_ret, 1)), data = log_ret_dt)
print("UNIT ROOT TEST")
adf.test(log_ret_dt$Natural_Gas_log_ret) #no unit root
res <- summary(model)$residuals
ar_lm <- lm(res ~ lagpad(res, 1))
print("RESIDUAL AUTOCORRELATION TEST")
summary(ar_lm) #no autocorrelation of residuals


## -------------------------------------------------------------------------------------------------------------------
#Crude Oil
model <- lm(Crude_Oil_WTI_log_ret ~ I(lagpad(Crude_Oil_WTI_log_ret, 1)), data = log_ret_dt)
print("UNIT ROOT TEST")
adf.test(log_ret_dt$Crude_Oil_WTI_log_ret) # no unit root
res <- summary(model)$residuals
ar_lm <- lm(res ~ lagpad(res, 1))
print("RESIDUAL AUTOCORRELATION TEST")
summary(ar_lm) #AR(1) OF RESIDUALS


## -------------------------------------------------------------------------------------------------------------------
#NY Gasoline
model <- lm(NY_Gasoline_log_ret ~ I(lagpad(NY_Gasoline_log_ret, 1)), data = log_ret_dt)
print("UNIT ROOT TEST")
adf.test(log_ret_dt$NY_Gasoline_log_ret) # no unit root
res <- summary(model)$residuals
ar_lm <- lm(res ~ lagpad(res, 1))
print("RESIDUAL AUTOCORRELATION TEST")
summary(ar_lm) #AR(1) OF RESIDUALS


## -------------------------------------------------------------------------------------------------------------------
#Gulf Gasoline
model <- lm(Gulf_Gasoline_log_ret ~ I(lagpad(Gulf_Gasoline_log_ret, 1)), data = log_ret_dt)
print("UNIT ROOT TEST")
adf.test(log_ret_dt$Gulf_Gasoline_log_ret) # no unit root
res <- summary(model)$residuals
ar_lm <- lm(res ~ lagpad(res, 1))
print("RESIDUAL AUTOCORRELATION TEST")
summary(ar_lm) #AR(1) OF RESIDUALS


## -------------------------------------------------------------------------------------------------------------------
#Heating Oil Gasoline
model <- lm(Heating_Oil_log_ret ~ I(lagpad(Heating_Oil_log_ret, 1)), data = log_ret_dt)
print("UNIT ROOT TEST")
adf.test(log_ret_dt$Heating_Oil_log_ret) # no unit root
res <- summary(model)$residuals
ar_lm <- lm(res ~ lagpad(res, 1))
print("RESIDUAL AUTOCORRELATION TEST")
summary(ar_lm) #AR(1) OF RESIDUALS


## -------------------------------------------------------------------------------------------------------------------
#Diesel
model <- lm(Diesel_log_ret ~ I(lagpad(Diesel_log_ret, 1)), data = log_ret_dt)
print("UNIT ROOT TEST")
adf.test(log_ret_dt$Diesel_log_ret) # no unit root
res <- summary(model)$residuals
ar_lm <- lm(res ~ lagpad(res, 1))
print("RESIDUAL AUTOCORRELATION TEST")
summary(ar_lm) #no autocorrelation of residuals


## -------------------------------------------------------------------------------------------------------------------
diesel_ma <- arima(log_ret_dt$Diesel_log_ret, order = c(0, 0, 1))
diesel_ma

ts.plot(log_ret_dt$Diesel_log_ret)
points(log_ret_dt$Diesel_log_ret-diesel_ma$residuals, type = "l", col = 2, lty = 2)

arima(log_ret_dt$Diesel_log_ret, order = c(1, 0, 0)) 
#same as summary(model) from before, so I could also use this package for AR testing

