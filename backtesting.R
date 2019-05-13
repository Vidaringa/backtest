
# Backtesta strategíu -----------------------------------------------------

library(tidyverse)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)


# Næ í gögnin og bý til breytur -------------------------------------------

getSymbols("^GSPC", from = "2000-01-01")

gspc <- GSPC[, 4] %>%
        # tail(2500) %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "date") %>% 
        as_tibble()

colnames(gspc) <- c("date","close")


gspc <- gspc %>% 
        mutate(sma20 = SMA(close, n = 20),
               ema20 = EMA(close, n = 20),
               dema_s = DEMA(close, n = 12),
               dema_l = DEMA(close, n = 20))


# Bý til signal
gspc <- gspc %>% 
        mutate(signal_20 = case_when(ema20 <= sma20 ~ 0,
                                     TRUE ~ 1),
               signal_dema = case_when(dema_s <= dema_l ~ 0,
                                       TRUE ~ 1))


# Return og strategy
perform <- gspc %>% 
        mutate(sp500_return = lead(ROC(close)),
               MA_strategy = sp500_return * signal_20,
               DEMA_strategy = sp500_return * signal_dema) %>% 
        select(date, sp500_return, MA_strategy, DEMA_strategy) %>% 
        na.omit()


perform$date <- as.Date(perform$date)
perform <- as.data.frame(perform)
perform <- as.xts(perform[, -1], order.by = perform[,1])


charts.PerformanceSummary(perform)
apply(perform, 2,  Return.cumulative)