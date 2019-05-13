library(tidyverse)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)


getSymbols("^GSPC", from = "2000-01-01")

gspc <- GSPC[, 4] %>% tail(4800)

colnames(gspc) <- c("close")

gspc$sma20 <- SMA(gspc$close, n = 20)
gspc$ema20 <- EMA(gspc$close, n = 20)
gspc$sma15 <- SMA(gspc$close, n = 15)
gspc$ema15 <- EMA(gspc$close, n = 15)
gspc$dema_s <- DEMA(gspc$close, 14)
gspc$dema_l <- DEMA(gspc$close, 20)

gspc$sig20 <- ifelse(gspc$ema20 <= gspc$sma20, 0, 1)
gspc$sig15 <- ifelse(gspc$ema15 <= gspc$sma15, 0, 1)
gspc$sigdeam <- ifelse(gspc$dema_s <= gspc$dema_l, 0, 1)

gspc <- as.data.frame(gspc) %>% rownames_to_column(var = "date")

gspc$sp500 <- lead(ROC(gspc$close))
# gspc$sp500 <- ROC(gspc$close)

gspc$strategy_20 <- gspc$sig20 * gspc$sp500
gspc$strategy_15 <- gspc$sig15 * gspc$sp500
gspc$strategy_dema <- gspc$sigdeam * gspc$sp500


perform <- gspc[, c(1, 12:15)]
perform$date <- as.Date(perform$date)

perform <- na.omit(perform)

perform <- as.xts(perform[, -1], order.by = perform[,1])

# table.DownsideRisk(perform)
charts.PerformanceSummary(perform)
apply(perform, 2,  Return.cumulative)



# smá loop fjör

obs <- seq(200, 4800, 10)
avoxtun <- list()

for(i in obs) {
        gspc <- GSPC[, 4] %>% tail(i)
        
        colnames(gspc) <- c("close")
        
        gspc$sma20 <- SMA(gspc$close, n = 20)
        gspc$ema20 <- EMA(gspc$close, n = 20)
        gspc$sma15 <- SMA(gspc$close, n = 15)
        gspc$ema15 <- EMA(gspc$close, n = 15)
        gspc$dema_s <- DEMA(gspc$close, 12)
        gspc$dema_l <- DEMA(gspc$close, 20)
        
        gspc$sig20 <- ifelse(gspc$ema20 <= gspc$sma20, 0, 1)
        gspc$sig15 <- ifelse(gspc$ema15 <= gspc$sma15, 0, 1)
        gspc$sigdeam <- ifelse(gspc$dema_s <= gspc$dema_l, 0, 1)
        
        gspc$sp500 <- ROC(gspc$close)
        
        gspc$strategy_20 <- gspc$sig20 * gspc$sp500
        gspc$strategy_15 <- gspc$sig15 * gspc$sp500
        gspc$strategy_dema <- gspc$sigdeam * gspc$sp500
        
        
        perform <- gspc[, c(11:14)]
        
        perform <- na.omit(perform)
        
        # table.DownsideRisk(perform)
        # charts.PerformanceSummary(perform)
        avoxtun[[i/100]] <- apply(perform, 2,  Return.cumulative)
        
}

avoxtun <- do.call(rbind, avoxtun)

avoxtun <- avoxtun %>% as_tibble() %>% 
        mutate(buy_hold = case_when(sp500 > strategy_dema ~ "betra",
                                    TRUE ~ "verra"))