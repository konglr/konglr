# 导入所需的包
library(quantmod)

# 下载ticker股票价格数据

ticker <-getSymbols("002077.SZ", auto.assign = FALSE)
ticker <- na.omit(ticker)
# 计算5日和50日的简单移动平均线（SMA）
sma5 <- SMA(Cl(ticker), n = 5)
sma50 <- SMA(Cl(ticker), n = 50)

# 初始化买入和卖出信号的变量
buy_signal <- rep(0, nrow(ticker))
sell_signal <- rep(0, nrow(ticker))

# 根据策略生成买入和卖出信号


for (i in 51:nrow(ticker)) {
  if (Cl(ticker[i]) > sma5[i] & as.numeric(Cl(ticker[i])) > as.numeric(Cl(ticker[i-1]))) {
    buy_signal[i] <- 1
  }
  
  if (Cl(ticker[i]) > sma5[i] * 1.1) {
    sell_signal[i] <- 1
  }
}


# 模拟交易
portfolio <- rep(0, nrow(ticker))
cash <- 100000  # 初始资金
shares_to_buy <- 100
shares <- 0    # 持有股票数量
orders_to_buy <- 0  #Date and share
orders_to_sell<- 0  #Date and share
price_increase_threshold <- 0.02
below_sma5 <- TRUE

for (i in 3000:nrow(ticker)) {
  
  if (Cl(ticker[i]) < sma5[i]) {
    below_sma5 <- FALSE
  }
  
  if (buy_signal[i] == 1 && !below_sma5) {
    if (shares == 0) { 
      shares <- shares + shares_to_buy
      cash <- cash - (shares_to_buy * as.numeric(Cl(ticker[i])))
      orders_to_buy <-  orders_to_buy+1
    } 
    else {
      if (as.numeric(Cl(ticker[i])) > (1 + price_increase_threshold) * as.numeric(Cl(ticker[i - 1]))) {
        shares <- shares + shares_to_buy
        cash <- cash - (shares_to_buy * as.numeric(Cl(ticker[i])))
        orders_to_buy <-  orders_to_buy+1
      }
    }
  }
  
  if (sell_signal[i] == 1) {
    cash_to_receive <- shares * as.numeric(Cl(ticker[i]))
    cash <- cash + cash_to_receive
    shares <- 0
    orders_to_sell <-  orders_to_sell+1
  }
  
  portfolio[i] <- cash + shares * as.numeric(Cl(ticker[i]))
}

tail(portfolio, n = 1)
cash
shares
orders_to_buy
orders_to_sell
paste("Return is", tail(portfolio, n=1)+cash-100000)

portfolio_df <- data.frame(Date = index(ticker), PortfolioValue = portfolio)

# 绘制回测结果
plot(portfolio_df[3000:nrow(portfolio_df),], type = "l", main = "Backtest Results", xlab = "Date", ylab = "Value")

