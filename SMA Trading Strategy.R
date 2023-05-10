# 导入所需的包
library(quantmod)

# 下载AAPL股票价格数据
getSymbols("AAPL")

# 计算5日和50日的简单移动平均线（SMA）
sma5 <- SMA(Cl(AAPL), n = 5)
sma50 <- SMA(Cl(AAPL), n = 50)

# 初始化买入和卖出信号的变量
buy_signal <- rep(0, nrow(AAPL))
sell_signal <- rep(0, nrow(AAPL))

# 根据策略生成买入和卖出信号
for (i in 51:nrow(AAPL)) {
  if (Cl(AAPL[i]) > sma5[i] && as.numeric(Cl(AAPL[i])) > as.numeric(Cl(AAPL[i-1]))) {
    buy_signal[i] <- 1
  }
  
  if (Cl(AAPL[i]) > sma5[i] * 1.2) {
    sell_signal[i] <- 1
  }
}


# 模拟交易
portfolio <- rep(0, nrow(AAPL))
cash <- 100000  # 初始资金
shares_to_buy <- 100
shares <- 0    # 持有股票数量
price_increase_threshold <- 0.05
below_sma5 <- FALSE

for (i in 52:nrow(AAPL)) {
  if (buy_signal[i] == 1 && !below_sma5) {
    if (as.numeric(Cl(AAPL[i])) > (1 + price_increase_threshold) * as.numeric(Cl(AAPL[i - 1]))) {
      shares_to_buy <- cash / Cl(AAPL[i])
      shares <- shares + shares_to_buy
      cash <- cash - (shares_to_buy * Cl(AAPL[i]))
    }
  }
  
  if (sell_signal[i] == 1) {
    cash_to_receive <- shares * Cl(AAPL[i])
    cash <- cash + cash_to_receive
    shares <- 0
    below_sma5 <- TRUE
  }
  
  if (Cl(AAPL[i]) < sma5[i]) {
    below_sma5 <- FALSE
  }
  
  portfolio[i] <- cash + (shares * Cl(AAPL[i]))
}

# 绘制回测结果
plot(portfolio, type = "l", main = "股票策略回测结果", xlab = "日期", ylab = "投资组合价值")
