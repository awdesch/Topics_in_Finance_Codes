library(quantmod)

getFinancials("AAPL")
viewFin(AAPL.f)

tickers = c("AAPL","GOOG", "MSFT", "XOM", "GE", "KO", "F", "GS", "AIG", "HPQ")
for(i in 1:length(tickers)){
  getFinancials(tickers[i])
}
viewFin(AAPL.f)
viewFin(MSFT.f)
