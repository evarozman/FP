tabela2015 <- read.csv('hist_EURIBOR_2015.csv', row.names=1)
tabela2016 <- read.csv('hist_EURIBOR_2016.csv', row.names=1)
tabela2017 <- read.csv('hist_EURIBOR_2017.csv', row.names=1)

tabela2015.prvidnevi <- tabela2015[,c(1, 22, 42, 64, 84, 104, 126, 149, 170, 192, 214, 235)]
tabela2016.prvidnevi <- tabela2016[,c(1, 21, 42, 63, 84, 106, 128, 149, 172, 194, 215, 237)]
tabela2017.prvidnevi <- tabela2017[,c(1, 23, 43, 66, 84, 106, 128, 149, 172, 193, 215, 237)]

skupna.tabela <- cbind(tabela2015.prvidnevi, tabela2016.prvidnevi, tabela2017.prvidnevi)
skupna.tabela <- t(skupna.tabela)

casovna.vrsta6 <- ts(skupna.tabela[, c('6m')], start = c(2015, 1), frequency = 12)
casovna.vrsta12 <- ts(skupna.tabela[, c('12m')], start = c(2015, 1), frequency = 12)

graf <- ts.plot(casovna.vrsta6, casovna.vrsta12, xlab='Time', ylab='%', main='Euribor', col=c('red', 'blue'))
        legend("topright", legend=c("6m","12m"), lty=1, col=c("red","blue"))
