
## 1. naloga

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

graf.obrestne.mere <- ts.plot(casovna.vrsta6, casovna.vrsta12, xlab='Time', ylab='%',
                              main='Euribor', col=c('red', 'blue'))
                      legend("topright", legend=c("6m","12m"), lty=1, col=c("red","blue"), bty = "n")


## 2. naloga

drugi.jan.2015 <- skupna.tabela[1,]
prvi.jun.2016 <- skupna.tabela[18,]
prvi.dec.2017 <- skupna.tabela[36,]

cas <- c(0.25,0.5, 1, 2, 3, 6, 9, 12)

graf.casovne.strukture <- plot(cas, drugi.jan.2015, type="o", pch=16, col="yellow", main="Časovna struktura Euribor", 
                               ylim=c(-0.5,0.5), xlab="Dospetje [mesec]", ylab="%")
                          lines(cas, prvi.jun.2016, type="o", pch=16, col="blue")
                          lines(cas, prvi.dec.2017, type="o", pch=16, col="purple")

text(10.5,-0.3, "2.1.2015",col = "purple")
text(10.5,0.05, "1.6.2016",col="blue")
text(10.5,0.4, "1.12.2017", col="yellow")

# Vse tri krivulje  prikazujejo približno linearno rast višine obrestnih mer glede na dospetje (bolj
# kot je dospetje oddaljeno, višja je obrestna mera). 2.1.2015 in 1.6.2016 je imela obrestna mera
# za krajša dopetja približno enako vrednost, bolj strmo je začel naraščati graf za junij 2016 pri
# daljših dospetjih. 1.12.2017 je bila obrestna mera za 0.4% višja, hitrost naraščanja pa je bila
# zelo podobna tisti iz 1.6.2016.


## 3. naloga

napoved <- skupna.tabela[,c(6,8)]
colnames(napoved) <- c("Euribor6m", "Euribor12m")
napoved <- data.frame(napoved)
napoved$Euribor6m <- as.numeric(as.character(napoved$Euribor6m))
napoved$Euribor12m <- as.numeric(as.character(napoved$Euribor12m))

napoved <- transform(napoved, Napoved6m=as.numeric(((1+napoved$Euribor12m/100)/(1+0.5*(napoved$Euribor6m)/100)-1)*(100/0.5)))
napoved$Napoved6m <- as.numeric(as.character(napoved$Napoved6m))
napoved$Napoved6m <- c(NA, NA, NA, NA, NA, NA, napoved$Napoved6m[1:30])

razlika <- napoved[,c(1,3)]
leto.2015 <- razlika[1:12,]
leto.2016 <- razlika[13:24,]
leto.2017 <- razlika[25:36,]

crta <- lm(razlika$Napoved6m ~ razlika$Euribor6m)
graf.razlike <- plot(leto.2015, pch = 20, xlab = "Napoved", ylab = "Opazovano", col = "red",
                     main="6m Euribor 2015-2017", xlim=c(-0.4,0.6), ylim=c(-0.4,0.6))
                points(leto.2016, y = NULL, col = "29", pch = 20)
                points(leto.2017, y = NULL, col = "636", pch = 20)
                legend("topleft", c("2015", "2016", "2017"), col=c('red','29', '636'), 
                       pch=c(19, 19, 19), bty = "n")
                abline(crta) 
                abline(0, 1, lty = 'dashed')

graf.2015 <- plot(leto.2015, main = "6m Euribor 2015", xlab = "Napoved", ylab = "Opazovano", pch = 20,
              col = "red", xlim=c(-0.1,0.6), ylim=c(-0.1,0.6))
             abline(lm(razlika$Napoved6m[1:12] ~ razlika$Euribor6m[1:12]), col = "red")
             abline(0, 1, lty = 'dashed')

graf.2016 <- plot(leto.2016, main = "6m Euribor 2016", xlab = "Napoved", ylab = "Opazovano", pch = 20,
              col = "29", xlim=c(-0.3,0.4), ylim=c(-0.3,0.4))
             abline(lm(razlika$Napoved6m[13:24] ~ razlika$Euribor6m[13:24]), col = "29")
             abline(0, 1, lty = 'dashed')

graf.2017 <- plot(leto.2017, main = "6m Euribor 2017", xlab = "Napoved", ylab = "Opazovano", pch = 20,
              col = "636", xlim=c(-0.3,0.15), ylim=c(-0.3,0.15))
             abline(lm(razlika$Napoved6m[25:36] ~ razlika$Euribor6m[25:36]), col = "636")
             abline(0, 1, lty = 'dashed')

# Če bi hipoteza pričakovanj trga veljala, bi morale točke na grafih v nalogah (c) in (d) ležati
# na simetrali lihih kvadrantov. Empirični podatki te hipoteze ne potrjujejo.
