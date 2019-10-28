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
