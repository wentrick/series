pacman::p_load(tidyverse,extrafont,TSA,tseries,readxl)

dados_energia <- read_excel("dados/ConsumoEnergiaEAgua.xlsx") %>%
  select(-c(2,5:11)) 


dados_energia = dados_energia %>%
  mutate(consumo = dados_energia$Energia/dados_energia$Dias)


#### plot


ggplot(data = dados_energia, aes(x = mes, y = consumo))+
  geom_line(linewidth=1.2)+
  ggtitle("Consumo kWh/dia")

#funcao de autocorrelacao amostral (2 plots iguais mas feito de forma diferente)

rho = acf(dados_energia$consumo, lag = 40, plot = FALSE)

plot(rho, main = " ")

#plot 2 (nao sei pq o professor se deu esse trabalho todo sendo que da na mesma enfim)
Y.t <- dados_energia$consumo
fac <-acf(Y.t, lag=40, plot = FALSE)
Fig.2<- with(fac, data.frame(lag, acf))
Fig.2<-rbind(c(0,1), Fig.2)

ggplot(data = Fig.2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0),linewidth=1.2)+
  ggtitle("FAC da série original")


### teste de raiz unitária de Dickey-Fuller

adf.test(Y.t,k=0)


# diferenciacao

X.t = diff(dados_energia$consumo)
dados_energia$dif = c(NA,X.t)


#plot dps da diferenciacao da variavel


ggplot(data = dados_energia, aes(x = mes, y = dif))+
  geom_line(linewidth=1.2)+
  ggtitle("Consumo kWh/dia")



### intervalo de confianca


conf.lim <- 2/sqrt(fac$n.used)

Y.t <- X.t
fac <-acf(Y.t, lag=40, plot = FALSE)
Fig.2<- with(fac, data.frame(lag, acf))
Fig.2<-rbind(c(0,1), Fig.2)

ggplot(data = Fig.2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0),linewidth=1.2)+
  ggtitle("FAC da série original")+ 
  geom_hline(aes(yintercept = conf.lim), linetype="dotted",
                     size = 2, color = "red") +
  geom_hline(aes(yintercept = -conf.lim), linetype="dotted",
             size = 2, color = "red")












