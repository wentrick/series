pacman::p_load(tidyverse,extrafont,TSA,tseries,readxl,astsa)

dados_energia <- read_excel("dados/ConsumoEnergiaEAgua.xlsx") %>%
  select(-c(2,5:11)) 


dados_energia = dados_energia %>%
  mutate(consumo = dados_energia$Energia/dados_energia$Dias)



# Treinamento

n.size <- dim(dados_energia)[1]
t.size <- ceiling(3*n.size/4)
X.t <- dados_energia$consumo[1:t.size]

#determinacao da ordem do modelo da serie de treinamento

BIC <- NULL
grid <- 0:4
Grid <- 0:2
for (p.grid in grid) {
  BIC.col <- NULL
  for (q.grid in grid) {
    for (P.grid in Grid) {
      for (Q.grid in Grid) {
        draft <-sarima( X.t, p.grid, 1, q.grid,
                        P = P.grid,D=0,Q=Q.grid,S=12,
                        no.constant=TRUE, details=FALSE)
        BIC <- rbind( BIC, c(BIC = unlist(draft[4])[3],
                             p = p.grid, q = q.grid, P = P.grid, Q = Q.grid))
      }}}}



BIC <- data.frame(BIC)
BIC <- BIC[order(BIC$BIC.ICs.BIC),]
BIC[1:10,]


fit <- sarima(X.t,BIC[1,2],1,BIC[1,3],P=BIC[1,4],D=0,Q=BIC[1,5],S=12, no.constant=TRUE) 
fit

res = resid(fit$fit)


#plot 2 (nao sei pq o professor se deu esse trabalho todo sendo que da na mesma enfim)


Y.t <- res
fac <- acf(Y.t, lag=40, plot = FALSE)
Fig.2<- with(fac, data.frame(lag, acf))
Fig.2<- rbind(c(0,1), Fig.2)
conf.lim <- 2/sqrt(fac$n.used)

ggplot(data = Fig.2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0),linewidth=1.2)+
  ggtitle("FAC da série residual")+ 
  geom_hline(aes(yintercept = conf.lim), linetype="dotted",
             size = 2, color = "red") +
  geom_hline(aes(yintercept = -conf.lim), linetype="dotted",
             size = 2, color = "red")

#plot 2 (nao sei pq o professor se deu esse trabalho todo sendo que da na mesma enfim)


fac <- pacf(Y.t, lag=40, plot = FALSE)
Fig.2<- with(fac, data.frame(lag, acf))
Fig.2<- rbind(c(0,1), Fig.2)
conf.lim <- 2/sqrt(fac$n.used)

ggplot(data = Fig.2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0),linewidth=1.2)+
  ggtitle("FACP da série residual")+ 
  geom_hline(aes(yintercept = conf.lim), linetype="dotted",
             size = 2, color = "red") +
  geom_hline(aes(yintercept = -conf.lim), linetype="dotted",
             size = 2, color = "red")



# teste de normalidade dos resiiduos

shapiro.test(res)


#analise da evolucao hisotorica

energia$X.hat <- X.t - residuals
energia$X.hat[1:t.size] <- NA
Fig.energia + geom_line(data=energia,mapping = aes(x = mes, Y = X.hat), linewidth=1.2, col = "red")





















































