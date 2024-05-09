pacman::p_load(tidyverse,readxl,pracma)

dados_energia <- read_excel("dados/ConsumoEnergiaEAgua.xlsx") %>%
  select(-c(2,5:11)) 
  
#####
# Adicionando a Nova observacao

dados_energia = rbind(dados_energia, list("2024-04-01",419,33))

#####


ggplot(data = dados_energia, aes(x = mes, y = Energia))+
  geom_line()

dados_energia = dados_energia %>%
  mutate(consumo = Energia/Dias)
  

#########

rho = acf(dados_energia$consumo, lag = length(dados_energia$consumo), plot = FALSE)

plot(rho, main = " ")

#########

rho = acf(dados_energia$consumo, lag = 36, plot = FALSE)

plot(rho, main = " ")

#########

phi = pacf(dados_energia$consumo, lag = length(dados_energia$consumo), plot = FALSE)

plot(phi, main = " ")


#########

phi = pacf(dados_energia$consumo, lag = 36, plot = FALSE)

plot(phi, main = " ")

#########

n.size = length(dados_energia$consumo)

dados_energia$consumo.1 = c(NA, (dados_energia$consumo[1:(n.size-1)]))

(cbind(dados_energia$consumo,dados_energia$consumo.1))

dados_energia$diff = c(NA,diff(dados_energia$consumo))

dados_energia = dados_energia %>%
  mutate(consumo.1 = c(NA, (consumo[1:(n.size-1)])),
         diff = c(NA,diff(consumo)))

ggplot(data = dados_energia, aes(x = dados_energia$consumo.1, y = dados_energia$consumo))+
  geom_point()+
  geom_smooth(method='lm')

#########

ggplot(data = dados_energia, aes(x = mes, y = diff))+
  geom_line()

########

x = na.omit(dados_energia$diff)
rho = acf(x, lag = length(x), plot = FALSE)
plot(rho, main = " ")

#######

phi = pacf(x, lag = length(x),plot = FALSE)

plot(phi, main = " ")


####### Funcao de predicao original

x = na.omit(dados_energia$diff)
n.size = length(x)
n.training = ceiling(n.size/2)
observed = NULL
predicted = NULL

for (t in (n.training+1):n.size) {
  x.training = x[1:(t-1)]
  rho = acf(x.training,lag= (t-1), plot = FALSE)
  last.lag = length(rho$acf)
  Rho = rho$acf
  Omega = toeplitz(Rho[-last.lag])
  beta = inv(Omega) %*% Rho[-1]
  beta.0 = mean(x.training) * (1-sum(beta))
  predicted[t] = beta.0 + sum(rev(beta)*x.training[-1])
  observed[t] = x[t]
}

########### Funcao comentada

# Remover valores ausentes e preparar os dados
x = na.omit(dados_energia$diff) # Removendo os NA
n.size = length(x)  # Obtém o tamanho da amostra após a remoção de NA
n.training = ceiling(n.size/2)  # Calcula o tamanho do conjunto de treinamento (metade do tamanho da amostra arredondado para cima)
observed = NULL  # Inicializa a lista para armazenar as observações reais
predicted = NULL  # Inicializa a lista para armazenar as previsões

# Loop para previsão de observações
for (t in (n.training+1):n.size) {
  # Selecionar dados de treinamento até o ponto anterior ao ponto de teste atual
  x.training = x[1:(t-1)]
  
  # Calcular a função de autocorrelação até o lag correspondente ao ponto de teste
  rho = acf(x.training, lag = (t-1), plot = FALSE)
  last.lag = length(rho$acf)  # Obtém o índice do último valor na função de autocorrelação
  Rho = rho$acf  # Obtém a função de autocorrelação
  
  # Construir a matriz de covariância a partir da função de autocorrelação
  Omega = toeplitz(Rho[-last.lag])  # Constrói a matriz de Toeplitz a partir dos valores da função de autocorrelação
  beta = inv(Omega) %*% Rho[-1]  # Estima os coeficientes do modelo de previsão
  
  # Calcular o termo constante do modelo de previsão
  beta.0 = mean(x.training) * (1 - sum(beta))  # Calcula o termo constante do modelo
  
  # Fazer a previsão para o ponto de teste atual usando o modelo construído
  predicted[t] = beta.0 + sum(rev(beta) * x.training[-1])  # Calcula a previsão para o ponto de teste atual
  
  # Armazenar a observação real correspondente ao ponto de teste atual
  observed[t] = x[t]  # Armazena a observação real para fins de comparação posterior
}

########### funcao com comentarios mais breves

# Remover valores ausentes e preparar os dados
x = na.omit(dados_energia$diff) # Removendo os NA
n.size = length(x)
n.training = ceiling(n.size/2)
observed = NULL
predicted = NULL

# Loop para previsão de observações
for (t in (n.training+1):n.size) {
  # Selecionar dados de treinamento até o ponto anterior
  x.training = x[1:(t-1)]
  
  # Calcular a função de autocorrelação
  rho = acf(x.training, lag = (t-1), plot = FALSE)
  last.lag = length(rho$acf)
  Rho = rho$acf
  
  # Calcular a matriz de covariância e estimar os coeficientes
  Omega = toeplitz(Rho[-last.lag])
  beta = inv(Omega) %*% Rho[-1]
  
  # Calcular o termo constante do modelo
  beta.0 = mean(x.training) * (1 - sum(beta))
  
  # Fazer a previsão para o ponto de teste atual
  predicted[t] = beta.0 + sum(rev(beta) * x.training[-1])
  
  # Armazenar a observação correspondente
  observed[t] = x[t]
}


###########
dados_energia = dados_energia %>%
  mutate(observed = c(NA,observed),
         predicted = c(NA,predicted))

ggplot(data = dados_energia, aes(x = mes, y = diff))+
  geom_line(lwd = 1)+
  geom_line(aes(x = mes,y = predicted), color = "red",lwd =1)

########

cor(predicted,observed,use="complete.obs")


MAE = mean(abs(na.omit(predicted)-na.omit(observed)))

########

Y.hat = NULL

Y.t = NULL

for (h in (n.training+1):n.size) {
  Y.t[h+1] = dados_energia$consumo[h] + dados_energia$diff[h+1]
  Y.hat[h+1] = dados_energia$consumo[h] + dados_energia$predicted[h+1]
  
}

dados_energia = dados_energia %>%
  mutate(Y.hat = Y.hat,
         Y.t = Y.t)

ggplot(data = dados_energia, aes(x = mes, y = consumo))+
  geom_line(lwd = 1)+
  geom_line(aes(x = mes, y = Y.hat), color = "red", lwd =1)

########

MAE = mean(abs(na.omit(Y.hat)-na.omit(Y.t)))

MAPE = mean(abs(na.omit(Y.hat)/na.omit(Y.t)-1))

########



plot(na.omit(Y.hat),na.omit(Y.t))
cor(na.omit(Y.hat),na.omit(Y.t))

shapiro.test(na.omit(Y.t-Y.hat))

sd(Y.t-Y.hat,na.rm=T)

