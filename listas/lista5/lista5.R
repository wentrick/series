pacman::p_load(astsa,extrafont,TSA,tseries,readxl,forecast,aTSA,MASS,lmtest,tidyverse,foreach,doParallel)
# Instalar e carregar pacotes necessários
#install.packages("readxl")      # Para ler arquivos Excel
#install.packages("ggplot2")     # Para plotagem
#install.packages("forecast")    # Para modelos ARIMA e plotagem de ACF e PACF
#install.packages("tseries")     # Para teste de Dickey-Fuller Aumentado

# Leitura dos dados
df <- read_excel("dados/ConsumoEnergiaEAgua.xlsx") %>%
  dplyr::select(-c(2,5:11)) 

# Renomear colunas
colnames(df) <- c('Ano', 'Energia', 'Dias', 'NA')
df <- df[, c('Ano', 'Energia', 'Dias')]  # Selecionar apenas as colunas de interesse

################################################################################
#QUESTAO 1
# Converter a coluna 'Ano' para o tipo Date
df$Ano <- as.Date(df$Ano, format='%Y-%m-%d')

# Calcular o Consumo Médio Diário
df$Consumo_Medio_Diario <- df$Energia / df$Dias

# Remover linhas com valores NA na coluna 'Consumo_Medio_Diario'
df <- df[!is.na(df$Consumo_Medio_Diario), ]

################################################################################
#QUESTAO 2
# Plotar a evolução temporal do Consumo Médio Diário
ggplot(df, aes(x = Ano, y = Consumo_Medio_Diario)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Evolução Temporal do Consumo Médio Diário de Energia",
       x = "Ano",
       y = "Consumo Médio Diário (kWh/dia)") +
  theme_minimal()

################################################################################
#QUESTAO 3
# Plotar a função de autocorrelação (ACF) e a função de autocorrelação parcial (PACF)
par(mfrow=c(2, 1))  # Configurar layout de 2 plots

acf(df$Consumo_Medio_Diario, lag.max = 50, main = "Função de Autocorrelação (FAC)")
pacf(df$Consumo_Medio_Diario, lag.max = 50, main = "Função de Autocorrelação Parcial (FACP)")

par(mfrow=c(1, 1))  # Resetar layout de plot

################################################################################
#QUESTAO 4
# Converter a série 'Consumo_Medio_Diario' para um objeto ts
y <- ts(df$Consumo_Medio_Diario, frequency=12)
y <- df$Consumo_Medio_Diario
# Aplicar o teste aumentado de Dickey-Fuller
adf_result <- adf.test(y)
print(adf_result)


# Criar funções harmônicas para modelagem sazonal
n <- length(y)
period <- 12
t <- 1:n

harmonics <- data.frame(
  sin1 = sin(2 * pi * t / period),
  cos1 = cos(2 * pi * t / period),
  sin2 = sin(4 * pi * t / period),
  cos2 = cos(4 * pi * t / period)
)

# Adicionar uma constante às funções harmônicas
X <- cbind(1, harmonics)

# Ajustar o modelo de regressão com funções harmônicas
harmonic_model <- lm(y ~ sin1 + cos1 + sin2 + cos2, data = harmonics)
summary(harmonic_model)

# Extrair os resíduos do modelo
residuals <- residuals(harmonic_model)

# Aplicar o teste aumentado de Dickey-Fuller aos resíduos
adf_result_residuals <- adf.test(residuals)
print(adf_result_residuals)

###############################################################################
#QUESTAO 5
# Calcular a diferença da série temporal
y_diff <- diff(y)

# Aplicar o teste aumentado de Dickey-Fuller à série diferenciada
adf_result_diff <- adf.test(y_diff)
print(adf_result_diff)

###############################################################################
#QUESTAO 6
# Plotar a série temporal original e a série diferenciada
par(mfrow=c(2, 1), mar=c(4, 4, 2, 1))  # Configurar layout do plot

plot(y, type='l', main='Série Temporal Original', ylab='Consumo Médio Diário', col='blue')
legend("topright", legend="Consumo Médio Diário", col="blue", lty=1)

plot(y_diff, type='l', main='Série Temporal Diferenciada', ylab='Consumo Médio Diário Diferenciado', col='red')
legend("topright", legend="Consumo Médio Diário Diferenciado", col="red", lty=1)

par(mfrow=c(1, 1))  # Resetar layout de plot

##################################################################################
#QUESTAO 7
# Criar a série temporal diferenciada
y_diff <- diff(y)

# Plotar as funções de autocorrelação (ACF) e autocorrelação parcial (PACF)
par(mfrow=c(2, 2))  # Configurar layout do plot

acf(y, lag.max=50, main='Função de Autocorrelação (FAC)')
pacf(y, lag.max=50, main='Função de Autocorrelação Parcial (FACP)')
acf(y_diff, lag.max=50, main='Função de Autocorrelação (FAC) para a série diferenciada')
pacf(y_diff, lag.max=50, main='Função de Autocorrelação Parcial (FACP) para a série diferenciada')

par(mfrow=c(1, 1))  # Resetar layout de plot

# Criar funções harmônicas para a série diferenciada
n_diff <- length(y_diff)
t_diff <- 1:n_diff

harmonics_diff <- data.frame(
  sin1 = sin(2 * pi * t_diff / period),
  cos1 = cos(2 * pi * t_diff / period),
  sin2 = sin(4 * pi * t_diff / period),
  cos2 = cos(4 * pi * t_diff / period)
)

# Adicionar uma constante às funções harmônicas
X_diff <- cbind(1, harmonics_diff)

# Ajustar o modelo de regressão com funções harmônicas para a série diferenciada
harmonic_model_diff <- lm(y_diff ~ sin1 + cos1 + sin2 + cos2, data = harmonics_diff)
summary(harmonic_model_diff)

# Extrair os resíduos do modelo
residuals_diff <- residuals(harmonic_model_diff)

# Aplicar o teste aumentado de Dickey-Fuller aos resíduos do modelo harmônico da série diferenciada
adf_result_residuals_diff <- adf.test(residuals_diff)
print(adf_result_residuals_diff)

# Plotar os resíduos do modelo harmônico
ggplot(data.frame(residuals_diff = residuals_diff), aes(x = 1:length(residuals_diff), y = residuals_diff)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Resíduos do Modelo Harmônico",
       x = "Tempo",
       y = "Resíduos") +
  theme_minimal()

###############################################################################
#QUESTAO 9
## Dividir os dados em treino e validação
train <- head(df$Consumo_Medio_Diario, round(0.8 * nrow(df)))
validation <- tail(df$Consumo_Medio_Diario, nrow(df) - length(train))
full_data <- c(train, validation)

# Definir as malhas de valores para os parâmetros p, d, q, P, D, Q, m
p <- c(0, 1, 2)
d <- c(1)
q <- c(0, 1, 2)
P <- c(0, 1, 2)
D <- c(1)
Q <- c(0, 1, 2)
m <- c(12)

# Criar combinações de pdq e PDQm
pdq_grid <- expand.grid(p=p, d=d, q=q)
PDQm_grid <- expand.grid(P=P, D=D, Q=Q, m=m)

# Função para calcular o BIC para cada combinação de pdq e PDQm
SARIMA_grid <- function(endog, pdq_grid, PDQm_grid) {
  model_info <- data.frame(order=character(), seasonal_order=character(), MAPE=numeric(), RMSE=numeric(), AIC=numeric(), BIC=numeric(), stringsAsFactors=FALSE)
  
  for (i in 1:nrow(pdq_grid)) {
    for (j in 1:nrow(PDQm_grid)) {
      tryCatch({
        fit <- Arima(endog, order=as.numeric(pdq_grid[i, 1:3]), seasonal=list(order=as.numeric(PDQm_grid[j, 1:3]), period=PDQm_grid[j, 4]))
        pred <- fitted(fit)
        
        MAPE <- mean(abs((endog - pred) / endog), na.rm=TRUE)
        RMSE <- sqrt(mean((endog - pred)^2, na.rm=TRUE))
        AIC <- fit$aic
        BIC <- BIC(fit)
        
        model_info <- rbind(model_info, data.frame(order=paste(as.numeric(pdq_grid[i, 1:3]), collapse=","),
                                                   seasonal_order=paste(as.numeric(PDQm_grid[j, 1:4]), collapse=","),
                                                   MAPE=MAPE, RMSE=RMSE, AIC=AIC, BIC=BIC, stringsAsFactors=FALSE))
      }, error=function(e) {})
    }
  }
  
  return(model_info)
}

# Medir o tempo de execução
start_time <- Sys.time()

# Calcular os modelos SARIMA
model_info <- SARIMA_grid(train, pdq_grid, PDQm_grid)

end_time <- Sys.time()
execution_time <- end_time - start_time
print(paste("Tempo necessário:", execution_time))

# Obter os 10 melhores modelos com base nos critérios MAPE, RMSE, AIC e BIC
least_MAPE <- model_info %>% arrange(MAPE) %>% head(10)
print("Melhores modelos por MAPE:")
print(least_MAPE)

least_RMSE <- model_info %>% arrange(RMSE) %>% head(10)
print("Melhores modelos por RMSE:")
print(least_RMSE)

least_AIC <- model_info %>% arrange(AIC) %>% head(10)
print("Melhores modelos por AIC:")
print(least_AIC)

least_BIC <- model_info %>% arrange(BIC) %>% head(10)
print("Melhores modelos por BIC:")
print(least_BIC)

################################################################################
#Questao 9
#Funcao paralelizada (7.7sec) enquanto a normal demora 21sec
# Dividir os dados em treino e validação
train <- head(df$Consumo_Medio_Diario, round(0.8 * nrow(df)))
validation <- tail(df$Consumo_Medio_Diario, nrow(df) - length(train))
full_data <- c(train, validation)

# Definir as malhas de valores para os parâmetros p, d, q, P, D, Q, m
p <- c(0, 1, 2)
d <- c(1)
q <- c(0, 1, 2)
P <- c(0, 1, 2)
D <- c(1)
Q <- c(0, 1, 2)
m <- c(12)

# Criar combinações de pdq e PDQm
pdq_grid <- expand.grid(p=p, d=d, q=q)
PDQm_grid <- expand.grid(P=P, D=D, Q=Q, m=m)

# Configurar paralelismo
cores <- detectCores() - 1
cl <- makeCluster(cores)
registerDoParallel(cl)

# Função para calcular o BIC para cada combinação de pdq e PDQm
SARIMA_grid <- function(endog, pdq_grid, PDQm_grid) {
  # Pré-alocar data frame
  model_info <- data.frame(order=character(), seasonal_order=character(), MAPE=numeric(), RMSE=numeric(), AIC=numeric(), BIC=numeric(), stringsAsFactors=FALSE)
  
  # Usar foreach para paralelismo
  results <- foreach(i = 1:nrow(pdq_grid), .combine = rbind, .packages = c('forecast')) %dopar% {
    local_model_info <- data.frame(order=character(), seasonal_order=character(), MAPE=numeric(), RMSE=numeric(), AIC=numeric(), BIC=numeric(), stringsAsFactors=FALSE)
    
    for (j in 1:nrow(PDQm_grid)) {
      try({
        fit <- Arima(endog, order=as.numeric(pdq_grid[i, 1:3]), seasonal=list(order=as.numeric(PDQm_grid[j, 1:3]), period=PDQm_grid[j, 4]))
        pred <- fitted(fit)
        
        MAPE <- mean(abs((endog - pred) / endog), na.rm=TRUE)
        RMSE <- sqrt(mean((endog - pred)^2, na.rm=TRUE))
        AIC <- fit$aic
        BIC <- BIC(fit)
        
        local_model_info <- rbind(local_model_info, data.frame(order=paste(as.numeric(pdq_grid[i, 1:3]), collapse=","),
                                                               seasonal_order=paste(as.numeric(PDQm_grid[j, 1:4]), collapse=","),
                                                               MAPE=MAPE, RMSE=RMSE, AIC=AIC, BIC=BIC, stringsAsFactors=FALSE))
      }, silent=TRUE)
    }
    
    return(local_model_info)
  }
  
  return(results)
}

# Medir o tempo de execução
start_time <- Sys.time()

# Calcular os modelos SARIMA
model_info <- SARIMA_grid(train, pdq_grid, PDQm_grid)

end_time <- Sys.time()
execution_time <- end_time - start_time
print(paste("Tempo necessário:", execution_time))

# Fechar cluster
stopCluster(cl)

# Obter os 10 melhores modelos com base nos critérios MAPE, RMSE, AIC e BIC
least_MAPE <- model_info %>% arrange(MAPE) %>% head(10)
print("Melhores modelos por MAPE:")
print(least_MAPE)

least_RMSE <- model_info %>% arrange(RMSE) %>% head(10)
print("Melhores modelos por RMSE:")
print(least_RMSE)

least_AIC <- model_info %>% arrange(AIC) %>% head(10)
print("Melhores modelos por AIC:")
print(least_AIC)

least_BIC <- model_info %>% arrange(BIC) %>% head(10)
print("Melhores modelos por BIC:")
print(least_BIC)

###############################################################################
#Questao 11.1
# Analisar as estimativas dos parâmetros do melhor modelo


# Seleciona os parâmetros do melhor modelo com base no menor BIC
best_model_params <- least_BIC[1,1]
best_model_params_seasonal = least_BIC[1,2]

order <- as.integer(unlist(strsplit(best_model_params, ",")))
best_seasonal_order <- as.integer(unlist(strsplit(best_model_params_seasonal, ",")))
seasonal_order = list(order = best_seasonal_order, period = m)

# Ajusta o modelo SARIMA com os melhores parâmetros
best_model <- sarima(train,  p = order[1], d = order[2], q = order[3] , seasonal = seasonal_order)

###############################################################################
#Questao 11.2
# Obtém os resíduos do melhor modelo
residuals <- residuals(best_model$fit)

# Plota a ACF e PACF dos resíduos
par(mfrow = c(2, 1))

acf(residuals, main = "ACF dos Residuos", lag.max = 20)
pacf(residuals, main = "PACF dos Residuos", lag.max = 20)

# Teste de Ljung-Box
ljung_box_result <- Box.test(residuals, lag = 10, type = "Ljung-Box", fitdf = length(order) - 1)

# Resultados para o décimo Lag
print("Resultados para o décimo Lag")
print(ljung_box_result)

lb_stat <- ljung_box_result$statistic
lb_pvalue <- ljung_box_result$p.value

# Conclusão
conclusion <- ifelse(lb_pvalue < 0.05,
                     "Há evidências de autocorrelação significativa nos resíduos no lag 10 (p < 0.05).",
                     "Não há evidências de autocorrelação significativa nos resíduos no lag 10 (p >= 0.05).")

cat("\nConclusão:\n", conclusion, "\n")

###############################################################################
#Questao 11.3

# Teste de Shapiro-Wilk para normalidade dos resíduos
shapiro_test <- shapiro.test(residuals)
shapiro_p_value <- shapiro_test$p.value

# Resultado do teste de Shapiro-Wilk
cat("Resultado do Teste de Shapiro-Wilk (p-value):", shapiro_p_value, "\n")

if (shapiro_p_value > 0.05) {
  cat("Os resíduos parecem ser normalmente distribuídos.\n")
} else {
  cat("Os resíduos não parecem ser normalmente distribuídos. Considere investigar mais.\n")
}

###############################################################################
# Questao 12.1
# Ajusta o modelo SARIMA na base de dados completa
fitted_model <- sarima(full_data, p = order[1], d = order[2], q = order[3] , seasonal = seasonal_order)

# Exibe o resumo do modelo ajustado
summary(fitted_model)

###############################################################################
# Questao 12.2
# Previsão na base de validação
forecast <- sarima.for(as.ts(full_data), n.ahead=length(validation), 
                       p=order[1], d=order[2], q=order[3], 
                       P=best_seasonal_order[1], D=best_seasonal_order[2], Q=best_seasonal_order[3], S=best_seasonal_order[4])

# Calcular os erros de previsão
mse <- mean((validation - forecast$pred)^2)
mae <- mean(abs(validation - forecast$pred))

cat("\nErros de Previsão um passo à frente na massa de validação:\n")
cat(sprintf("MSE: %f\n", mse))
cat(sprintf("MAE: %f\n", mae))

# Gráfico de comparação
plot_data <- data.frame(
  Data = time(train),
  Consumo = as.numeric(train),
  Tipo = rep("Treinamento", length(train))
)
validation_data <- data.frame(
  Data = time(validation)+length(train),
  Consumo = as.numeric(validation),
  Tipo = rep("Validacao", length(validation))
)
forecast_data <- data.frame(
  Data = time(validation)+length(train),
  Consumo = as.numeric(forecast$pred),
  Tipo = rep("Previsao", length(validation))
)
plot_data <- rbind(plot_data, validation_data, forecast_data)

ggplot(plot_data, aes(x=Data, y=Consumo, color=Tipo)) +
  geom_line() +
  labs(title="Previsao vs Dados de Validacao",
       x="Data",
       y="Consumo Medio Diario (kWh/dia)") +
  scale_color_manual(values=c("Treinamento"="black", "Validacao"="blue", "Previsao"="red")) +
  theme_minimal()

################################################################################
#Questao 12.4

forecast_mean <- forecast$pred

# Calcular o MAPE
mape <- mean(abs((validation - forecast_mean) / validation)) 
cat(sprintf("O MAPE (Erro Médio Percentual Absoluto) é: %.2f%%\n", mape))

if (mape < 10) {
  cat("Modelo muito bom.\n")
} else if (mape < 20) {
  cat("Bom modelo preditivo.\n")
} else if (mape < 50) {
  cat("Modelo razoável/aceitável.\n")
} else {
  cat("O modelo precisa ser melhorado.\n")
}

################################################################################
#QUESTAO 13

horizon <- 12

last_date <- tail(df$Ano,n=1)

# Extrair o último mês e ano das datas
last_month <- as.numeric(format(last_date, "%m"))
last_year <- as.numeric(format(last_date, "%Y"))

# Gerar índices de datas para os próximos 12 meses
date_index <- seq(as.Date(paste0(last_year, "-", last_month, "-01")), 
                  by = "month", length.out = horizon)

# Realizar a previsão usando o modelo ajustado
forecast <- sarima.for(as.ts(full_data), n.ahead = horizon, 
                       p = order[1], d = order[2], q = order[3], 
                       P = best_seasonal_order[1], D = best_seasonal_order[2], Q = best_seasonal_order[3], 
                       S = best_seasonal_order[4])

# Extrair valores previstos e intervalos de confiança
forecast_values <- forecast$pred
forecast_conf_int <- forecast$pred + cbind(-1.96 * forecast$se, 1.96 * forecast$se)

# Criar dataframe de previsões
forecast_df <- data.frame(
  Mês = date_index,
  Previsões = forecast_values,
  Limite_Inferior = forecast_conf_int[, 1],
  Limite_Superior = forecast_conf_int[, 2]
)

# Converter coluna de meses para o formato ano-mês
forecast_df$Mês <- format(forecast_df$Mes, "%Y-%m")

# Imprimir o dataframe de previsões
print(forecast_df)

























