pacman::p_load(tidyverse,readxl,pracma,readr,tsibble,feasts,tibbletime,zoo,forecast)

# Instalar e carregar pacotes necessários
# Para manipulação de dados e gráficos
#install.packages("tidyverse")  # Para manipulação de dados e gráficos
#install.packages("tsibble")    # Para trabalhar com séries temporais
#install.packages("feasts")     # Para decomposição de séries temporais
#install.packages("tibbletime") # Para manipulação de séries temporais em tibbles


# Leitura dos dados
data <- read_csv("dados/ALGONQUIN_PARK_Ontario_Canada.csv")


# Seleção das colunas de interesse
columns_of_interest <- c(
  "Mean Max Temp (°C)", "Mean Min Temp (°C)", "Mean Temp (°C)",
  "Extr Max Temp (°C)", "Extr Min Temp (°C)", "Total Rain (mm)",
  "Total Snow (cm)", "Total Precip (mm)"
)

# Contagem de valores nulos
null_counts <- sapply(data[columns_of_interest], function(x) sum(is.na(x)))
print(null_counts)

# Estatísticas descritivas
descriptive_stats <- data[columns_of_interest] %>% 
  summarise(across(everything(), list(
    count = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    std = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    q25 = ~quantile(., 0.25, na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    q75 = ~quantile(., 0.75, na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  )))
print(descriptive_stats)

# Função para decomposição sazonal e plotagem
plot_series_with_decomposition <- function(series, title, window=12) {
  series_ts <- ts(series, frequency=12)  # Converter série para objeto de série temporal
  series_rolling <- rollmean(series, k=window, fill=NA, align="center")  # Calcular média móvel
  decomposition <- stl(series_ts, s.window="periodic")  # Decompor série temporal
  
  par(mfrow=c(4, 1), mar=c(4, 4, 2, 1))  # Configurar layout do plot
  
  # Plotar série original e média móvel
  plot(series_ts, main=paste(title, "- Série Temporal"), col="black")
  lines(series_rolling, col="red")
  legend("topright", legend=c("Original", "Média Móvel"), col=c("black", "red"), lty=1)
  
  # Plotar tendência
  plot(decomposition$time.series[, "trend"], main=paste(title, "- Tendência"), col="blue")
  
  # Plotar sazonalidade
  plot(decomposition$time.series[, "seasonal"], main=paste(title, "- Sazonalidade"), col="green")
  
  # Plotar resíduos
  plot(decomposition$time.series[, "remainder"], main=paste(title, "- Resíduos"), col="purple")
  
  par(mfrow=c(1, 1))  # Resetar layout do plot
}

# Aplicar a função para cada coluna de interesse
for (column in columns_of_interest) {
  if (all(is.na(data[[column]]))) {
    next  # Pular colunas que estão completamente vazias
  }
  plot_series_with_decomposition(na.omit(data[[column]]), column)
}