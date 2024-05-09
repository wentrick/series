pacman::p_load(tidyverse,readxl)

PIS <- read_excel("dados/PIS.xlsx")

ggplot(data = PIS, aes(x = mes, y = pis))+
  geom_line()

































