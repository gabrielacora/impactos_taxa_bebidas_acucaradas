##############################################
#########         PACOTES              #######
##############################################

# install.packages('plotly')
# install.packages ('dplyr')
# install.packages('data.table')
# install.packages('Hmisc')
# install.packages('vcd')
# install.packages("vcdExtra")
# install.packages("ca")
# install.packages("agricolae")
# install.packages("GPArotation")
# install.packages("mvtnorm")
# install.packages("MASS")
# install.packages("rprojroot")
# install.packages("formatR")
# install.packages("DT")
# install.packages("knitr", dependencies=TRUE)
# install.packages("dygraphs")
# install.packages("highcharter")
# install.packages("forecast")
# install.packages("viridisLite")
# install.packages("dplyr")
# install.packages("quantmod")
# install.packages("treemap")
# install.packages("leaflet")
# install.packages("maps")
# install.packages ("xts")

require(readr)       # Utilizado para leitura de csvs
require(data.table)  # Utilizado para manipulação de dados
require(dplyr)       # Utilizado para manipulação de dados
require(plotly)      # Utilizado para gerar gráficos
require(Hmisc)       # teste Wilcoxon
require(vcd)         # coeficiente de contingência C
require(vcdExtra)    # coeficiente de contingência C
require(ca)          # Análise de correspondência
require(agricolae)   # Testes complementação ANOVA
require(GPArotation) # PCA
require(mvtnorm)     # Manova 1 via
require(MASS)        # Análise Discriminante
require(rprojroot)   # Utilizado para encontrar o diretório onde está localizado o projeto R
require(formatR)     # Utilizado para padronizar código R
require(DT)          # Utilizado para formatação de tabelas
require(knitr)       # Utilizado para formatação de tabelas
require(dygraphs)    # Utilizado para gerar gráficos de série temporal
require(highcharter) # Utilizado para gerar gráficos
require(forecast)    # Utilizado para gerar previsões
require(viridisLite) # Utilizado para colorir mapas
require(dplyr)       # Utilizado para explorar e transformar dados
require(quantmod)    # Utilizado para modelagem quantitativa financeira 
require(treemap)     # Utilizado para mapas de árvore
require(leaflet)     # Utilizado para construção de mapas interativos
require(maps)        # Utilizado para visualização de mapas
require(xts)         # Utilizado para a geração de séries temporais


######################################################
#### Gráfico consumo bebidas açucaradas no Brasil ####
######################################################

consumo_br <- read_delim("C:/Users/User/consumo_soft-drinks.csv", 
                                      ";", escape_double = FALSE, col_types = cols(Category = col_date(format = "%Y")))

consumo_br = data.frame(consumo_br$Category, consumo_br$`Soft Drinks`, consumo_br$`   Carbonates`, consumo_br$`   Concentrates`, consumo_br$`   RTD Tea`, consumo_br$`   Sports and Energy Drinks`)

colnames(consumo_br) = c('ano', 'bebidas açucaradas', 'refigerantes', 'sucos concentrados', 'chás açucarados', 'Bebidas esportivas e energeticos')

serie_consumo <- xts(consumo_br[,-1], order.by=as.Date(consumo_br[,1], "%Y"))

total = serie_consumo[,1]
refigerentes = serie_consumo[,2]
sucos_concentrados = serie_consumo[,3]
chas = serie_consumo[,4]
bebidas_esportivas = serie_consumo[,5]

plot_ly(x= consumo_br$ano, y = consumo_br$`sucos concentrados`, type = 'bar', name = 'sucos concentrados') %>%
  add_trace(y = consumo_br$refigerantes, name = 'refrigerantes', colors = "BuPu") %>%
  add_trace(y = consumo_br$`Bebidas esportivas e energeticos`, name = 'bebidas esportivas e energéticas') %>%
  add_trace(y = consumo_br$`chás açucarados`, name = 'chas') %>%
  layout(yaxis = list(title = 'Litros consumidos per capita'), barmode = 'stack') %>%
  layout(legend = list(orientation = 'h'))

###########################################
#####   Grafico consumo mundial ssb #######
###########################################

consumo_ssb = fread(input = "C:/Users/User/consumo_ssb.csv", header = TRUE)
colnames(consumo_ssb) = c('Ano', 'Ásia Pacífico', 'Australásia', 'América Latina', 'Europa Oriental', 'Oriente Médio e África' ,'Europa Ocidental', 'América do Norte')

plot_ly(x = consumo_ssb$Ano,y=  consumo_ssb$Australásia, type  = 'scatter', size = consumo_ssb$Australásia, name = 'Australásia') %>%
  add_trace(y = consumo_ssb$`América Latina`, name = 'América Latina', size = consumo_ssb$`América Latina`) %>%
  add_trace(y = consumo_ssb$`Europa Oriental``, name = 'Europa Oriental', size = consumo_ssb$`Europa Oriental`) %>%
  add_trace(y = consumo_ssb$`Oriente Médio e África`, name = 'Oriente Médio e África', size = consumo_ssb$`Oriente Médio e África`) %>%
  add_trace(y = consumo_ssb$`Europa Ocidental`, name = 'Europa Ociental', size = consumo_ssb$`Europa Ocidental`) %>%
  add_trace(y= consumo_ssb$`Amárica do Norte`, name = 'América do Norte', size = consumo_ssb$`América do Norte`) %>%
  add_trace(y = consumo_ssb$`Ásia Pacpífico`, name = 'Ásia Pacífico', size = consumo_ssb$`Ásia Pacífico` )


