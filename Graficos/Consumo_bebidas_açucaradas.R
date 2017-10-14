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

consumo_br = fread(input = 'C:/Users/User/Google Drive/UFRGS 2017/TCC/dados euromonitor/consumo_br.csv', header = TRUE)

plot_ly (x= consumo_br$V1, y= consumo_br$Refrigerantes, type = 'bar', name = 'Refrigerantes',  marker = list(color = 'rgb(158,202,225)',
                                                                                                             line = list(color = 'rgb(8,48,107)',
                                                                                                                         width = 1.5))) %>%
  add_trace(y = consumo_br$Concentradas, name = 'Concentrados', marker = list(color = 'rgb(32,210,187)',
                                                                               line = list(color = 'rgb(7,131,114)',
                                                                                           width = 1.5))) %>%
  add_trace(y = consumo_br$Sucos, name = 'Sucos', marker = list(color = 'rgb(242,203,242)',
                                                                line = list(color = 'rgb(185,7,185)',
                                                                            width = 1.5))) %>%
  add_trace(y= consumo_br$Chás, name = "Chás", marker = list(color = 'rgb(181,225,151)',
                                                             line = list(color = 'rgb(102,153,102)',
                                                                         width = 1.5))) %>%
  add_trace(y= consumo_br$`Esportivas e Energéticas`, name = 'Energéticos e Esportivas', marker = list(color = 'rgb(255,152,152)',
                                                                                                       line = list(color = 'rgb(239,36,36)',
                                                                                                                   width = 1.5))) %>%
  layout(yaxis = list(title = 'Consumo em litros per capita'), barmode = 'stack',  legend = list(orientation = "h",   # show entries horizontally
                                                                                       x = 5)     )
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


