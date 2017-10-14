##############################################
#########         PACOTES              #######
##############################################

# install.packages('plotly')
# install.packages ('dplyr')
# install.packages('data.table')
# install.packages('readr')

require(readr)       # Utilizado para leitura de csvs
require(data.table)  # Utilizado para manipulação de dados
require(dplyr)       # Utilizado para manipulação de dados
require(plotly)      # Utilizado para gerar gráficos


##################
#### GRÁFICOS ####
##################


### IMC PRÉ TAXAÇÃO

imc_selecionados = filter(table_imc, imc > 16 & imc < 40) # retirando disparidades para gr?fico

imc_arredondado = as.data.frame(table(round(imc_selecionados$imc)))

imc_arredondado$perc = (prop.table(imc_arredondado$Freq)*100)

colnames(imc_arredondado) = c("IMC", "Freq", "Percentual")

imc_arredondado$IMC = as.numeric(as.character(imc_arredondado$IMC))


### IMC PÓS TAXAÇÃO

imc_selecionados_2 = filter(table_imc_pos_intervencao, imc_pos_intervencao > 16 & imc_pos_intervencao < 40) # retirando disparidades para gr?fico

imc_arredondado_2 = as.data.frame(table(round(imc_selecionados_2$imc_pos_intervencao)))

imc_arredondado_2$Freq = as.numeric(as.character(imc_arredondado_2$Freq))

imc_arredondado_2$perc = (prop.table(imc_arredondado_2$Freq)*100)

colnames(imc_arredondado_2) = c("IMC", "Freq", "Percentual")

imc_arredondado_2$IMC = as.numeric(as.character(imc_arredondado_2$IMC))

a <- list( 
  x = 30,
  y = 0,
  text = "Obesidade",
  xref = "x",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 0,
  ay = -270
)

b <- list( 
  x = 25,
  y = 0,
  text = "Sobrepeso",
  xref = "x",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 0,
  ay = -380
)

plot_distribuicao_imc = plot_ly(x= imc_arredondado$IMC, y = imc_arredondado$Percentual, type = 'bar', name = "IMC antes da Taxa??o" , marker = list(color = '#FFC0D5',line = list(color = '#FF90B5', width = 1.5))) %>%
  add_trace(x= imc_arredondado_2$IMC, y = imc_arredondado_2$Percentual, type = 'bar', name = "IMC p?s Taxa??o", marker = list(color = '#DFE9FF', line = list(color = '#B2C8FC', width = 1.0))) %>%
  layout(yaxis = list(title = 'Percentual da popula??o adulta (%)', text = paste(imc_arredondado_2$Percentual, '%')), legend = list(orientation = 'h', x = 100, y = 8), xaxis = list(title = "IMC",  nticks = 6), annotations = list(a,b))
