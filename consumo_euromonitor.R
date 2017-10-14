##### PREPARANDO O AMBIENTE DE TRABALHO ######

# install.packages("readr")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("zoo")
# install.packages("plotly)

library(readr)
library(dplyr)
library(data.table)
library(zoo)
library(plotly)


#### Importando dados de consumo e de calorias #####

# dados de consumo off-trade 

consumo_soft_drinks = fread('C:/Users/User/Google Drive/UFRGS 2017/TCC/dados euromonitor/consumo_bebidas.csv')

str(consumo_soft_drinks)

# improtanto base de calorias de cada bebida

calorias_soft_drinks = fread("C:/Users/User/Google Drive/UFRGS 2017/TCC/dados euromonitor/calorias_bebidas.csv")

str(calorias_soft_drinks)


#### Unindo dados ####

consumo = merge(consumo_soft_drinks, calorias_soft_drinks, by = 'Bebida')


# Retirando o total de refrigerantes (pois j? temos o desagregado)

consumo = consumo [-4,]


#################
### CALCULOS ###
################

# determinando o total de calorias ingeridas, di?rias e anualmente per capita

consumo$consumo_calorico = consumo$Cosumo * consumo$Calorias

consumo$consumo_cal_diario = (consumo$Cosumo*consumo$Calorias)/365

sum(consumo$consumo_cal_diario)  #total de calorias ingeridas ao dia


### C?lculo da varia??o do consumo p?s introdu??o de taxa

# elasticidades 

elasticidade_refrigerantes = -0.61

elasticidade_demais = -1.32

elasticidade = c(-1.32, -1.32, -1.32, -1.32, -0.61, -0.61, -0.61, -0.61, -0.61)
  
consumo$elasticidade = elasticidade #unindo a base de dados pre-existente

# supondo repasse total de 20% para produtos

consumo$variacao = (0.20 * elasticidade)

consumo$novo_consumo = consumo$Cosumo * (consumo$variacao + 1 ) # consumo p?s taxa??o

consumo$novo_consumo_calorico = consumo$novo_consumo * consumo$Calorias #consumo cal?rico p?s taxa??o

consumo$novo_consumo_cal_diario = (consumo$novo_consumo*consumo$Calorias)/365 #consumo cal?rico di?rio p?s taxa??o

sum(consumo$novo_consumo_cal_diario) #consumo cal?rico di?rio p?s taxa??o

### Estimando a varia??o de peso a partir da varia??o cal?rica

# estimando as calorias l?quidas salvas, assumindo grau de substitui??o cal?rica

consumo$calorias_salvas_totais = consumo$consumo_calorico - consumo$novo_consumo_calorico

consumo$calorias_salvas_totais_diar = consumo$consumo_cal_diario - consumo$novo_consumo_cal_diario

consumo$calorias_liquidas_salvas_diar = consumo$calorias_salvas_totais_diar * 0.67

consumo$calorias_liquidas_salvas = consumo$calorias_salvas_totais * 0.67

total_salvo = sum(consumo$calorias_liquidas_salvas) # total da redu??o de calorias reduzidas em um ano per capita

total_salvo_diar = sum(consumo$calorias_liquidas_salvas_diar) #total da redu??o cal?rica por dia per capita

# Assumindo que 3.500 kgs altera 0,455 gramas de massa

reducao_peso = (total_salvo*0.453)/3500

##### CALCULO DO IMC PR? TAXA

# Importando microdados POF 2008-2009 para c?lculo do IMC

load("C:/Users/User/Documents/Taxa_Bedidas_a�ucaradas/Dados/2009/t_morador_s.rda")

# transformando dados 

dados = as.data.frame(t_morador_s)

dados_1 = filter(dados, idade_anos > 18) #selecionando apenas os adultos

dados_2 = data.frame (dados_1$peso_imputado, dados_1$altura_imputado, dados_1$cod_sexo)

colnames(dados_2) = c ("peso", "altura", "sexo")

dados_2 = dados_2 %>% dplyr::filter (peso > 20) # retirando dados errados ou outliers

imc = dados_2$peso/((dados_2$altura/100)^2) #calculo do IMC

table_imc = as.data.frame(table(imc))

table_imc$imc = as.numeric(as.character(table_imc$imc))

table_imc$perc = (prop.table(table_imc$Freq))*100

table_imc$freq = as.numeric(table_imc$Freq)

obes_pre_taxa = table_imc %>% filter(imc >= 30) #filtrando apenas os obesos

sum(obes_pre_taxa$perc) # percentual de obesos na pooopula??o
 
sobrepeso_pre_taxa = table_imc %>% filter(imc >= 25) #filtrando sobrepeso

sum(sobrepeso_pre_taxa$perc) #percentual da popula??o com sobrepeso



#### CALCULANDO ALTERA??O NO IMC P?S INTERVEN??O

dados_3 = dados_2

dados_3$peso_novo = dados_3$peso - reducao_peso #calculando novo peso ap?s taxa

imc_pos_intervencao = dados_3$peso_novo/((dados_2$altura/100)^2) # calculando novo imc

table_imc_pos_intervencao = as.data.frame(table(imc_pos_intervencao))

table_imc_pos_intervencao$imc_pos_intervencao = as.numeric(as.character(table_imc_pos_intervencao$imc_pos_intervencao))

table_imc_pos_intervencao$perc = (prop.table(table_imc_pos_intervencao$Freq))*100

table_imc$freq = as.numeric(table_imc$Freq)

total_obesidade_pos_imp = table_imc_pos_intervencao %>% filter(imc_pos_intervencao >= 30) # selecionando apenas indiv?duos obesos

sum(total_obesidade_pos_imp$perc) #percentual da popula??o obesa

total_sobreso_pos_imp = table_imc_pos_intervencao %>% filter(imc_pos_intervencao >= 25) # selecionando indiv?duos acima do peso

sum(total_sobreso_pos_imp$perc) # percentual da popula??o acima do peso


# variação

var_obes = sum(obes_pre_taxa$perc) - sum(total_obesidade_pos_imp$perc)

var_sobrepeso = sum(sobrepeso_pre_taxa$perc) - sum(total_sobreso_pos_imp$perc)

### GR?FICOS

# imc pr? taxa

imc_selecionados = filter(table_imc, imc > 16 & imc < 40) # retirando disparidades para gr?fico

imc_arredondado = as.data.frame(table(round(imc_selecionados$imc)))

imc_arredondado$perc = (prop.table(imc_arredondado$Freq)*100)

colnames(imc_arredondado) = c("IMC", "Freq", "Percentual")

imc_arredondado$IMC = as.numeric(as.character(imc_arredondado$IMC))


# imc p?s taxa

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

  
#################################################################
############# AN?LISE DE SENSIBILIDADE ##########################
################################################################

###########################################
########## VARIA??O NO REPASSE ############



# 1. supondo repasse total de 110% para produtos (22%)

consumo$variacao110 = (0.22 * elasticidade)

consumo$novo_consumo110 = consumo$Cosumo * (consumo$variacao110 + 1 ) # consumo p?s taxa??o

consumo$novo_consumo_calorico110 = consumo$novo_consumo110 * consumo$Calorias #consumo cal?rico p?s taxa??o

consumo$novo_consumo_cal_diario110 = (consumo$novo_consumo110*consumo$Calorias)/365 #consumo cal?rico di?rio p?s taxa??o

sum(consumo$novo_consumo_cal_diario110) #consumo cal?rico di?rio p?s taxa??o

### Estimando a varia??o de peso a partir da varia??o cal?rica

# estimando as calorias l?quidas salvas, assumindo grau de substitui??o cal?rica

consumo$calorias_salvas_totais110 = consumo$consumo_calorico - consumo$novo_consumo_calorico110

consumo$calorias_salvas_totais_diar110 = consumo$consumo_cal_diario - consumo$novo_consumo_cal_diario110

consumo$calorias_liquidas_salvas_diar110 = consumo$calorias_salvas_totais_diar110 * 0.67

consumo$calorias_liquidas_salvas110 = consumo$calorias_salvas_totais110 * 0.67

total_salvo110 = sum(consumo$calorias_liquidas_salvas110) # total da redu??o de calorias reduzidas em um ano per capita

total_salvo_diar110 = sum(consumo$calorias_liquidas_salvas_diar110) #total da redu??o cal?rica por dia per capita

# Assumindo que 3.500 kgs altera 0,455 gramas de massa

reducao_peso110 = (total_salvo110*0.453)/3500

##### CALCULO DO IMC PR? TAXA

sum(obes_pre_taxa$perc) # percentual de obesos na pooopula??o

sobrepeso_pre_taxa = table_imc %>% filter(imc >= 25) #filtrando sobrepeso

sum(sobrepeso_pre_taxa$perc) #percentual da popula??o com sobrepeso


#### CALCULANDO ALTERA??O NO IMC P?S INTERVEN??O

dados_3 = dados_2

dados_3$peso_novo110 = dados_3$peso - reducao_peso110 #calculando novo peso ap?s taxa

imc_pos_intervencao110 = dados_3$peso_novo110/((dados_2$altura/100)^2) # calculando novo imc

table_imc_pos_intervencao110 = as.data.frame(table(imc_pos_intervencao110))

table_imc_pos_intervencao110$imc_pos_intervencao110 = as.numeric(as.character(table_imc_pos_intervencao110$imc_pos_intervencao110))

table_imc_pos_intervencao110$perc = (prop.table(table_imc_pos_intervencao110$Freq))*100

table_imc$freq = as.numeric(table_imc$Freq)

total_obesidade_pos_imp110 = table_imc_pos_intervencao110 %>% filter(imc_pos_intervencao110 >= 30) # selecionando apenas indiv?duos obesos

sum(total_obesidade_pos_imp110$perc) #percentual da popula??o obesa

total_sobreso_pos_imp110 = table_imc_pos_intervencao110 %>% filter(imc_pos_intervencao110 >= 25) # selecionando indiv?duos acima do peso

sum(total_sobreso_pos_imp110$perc) # percentual da popula??o acima do peso



var_obes = sum(obes_pre_taxa$perc) - sum(total_obesidade_pos_imp110$perc)

var_sobrepeso = sum(sobrepeso_pre_taxa$perc) - sum(total_sobreso_pos_imp110$perc)


### GR?FICOS

# imc pr? taxa

imc_selecionados = filter(table_imc, imc > 16 & imc < 40) # retirando disparidades para gr?fico

imc_arredondado = as.data.frame(table(round(imc_selecionados$imc)))

imc_arredondado$perc = (prop.table(imc_arredondado$Freq)*100)

colnames(imc_arredondado) = c("IMC", "Freq", "Percentual")

imc_arredondado$IMC = as.numeric(as.character(imc_arredondado$IMC))


# imc p?s taxa

imc_selecionados_110 = filter(table_imc_pos_intervencao110, imc_pos_intervencao110 > 16 & imc_pos_intervencao110 < 40) # retirando disparidades para gr?fico

imc_arredondado_110 = as.data.frame(table(round(imc_selecionados_110$imc_pos_intervencao110)))

imc_arredondado_110$Freq = as.numeric(as.character(imc_arredondado_110$Freq))

imc_arredondado_110$perc = (prop.table(imc_arredondado_110$Freq)*100)

colnames(imc_arredondado_110) = c("IMC", "Freq", "Percentual")

imc_arredondado_110$IMC = as.numeric(as.character(imc_arredondado_110$IMC))



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

plot_distribuicao_imc_110 = plot_ly(x= imc_arredondado$IMC, y = imc_arredondado$Percentual, type = 'bar', name = "IMC antes da Taxa??o" , marker = list(color = '#FFC0D5',line = list(color = '#FF90B5', width = 1.5))) %>%
  add_trace(x= imc_arredondado_110$IMC, y = imc_arredondado_110$Percentual, type = 'bar', name = "IMC p?s Taxa??o", marker = list(color = '#DFE9FF', line = list(color = '#B2C8FC', width = 1.0))) %>%
  layout(yaxis = list(title = 'Percentual da popula??o adulta (%)', text = paste(imc_arredondado_2$Percentual, '%')), legend = list(orientation = 'h', x = 100, y = 8), xaxis = list(title = "IMC",  nticks = 6), annotations = list(a,b))




# 2. Supondo repasse de apenas 90% dos pessoas (varia??o de 18%)


consumo$variacao90 = (0.18 * elasticidade)

consumo$novo_consumo90 = consumo$Cosumo * (consumo$variacao90 + 1 ) # consumo p?s taxa??o

consumo$novo_consumo_calorico90 = consumo$novo_consumo90 * consumo$Calorias #consumo cal?rico p?s taxa??o

consumo$novo_consumo_cal_diario90 = (consumo$novo_consumo90*consumo$Calorias)/365 #consumo cal?rico di?rio p?s taxa??o

sum(consumo$novo_consumo_cal_diario90) #consumo cal?rico di?rio p?s taxa??o

### Estimando a varia??o de peso a partir da varia??o cal?rica

# estimando as calorias l?quidas salvas, assumindo grau de substitui??o cal?rica

consumo$calorias_salvas_totais90 = consumo$consumo_calorico - consumo$novo_consumo_calorico90

consumo$calorias_salvas_totais_diar90 = consumo$consumo_cal_diario - consumo$novo_consumo_cal_diario90

consumo$calorias_liquidas_salvas_diar90 = consumo$calorias_salvas_totais_diar90 * 0.67

consumo$calorias_liquidas_salvas90 = consumo$calorias_salvas_totais90 * 0.67

total_salvo90 = sum(consumo$calorias_liquidas_salvas90) # total da redu??o de calorias reduzidas em um ano per capita

total_salvo_diar90 = sum(consumo$calorias_liquidas_salvas_diar90) #total da redu??o cal?rica por dia per capita

# Assumindo que 3.500 kgs altera 0,455 gramas de massa

reducao_peso90 = (total_salvo90*0.453)/3500

##### CALCULO DO IMC PR? TAXA

sum(obes_pre_taxa$perc) # percentual de obesos na pooopula??o

sobrepeso_pre_taxa = table_imc %>% filter(imc >= 25) #filtrando sobrepeso

sum(sobrepeso_pre_taxa$perc) #percentual da popula??o com sobrepeso


#### CALCULANDO ALTERA??O NO IMC P?S INTERVEN??O

dados_3 = dados_2

dados_3$peso_novo90 = dados_3$peso - reducao_peso90 #calculando novo peso ap?s taxa

imc_pos_intervencao90 = dados_3$peso_novo90/((dados_2$altura/100)^2) # calculando novo imc

table_imc_pos_intervencao90 = as.data.frame(table(imc_pos_intervencao90))

table_imc_pos_intervencao90$imc_pos_intervencao90 = as.numeric(as.character(table_imc_pos_intervencao90$imc_pos_intervencao90))

table_imc_pos_intervencao90$perc = (prop.table(table_imc_pos_intervencao90$Freq))*100

table_imc$freq = as.numeric(table_imc$Freq)

total_obesidade_pos_imp90 = table_imc_pos_intervencao90 %>% filter(imc_pos_intervencao90 >= 30) # selecionando apenas indiv?duos obesos

sum(total_obesidade_pos_imp90$perc) #percentual da popula??o obesa

total_sobreso_pos_imp90 = table_imc_pos_intervencao90 %>% filter(imc_pos_intervencao90 >= 25) # selecionando indiv?duos acima do peso

sum(total_sobreso_pos_imp90$perc) # percentual da popula??o acima do peso


var_obes = sum(obes_pre_taxa$perc) - sum(total_obesidade_pos_imp90$perc)

var_sobrepeso = sum(sobrepeso_pre_taxa$perc) - sum(total_sobreso_pos_imp90$perc)



### GR?FICOS

# imc pr? taxa

imc_selecionados = filter(table_imc, imc > 16 & imc < 40) # retirando disparidades para gr?fico

imc_arredondado = as.data.frame(table(round(imc_selecionados$imc)))

imc_arredondado$perc = (prop.table(imc_arredondado$Freq)*100)

colnames(imc_arredondado) = c("IMC", "Freq", "Percentual")

imc_arredondado$IMC = as.numeric(as.character(imc_arredondado$IMC))


# imc p?s taxa

imc_selecionados_90 = filter(table_imc_pos_intervencao90, imc_pos_intervencao90 > 16 & imc_pos_intervencao90 < 40) # retirando disparidades para gr?fico

imc_arredondado_90 = as.data.frame(table(round(imc_selecionados_90$imc_pos_intervencao90)))

imc_arredondado_90$Freq = as.numeric(as.character(imc_arredondado_90$Freq))

imc_arredondado_90$perc = (prop.table(imc_arredondado_90$Freq)*100)

colnames(imc_arredondado_90) = c("IMC", "Freq", "Percentual")

imc_arredondado_90$IMC = as.numeric(as.character(imc_arredondado_90$IMC))



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

plot_distribuicao_imc_90 = plot_ly(x= imc_arredondado$IMC, y = imc_arredondado$Percentual, type = 'bar', name = "IMC antes da Taxa??o" , marker = list(color = '#FFC0D5',line = list(color = '#FF90B5', width = 1.5))) %>%
  add_trace(x= imc_arredondado_90$IMC, y = imc_arredondado_90$Percentual, type = 'bar', name = "IMC p?s Taxa??o", marker = list(color = '#DFE9FF', line = list(color = '#B2C8FC', width = 1.0))) %>%
  layout(yaxis = list(title = 'Percentual da popula??o adulta (%)', text = paste(imc_arredondado_2$Percentual, '%')), legend = list(orientation = 'h', x = 100, y = 8), xaxis = list(title = "IMC",  nticks = 6), annotations = list(a,b))


##################################################
########### SUBSTITUI??O CAL?RICA ################

# 1. Supondo que n?o h? substitui??o cal?rica

### Estimando a varia??o de peso a partir da varia??o cal?rica

# estimando as calorias l?quidas salvas, assumindo que n?o h? substitui??o cal?rica

consumo$calorias_salvas_totais = consumo$consumo_calorico - consumo$novo_consumo_calorico

consumo$calorias_salvas_totais_diar = consumo$consumo_cal_diario - consumo$novo_consumo_cal_diario

consumo$calorias_liquidas_salvas_diar_sem_subst_cal = consumo$calorias_salvas_totais_diar

consumo$calorias_liquidas_salvas_sem_subst_cal = consumo$calorias_salvas_totais

total_salvo_sem_subst_cal = sum(consumo$calorias_liquidas_salvas_sem_subst_cal) # total da redu??o de calorias reduzidas em um ano per capita

total_salvo_diar_sem_subst_cal = sum(consumo$calorias_liquidas_salvas_diar_sem_subst_cal) #total da redu??o cal?rica por dia per capita

# Assumindo que 3.500 kgs altera 0,455 gramas de massa

reducao_peso_sem_subst_cal = (total_salvo_sem_subst_cal*0.453)/3500

##### CALCULO DO IMC PR? TAXA

sum(obes_pre_taxa$perc) # percentual de obesos na pooopula??o

sobrepeso_pre_taxa = table_imc %>% filter(imc >= 25) #filtrando sobrepeso

sum(sobrepeso_pre_taxa$perc) #percentual da popula??o com sobrepeso


#### CALCULANDO ALTERA??O NO IMC P?S INTERVEN??O

dados_3 = dados_2

dados_3$peso_novo_sem_subst_cal = dados_3$peso - reducao_peso_sem_subst_cal #calculando novo peso ap?s taxa

imc_pos_intervencao_sem_subst_cal = dados_3$peso_novo_sem_subst_cal/((dados_2$altura/100)^2) # calculando novo imc

table_imc_pos_intervencao_sem_subst_cal = as.data.frame(table(imc_pos_intervencao_sem_subst_cal))

table_imc_pos_intervencao_sem_subst_cal$imc_pos_intervencao_sem_subst_cal = as.numeric(as.character(table_imc_pos_intervencao_sem_subst_cal$imc_pos_intervencao_sem_subst_cal))

table_imc_pos_intervencao_sem_subst_cal$perc = (prop.table(table_imc_pos_intervencao_sem_subst_cal$Freq))*100

total_obesidade_pos_imp_sem_subst_cal = table_imc_pos_intervencao_sem_subst_cal %>% filter(imc_pos_intervencao_sem_subst_cal >= 30) # selecionando apenas indiv?duos obesos

sum(total_obesidade_pos_imp_sem_subst_cal$perc) #percentual da popula??o obesa

total_sobreso_pos_imp_sem_subst_cal = table_imc_pos_intervencao_sem_subst_cal %>% filter(imc_pos_intervencao_sem_subst_cal >= 25) # selecionando indiv?duos acima do peso

sum(total_sobreso_pos_imp_sem_subst_cal$perc) # percentual da popula??o acima do peso


var_obes = sum(obes_pre_taxa$perc) - sum(total_obesidade_pos_imp_sem_subst_cal$perc)

var_sobrepeso = sum(sobrepeso_pre_taxa$perc) - sum(total_sobreso_pos_imp_sem_subst_cal$perc)


### GR?FICOS

# imc pr? taxa

imc_selecionados = filter(table_imc, imc > 16 & imc < 40) # retirando disparidades para gr?fico

imc_arredondado = as.data.frame(table(round(imc_selecionados$imc)))

imc_arredondado$perc = (prop.table(imc_arredondado$Freq)*100)

colnames(imc_arredondado) = c("IMC", "Freq", "Percentual")

imc_arredondado$IMC = as.numeric(as.character(imc_arredondado$IMC))


# imc p?s taxa

imc_selecionados_sem_subst_cal = filter(table_imc_pos_intervencao_sem_subst_cal, imc_pos_intervencao_sem_subst_cal > 16 & imc_pos_intervencao_sem_subst_cal < 40) # retirando disparidades para gr?fico

imc_arredondado_sem_subst_cal = as.data.frame(table(round(imc_selecionados_sem_subst_cal$imc_pos_intervencao_sem_subst_cal)))

imc_arredondado_sem_subst_cal$Freq = as.numeric(as.character(imc_arredondado_sem_subst_cal$Freq))

imc_arredondado_sem_subst_cal$perc = (prop.table(imc_arredondado_sem_subst_cal$Freq)*100)

colnames(imc_arredondado_sem_subst_cal) = c("IMC", "Freq", "Percentual")

imc_arredondado_sem_subst_cal$IMC = as.numeric(as.character(imc_arredondado_sem_subst_cal$IMC))



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

plot_distribuicao_imc_sem_subst_cal = plot_ly(x= imc_arredondado$IMC, y = imc_arredondado$Percentual, type = 'bar', name = "IMC antes da Taxa??o" , marker = list(color = '#FFC0D5',line = list(color = '#FF90B5', width = 1.5))) %>%
  add_trace(x= imc_arredondado_sem_subst_cal$IMC, y = imc_arredondado_sem_subst_cal$Percentual, type = 'bar', name = "IMC p?s Taxa??o", marker = list(color = '#DFE9FF', line = list(color = '#B2C8FC', width = 1.0))) %>%
  layout(yaxis = list(title = 'Percentual da popula??o adulta (%)', text = paste(imc_arredondado_2$Percentual, '%')), legend = list(orientation = 'h', x = 100, y = 8), xaxis = list(title = "IMC",  nticks = 6), annotations = list(a,b))







