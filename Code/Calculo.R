##### PREPARANDO O AMBIENTE DE TRABALHO ######

# install.packages("readr")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("zoo")
# install.packages("plotly)
# install.packages("highcharter")

library(readr)
library(dplyr)
library(data.table)
library(zoo)
library(plotly)

####################################################
#### Importando dados de consumo e de calorias #####
####################################################

# dados de consumo off-trade 

consumo_soft_drinks = fread('C:/Users/User/dados euromonitor/consumo_bebidas.csv')

str(consumo_soft_drinks)

# improtanto base de calorias de cada bebida

calorias_soft_drinks = fread("C:/Users/User/dados euromonitor/calorias_bebidas.csv")

str(calorias_soft_drinks)


#### Unindo dados ####

consumo = merge(consumo_soft_drinks, calorias_soft_drinks, by = 'Bebida')


# Retirando o total de refrigerantes (pois já temos o desagregado)

consumo = consumo [-4,]


#################
### CÁLCULOS ###
################

# determinando o total de calorias ingeridas, diárias e anualmente per capita

consumo$consumo_calorico = consumo$Cosumo * consumo$Calorias

consumo$consumo_cal_diario = (consumo$Cosumo*consumo$Calorias)/365

sum(consumo$consumo_cal_diario)  #total de calorias ingeridas ao dia

#############################################################
### Cálculo da variação do consumo pós introdução de taxa ###


#### elasticidades 

elasticidade_refrigerantes = -0.61

elasticidade_demais = -1.32

elasticidade = c(-1.32, -1.32, -1.32, -1.32, -0.61, -0.61, -0.61, -0.61, -0.61)
  
consumo$elasticidade = elasticidade # unindo à base de dados pré-existente

##################################################
### 1. supondo repasse total de 20% para produtos

consumo$variacao = (0.20 * elasticidade)

consumo$novo_consumo = consumo$Cosumo * (consumo$variacao + 1 ) # consumo pós taxação

consumo$novo_consumo_calorico = consumo$novo_consumo * consumo$Calorias #consumo calórico pós taxação

consumo$novo_consumo_cal_diario = (consumo$novo_consumo*consumo$Calorias)/365 #consumo calórico diário pós taxação

sum(consumo$novo_consumo_cal_diario) #consumo calórico diário pós taxação

### Estimando a variação de peso a partir da variação calórica

# estimando as calorias líquidas salvas, assumindo grau de substituição calórica

consumo$calorias_salvas_totais = consumo$consumo_calorico - consumo$novo_consumo_calorico

consumo$calorias_salvas_totais_diar = consumo$consumo_cal_diario - consumo$novo_consumo_cal_diario

consumo$calorias_liquidas_salvas_diar = consumo$calorias_salvas_totais_diar * 0.67

consumo$calorias_liquidas_salvas = consumo$calorias_salvas_totais * 0.67

total_salvo = sum(consumo$calorias_liquidas_salvas) # total da redução de calorias reduzidas em um ano per capita

total_salvo_diar = sum(consumo$calorias_liquidas_salvas_diar) #total da redução calórica por dia per capita

# Assumindo que 3.500 kgs altera 0,455 gramas de massa

reducao_peso = (total_salvo*0.453)/3500


#############################################
###### Cálculo da variação do IMC ###########
#############################################

##### CALCULO DO IMC PRÉ TAXA

# Importando microdados POF 2008-2009 para c?lculo do IMC

load("C:/Users/User/Documents/Taxa_Bedidas_açucaradas/Dados/2009/t_morador_s.rda")

# transformando dados 

dados = as.data.frame(t_morador_s)

dados_1 = filter(dados, idade_anos > 18) #selecionando apenas os adultos

dados_2 = data.frame (dados_1$peso_imputado, dados_1$altura_imputado, dados_1$cod_sexo)

colnames(dados_2) = c ("peso", "altura", "sexo")

dados_2 = dados_2 %>% dplyr::filter (peso > 20) # retirando dados errados ou outliers, cujo peso de adultos é menor que 20 kgs

imc = dados_2$peso/((dados_2$altura/100)^2) #calculo do IMC

table_imc = as.data.frame(table(imc))

table_imc$imc = as.numeric(as.character(table_imc$imc))

table_imc$perc = (prop.table(table_imc$Freq))*100

table_imc$freq = as.numeric(table_imc$Freq)

obes_pre_taxa = table_imc %>% filter(imc >= 30) #filtrando apenas os obesos

sum(obes_pre_taxa$perc) # percentual de obesos na pooopulação
 
sobrepeso_pre_taxa = table_imc %>% filter(imc >= 25) #filtrando sobrepeso

sum(sobrepeso_pre_taxa$perc) #percentual da população com sobrepeso


#### CALCULANDO ALTERÇÃO NO IMC PÓS INTERVENÇÃO

dados_3 = dados_2

dados_3$peso_novo = dados_3$peso - reducao_peso #calculando novo peso após taxa

imc_pos_intervencao = dados_3$peso_novo/((dados_2$altura/100)^2) # calculando novo imc

table_imc_pos_intervencao = as.data.frame(table(imc_pos_intervencao))

table_imc_pos_intervencao$imc_pos_intervencao = as.numeric(as.character(table_imc_pos_intervencao$imc_pos_intervencao))

table_imc_pos_intervencao$perc = (prop.table(table_imc_pos_intervencao$Freq))*100

table_imc$freq = as.numeric(table_imc$Freq)

total_obesidade_pos_imp = table_imc_pos_intervencao %>% filter(imc_pos_intervencao >= 30) # selecionando apenas indivíduos obesos

sum(total_obesidade_pos_imp$perc) #percentual da população obesa

total_sobreso_pos_imp = table_imc_pos_intervencao %>% filter(imc_pos_intervencao >= 25) # selecionando indivíduos acima do peso

sum(total_sobreso_pos_imp$perc) # percentual da população acima do peso


##########################
##### Variação do IMC (%)

var_obes = sum(obes_pre_taxa$perc) - sum(total_obesidade_pos_imp$perc)

var_sobrepeso = sum(sobrepeso_pre_taxa$perc) - sum(total_sobreso_pos_imp$perc)


 
#################################################################
############# ANÁLISE DE SENSIBILIDADE ##########################
################################################################

###########################################
########## VARIAÇÃO NO REPASSE ############


# 1. supondo repasse total de 110% para produtos (22%)

consumo$variacao110 = (0.22 * elasticidade)

consumo$novo_consumo110 = consumo$Cosumo * (consumo$variacao110 + 1 ) # consumo pós taxação

consumo$novo_consumo_calorico110 = consumo$novo_consumo110 * consumo$Calorias #consumo calórico pós taxação

consumo$novo_consumo_cal_diario110 = (consumo$novo_consumo110*consumo$Calorias)/365 #consumo calórico diário pós taxação

sum(consumo$novo_consumo_cal_diario110) #consumo calórico diário pós taxação

### Estimando a variação de peso a partir da variação calórica

# estimando as calorias líquidas salvas, assumindo grau de substituição calórica

consumo$calorias_salvas_totais110 = consumo$consumo_calorico - consumo$novo_consumo_calorico110

consumo$calorias_salvas_totais_diar110 = consumo$consumo_cal_diario - consumo$novo_consumo_cal_diario110

consumo$calorias_liquidas_salvas_diar110 = consumo$calorias_salvas_totais_diar110 * 0.67

consumo$calorias_liquidas_salvas110 = consumo$calorias_salvas_totais110 * 0.67

total_salvo110 = sum(consumo$calorias_liquidas_salvas110) # total da redução de calorias reduzidas em um ano per capita

total_salvo_diar110 = sum(consumo$calorias_liquidas_salvas_diar110) #total da redução calórica por dia per capita

# Assumindo que 3.500 kgs altera 0,455 gramas de massa

reducao_peso110 = (total_salvo110*0.453)/3500



#####################################
##### CALCULO DO IMC PRÉ TAXA  #####
####################################

sum(obes_pre_taxa$perc) # percentual de obesos na pooopulação

sobrepeso_pre_taxa = table_imc %>% filter(imc >= 25) #filtrando sobrepeso

sum(sobrepeso_pre_taxa$perc) #percentual da popula??o com sobrepeso


###########################################################
#### CALCULANDO ALTERAÇÃOO NO IMC PÓS INTERVENÇÃOO ########
###########################################################


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


# 2. Supondo repasse de apenas 90% dos pessoas (variaçãoo de 18%)


consumo$variacao90 = (0.18 * elasticidade)

consumo$novo_consumo90 = consumo$Cosumo * (consumo$variacao90 + 1 ) # consumo pós taxação

consumo$novo_consumo_calorico90 = consumo$novo_consumo90 * consumo$Calorias #consumo calórico pós taxação

consumo$novo_consumo_cal_diario90 = (consumo$novo_consumo90*consumo$Calorias)/365 #consumo calórico diário pós taxação

sum(consumo$novo_consumo_cal_diario90) #consumo calórico diário pós taxação

### Estimando a variação de peso a partir da variação calórica

# estimando as calorias líquidas salvas, assumindo grau de substituição calórica

consumo$calorias_salvas_totais90 = consumo$consumo_calorico - consumo$novo_consumo_calorico90

consumo$calorias_salvas_totais_diar90 = consumo$consumo_cal_diario - consumo$novo_consumo_cal_diario90

consumo$calorias_liquidas_salvas_diar90 = consumo$calorias_salvas_totais_diar90 * 0.67

consumo$calorias_liquidas_salvas90 = consumo$calorias_salvas_totais90 * 0.67

total_salvo90 = sum(consumo$calorias_liquidas_salvas90) # total da redução de calorias reduzidas em um ano per capita

total_salvo_diar90 = sum(consumo$calorias_liquidas_salvas_diar90) #total da redução calórica por dia per capita

# Assumindo que 3.500 kgs altera 0,455 gramas de massa

reducao_peso90 = (total_salvo90*0.453)/3500

#### CALCULANDO ALTERAÇÃO NO IMC PÓS INTERVENÇÃO

dados_3 = dados_2

dados_3$peso_novo90 = dados_3$peso - reducao_peso90 #calculando novo peso após taxa

imc_pos_intervencao90 = dados_3$peso_novo90/((dados_2$altura/100)^2) # calculando novo imc

table_imc_pos_intervencao90 = as.data.frame(table(imc_pos_intervencao90))

table_imc_pos_intervencao90$imc_pos_intervencao90 = as.numeric(as.character(table_imc_pos_intervencao90$imc_pos_intervencao90))

table_imc_pos_intervencao90$perc = (prop.table(table_imc_pos_intervencao90$Freq))*100

table_imc$freq = as.numeric(table_imc$Freq)

total_obesidade_pos_imp90 = table_imc_pos_intervencao90 %>% filter(imc_pos_intervencao90 >= 30) # selecionando apenas indivíduos obesos

sum(total_obesidade_pos_imp90$perc) #percentual da população obesa

total_sobreso_pos_imp90 = table_imc_pos_intervencao90 %>% filter(imc_pos_intervencao90 >= 25) # selecionando indiv´´iduos acima do peso

sum(total_sobreso_pos_imp90$perc) # percentual da população acima do peso


var_obes = sum(obes_pre_taxa$perc) - sum(total_obesidade_pos_imp90$perc)

var_sobrepeso = sum(sobrepeso_pre_taxa$perc) - sum(total_sobreso_pos_imp90$perc)


#################################################
########### SUBSTITUIÇÃO CALÓRICA ################

# 1. Supondo que não há substituição calórica

### Estimando a variação de peso a partir da variação calórica

# estimando as calorias l?quidas salvas, assumindo que não há substituição calórica

consumo$calorias_salvas_totais = consumo$consumo_calorico - consumo$novo_consumo_calorico

consumo$calorias_salvas_totais_diar = consumo$consumo_cal_diario - consumo$novo_consumo_cal_diario

consumo$calorias_liquidas_salvas_diar_sem_subst_cal = consumo$calorias_salvas_totais_diar

consumo$calorias_liquidas_salvas_sem_subst_cal = consumo$calorias_salvas_totais

total_salvo_sem_subst_cal = sum(consumo$calorias_liquidas_salvas_sem_subst_cal) # total da redução de calorias reduzidas em um ano per capita

total_salvo_diar_sem_subst_cal = sum(consumo$calorias_liquidas_salvas_diar_sem_subst_cal) #total da redução calórica por dia per capita

# Assumindo que 3.500 kgs altera 0,455 gramas de massa

reducao_peso_sem_subst_cal = (total_salvo_sem_subst_cal*0.453)/3500

#### CALCULANDO ALTERA??O NO IMC PÓS INTERVENÇÃO

dados_3 = dados_2

dados_3$peso_novo_sem_subst_cal = dados_3$peso - reducao_peso_sem_subst_cal #calculando novo peso após taxa

imc_pos_intervencao_sem_subst_cal = dados_3$peso_novo_sem_subst_cal/((dados_2$altura/100)^2) # calculando novo imc

table_imc_pos_intervencao_sem_subst_cal = as.data.frame(table(imc_pos_intervencao_sem_subst_cal))

table_imc_pos_intervencao_sem_subst_cal$imc_pos_intervencao_sem_subst_cal = as.numeric(as.character(table_imc_pos_intervencao_sem_subst_cal$imc_pos_intervencao_sem_subst_cal))

table_imc_pos_intervencao_sem_subst_cal$perc = (prop.table(table_imc_pos_intervencao_sem_subst_cal$Freq))*100

total_obesidade_pos_imp_sem_subst_cal = table_imc_pos_intervencao_sem_subst_cal %>% filter(imc_pos_intervencao_sem_subst_cal >= 30) # selecionando apenas indivíduos obesos

sum(total_obesidade_pos_imp_sem_subst_cal$perc) #percentual da população obesa

total_sobreso_pos_imp_sem_subst_cal = table_imc_pos_intervencao_sem_subst_cal %>% filter(imc_pos_intervencao_sem_subst_cal >= 25) # selecionando indivíduos acima do peso

sum(total_sobreso_pos_imp_sem_subst_cal$perc) # percentual da população acima do peso


var_obes = sum(obes_pre_taxa$perc) - sum(total_obesidade_pos_imp_sem_subst_cal$perc)

var_sobrepeso = sum(sobrepeso_pre_taxa$perc) - sum(total_sobreso_pos_imp_sem_subst_cal$perc)


### GR?FICOS

# imc pr? taxa

imc_selecionados = filter(table_imc, imc > 16 & imc < 40) # retirando disparidades para gr?fico

imc_arredondado = as.data.frame(table(round(imc_selecionados$imc)))

imc_arredondado$perc = (prop.table(imc_arredondado$Freq)*100)

colnames(imc_arredondado) = c("IMC", "Freq", "Percentual")

imc_arredondado$IMC = as.numeric(as.character(imc_arredondado$IMC))


