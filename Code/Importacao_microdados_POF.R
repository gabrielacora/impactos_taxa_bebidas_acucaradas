####################################################
##### ANÁLISE DO CONSUMO DE BEBIDAS AÇUCARADAS #####
####################################################

###############################
### PREPARANDO AMBIENTENTE ####
##############################

#install.packages("downloader")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages ("data.table")
#install.packages("gdata")
#install.packages("SAScii")
#install.packages("downloader")
#install.packages("digest")
#install.packages("plotly")

library(downloader)
library(ggplot2)
library(dplyr)
library(data.table)
library(gdata)
library(SAScii)
library(downloader)
library(digest)
library(plotly)


# config globais

setwd( "~/Dados/" ) # aponta o caminho do diretório em que serão salvos os microdados da POF

###############################
## IMPORTANDO MICRODADOS POF ##
###############################


years.to.download = c( 2009 , 2003 )path.to.7z = normalizePath( "C:/Program Files/7-zip/7z.exe" )	# aqui é preciso ter o programa 7-zip instalado e o path do local corretamente colocado

source_url( "https://raw.githubusercontent.com/ajdamico/asdfree/master/Pesquisa%20de%20Orcamentos%20Familiares/download%20all%20microdata.R" , prompt = FALSE , echo = TRUE )


# se quiser especificar os anos para download:
# years.to.download = c( 2009 , 2003 )
# use apenas 2003 para importar os dados de 2003 e o mesmo para 2009. 

# remove the `#` in order to specify which years to download
# years.to.download = c( 2009 , 2003 )


# iniciando a programação

# conferindo se o programa 7zip está funcionando
if( system( paste0('"', path.to.7z , '" -h' ) ) != 0 ) stop("you need to install 7-zip")

# para prevenir que os dados baixem novamente uma vez que já foram baixados
source_url( 
	"https://raw.githubusercontent.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R" , 
	prompt = FALSE , 
	echo = FALSE 
)

# criando dois arquivos temporários e dosi diretórios temporários 
tf = tempfile() ; tf2 = tempfile() ; td = tempdir()


# baixando o arquivo principal 


for ( year in years.to.download ){


# cria uma pasta específica para cada ano no diretório 
dir.create( 
	normalizePath( paste( getwd() , year , sep = "/" ) ) , 
	showWarnings = FALSE 
)
	

# caminho para o download dos microdados no site do ibge
ftp.path = paste0( "ftp://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_",	year - 1 ,"_" ,	year ,"/Microdados/")

data.file = paste0( ftp.path , "Dados.zip" )

# importação das istruções sas dentro do diretório 
	if ( year < 2009 ){
		sas.input.instructions = paste0( ftp.path , "Documentacao.zip" )
	} else {
		sas.input.instructions = paste0( ftp.path , "documentacao.zip" )
	}
	

download_cached( data.file , tf , mode = "wb" )

# unzip
files = unzip( tf , exdir = td )

# download das instruções do sas
	
download_cached( sas.input.instructions , tf , mode = "wb" )

# unzip
files = c( files , unzip( tf , exdir = td ) )

	
# iniciando 2009, lembre-se que o código das comidas está disponível em um arquivo excel

if ( year >= 2009 ){
		alimentacao.file = paste0( ftp.path , "tradutores.zip" )
		
  # download do arquivo de alimentação na mesma pasta
download_cached( alimentacao.file , tf , mode = 'wb' )
		
  # unzip
 files = c( files , unzip( tf , exdir = td ) )
	}
		
# algumas linhas precisam ser manualmente codificadas 	
Encoding( files ) = 'latin1'
		
	
if ( year >= 2009 ){
		
# tabelas com o códigos de alimentação  #
    cda = files[ grep( 'codigos_de_alimentacao' , tolower( files ) ) ]
		componentes = read.xls( cda , sheet = 1 , skip = 1 , colClasses = 'character' )
		estrutura = read.xls( cda , sheet = 2 , skip = 1  , colClasses = 'character' )
				
	
		names( componentes ) =
			c( 'codigo' , 'nivel.1' , 'desc.1' , 'nivel.2' , 'desc.2' , 'nivel.3' , 'desc.3' )
		

		names( estrutura ) =
			names( componentes )[ -1 ]
		
		
		# componentes table has a footnote, so throw it out
		# by removing all records with a missing
		# or empty `nivel.1` field
		componentes = componentes[ !is.na( componentes$nivel.1 ) , ]
		componentes = componentes[ componentes$nivel.1 != "" , ]
		
		save( 
			componentes , estrutura , 
			file = paste0( './' , year , "/codigos de alimentacao.rda" ) 
		)
		
		# # # # # # # # # # # # # # # # # #
		# tabelas para pós-estratificação #
		
	
		pos = files[ grep( 'pos_estratos_totais' , tolower( files ) ) ]
	
		# extraindo as tabelas de pós estratificação
		poststr = read.xls( pos , sheet = 1 )
		# imported!  cool?  cool.
		
		# convertendo nome das colunas para letra minúscula
		names( poststr ) = tolower( names( poststr ) )
		
		# salvando os dados 
		save( 
			poststr ,
			file = paste0( './' , year , "/poststr.rda" ) 
		)
		
		
		rm( componentes , estrutura , poststr )
		
		# limpando o RAM
		gc()
	}
		
		
	
	
# # # # # # # # # # # # # # # # #
# importação da organização sas #
	
# extraindo os arquivos sas de leitura
leitura = files[ grep( 'leitura' , tolower( files ) ) ]

# lendo tudo para a memória
z = readLines( leitura )

# removendo alguns caracteres
z = gsub( "\t" , " " , z )

# removendo linhas contendo o padrão `if reg=__ then do;` 
	z = z[ !grepl( 'if reg=.* then do;' , z ) ]
	
# removendo @;
	z = gsub( "@;" , "" , z )
	
# removendo linhas contendo `input`
	z = z[ !( tolower( z ) == 'input' ) ]
	
# removendo o (SAScii-breaking)  `controle` columns
	z = z[ !grepl( "@3 controle 6." , z , fixed = TRUE ) ]
	
# escrevendo o arquivo novamente no segundo arquivo temporário
writeLines( z , tf2 )

# ache os parãmtros que iniciam as linhas

all.beginlines = grep( 'INFILE|infile' , z )
	
start.pos =
		unlist( 
			lapply(
				gregexpr( 
					"\\" , 
					z[ all.beginlines ] ,
					fixed = TRUE
				) ,
				max 
			) 
		) + 1
		
end.pos =
		unlist( 
				gregexpr( 
					".txt" , 
					z[ all.beginlines ] 
				) 
			) - 1 
		
# isolando os nomes dos dados que serão importados

data.files.to.import =
			substr( 
			z[ all.beginlines ] , 
			start.pos , 
			end.pos
		)
	
data.files.to.import
	
all.file.basenames =
		unlist( 
			lapply( 
				strsplit( 
					basename( files ) , 
					'.' , 
					fixed = TRUE 
				) , 
				'[[' , 
				1 
			) 
		)
	
for ( dfn in data.files.to.import ){
		if ( tolower( dfn ) == 't_rendimentos' ) {
			data.file = files[ which( 't_rendimentos1' == tolower( all.file.basenames ) ) ] 
		} else {
			data.file = files[ which( tolower( dfn ) == tolower( all.file.basenames ) ) ]
		}
		
		if ( length( data.file ) > 1 ){
		
		 	data.file = data.file[ grep( '.zip' , tolower( data.file ) , fixed = TRUE ) ]
			
			data.file = unzip( data.file , exdir = td )
		}
		
	
		
if ( grepl( "txt$" , tolower( data.file ) ) ){

curfile = data.file
			
	} else {

dos.command = paste0( '"' , path.to.7z , '" x ' , data.file )

 if ( .Platform$OS.type != 'windows' ) system( dos.command ) else shell( dos.command )

   curfile = gsub( ".7z" , ".txt" , basename( data.file ) )

		}
		
		
cur.beginline = which( tolower( dfn ) == tolower( data.files.to.import ) )
		
# importando os dados para o R

x = 	read.SAScii( curfile , tf2 , beginline = all.beginlines[ cur.beginline ] , skip.decimal.division = TRUE	)
		
# convertendo nomes das colunas para letra minúscula

names( x ) = tolower( names( x ) )
		
assign( tolower( dfn ) , x )
		
save( 
			list = tolower( dfn ) , 
			file = tolower( paste0( './' , year , "/" , dfn , ".rda" ) )
		)

	
rm( list = c( 'x' , 'dfn' ) )
		
gc()
		
		file.remove( curfile )
				
	}
	

 Encoding( files ) = ''
	
 file.remove( tf , tf2 , files )
	
}


##################################
##### ANÁLISE DOS MICRODADOS #####
##################################

# Importando dados

load("C:/Users/User/Documents/Taxa_Bedidas_açucaradas/Dados/2009/t_morador_s.rda")

# transformando dados 

dados_antropometricos = as.data.frame(t_morador_s)
dados_antropometricos_1 = filter(dados_antropometricos, idade_anos > 18)

dados_antropometricos_2 = data.frame (dados_antropometricos_1$peso_imputado, dados_antropometricos_1$altura_imputado, dados_antropometricos_1$cod_sexo)

dados_antropometricos_2 %>% filter (dados_antropometricos_2, dados_antropometricos_1.peso_imputado != 0.00)

imc =round(dados_antropometricos_2$dados_antropometricos_1.peso_imputado/((dados_antropometricos_2$dados_antropometricos_1.altura_imputado/100)^2)) 

imc = as.data.frame (imc)

imc_selecionados = filter(imc, imc > 16 & imc < 62)

summary(imc_selecionados)

table_imc = as.data.frame(table(imc_selecionados))

table_imc$perc = (prop.table(table_imc$Freq))*100


distribuicao_imc = data.frame(table_imc$imc_selecionados, table_imc$perc)

distribuicao_imc$table_imc.imc_selecionados =  as.numeric(as.character(distribuicao_imc$table_imc.imc_selecionados))

plot_distribuicao_imc = plot_ly(x= distribuicao_imc$table_imc.imc_selecionados, y = distribuicao_imc$table_imc.perc, type = 'bar')

total_obesos = filter(distribuicao_imc, table_imc.imc_selecionados >= 30)

total_obesidade = sum(total_obesos$table_imc.perc)


######################################################
#### CALCULANDO ALTERAÇÕES NO IMC PÓS INTERVENÇÃO ####
######################################################


dados_antro = dados_antropometricos
dados_antro$dados_antropometricos.peso_imputado = dados_antro$dados_antropometricos.peso_imputado - perda_peso

imc2 =round(dados_antro$dados_antropometricos.peso_imputado/((dados_antro$dados_antropometricos.altura_imputado/100)^2))


imc2 = as.data.frame(imc2)

require(plotly)

imc_selecionados2 = filter(imc2, imc2$imc2 > 16 & imc2$imc2 < 62)

summary(imc_selecionados)

pop_table_imc2= prop.table(imc_selecionados$imc2)

table_imc2 = as.data.frame(table(imc_selecionados2))

table_imc2$perc = (prop.table(table_imc2$Freq))*100


y = data.frame(table_imc2$imc_selecionados2, table_imc2$perc)

y$table_imc2.imc_selecionados2 = as.numeric(as.character(y$table_imc2.imc_selecionados2))

yy = plot_ly(x= x$table_imc.imc_selecionados, y = x$table_imc.perc, type = 'bar')

total_obeso = filter(y, table_imc2.imc_selecionados2 >= 30)

total_obesidade = sum(total_obeso$table_imc2.perc)

