
# install.packages('rworldmap')

require(rworldmap)


population_records = readLines("~/API_SP.POP.TOTL_DS2_en_csv_v2.csv")[-(1:4)]
population_data = read.csv(text=population_records, header = TRUE)

population_data = data.frame(population_data$Country.Name, population_data$Country.Code) 
colnames(population_data) = c( 'Country', 'Country.Code')

obesity_data <- fread('~/obesity.csv', header = TRUE, sep = ';', stringsAsFactors = FALSE)

obes_data = merge(obesity_data, population_data,by = 'Country' )

mapped_data <- joinCountryData2Map(obes_data, joinCode = "ISO3", 
                                   nameJoinColumn = "Country.Code")


colourPalette <- RColorBrewer::brewer.pal(9, 'YlOrRd')

par(mai=c(0,0,0.5,0),xaxs="i",yaxs="i")
mapParams= mapCountryData(mapped_data, nameColumnToPlot = "Male", colourPalette= colourPalette, mapTitle= '' , addLegend = FALSE  ,oceanCol='lightblue' , missingCountryCol='white'
 )
)

do.call( addMapLegend
          , c( mapParams
                 , legendLabels="all"
                 , legendWidth=0.5
                 , legendIntervals="data"
                 , legendMar = 2 ) )



# feminina


par(mai=c(0,0,0.5,0),xaxs="i",yaxs="i")
mapParams_fem= mapCountryData(mapped_data, nameColumnToPlot = "Female", colourPalette= colourPalette, mapTitle= '' , addLegend = FALSE  ,oceanCol='lightblue' , missingCountryCol='white'
)
)

do.call( addMapLegend
         , c( mapParams_fem
              , legendLabels="all"
              , legendWidth=0.5
              , legendIntervals="data"
              , legendMar = 2 ) )
