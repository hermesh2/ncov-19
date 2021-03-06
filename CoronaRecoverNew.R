# https://data.worldbank.org/indicator/sp.pop.totl
# https://rubenfcasal.github.io/COVID-19/COVID-19-tablas.html
# https://rubenfcasal.github.io/pd
rm(list =ls());gc()

if(!require("pacman")){
  install.packages("pacman")
}
pacman::p_load(data.table, dplyr, zoo, forecast, lubridate, dygraphs)  

fecIni <- 20200101
n <- 0 # Puede ser que el dato del dia no se fiable.
cat("Esta es la fecha tope del script", as.character(format( Sys.Date() - days(n), "%d-%m-%Y") ) , "\n")
# setwd("C:/Users/hermesh/Dropbox/Investigaciones/Coronavirus/")
# setwd("C:/Users/herme///Dropbox/Investigaciones/Coronavirus/")
funcMelt <- 
  function(dataFun, varTxt = "txt" ){
    dataFun <- 
      dataFun %>% 
      melt(measure.vars = names(dataFun)[ ! names(dataFun ) %in% c("Province/State",  "Country/Region", "Lat", "Long")])
    
    dataFun$Fecha <- 
      dataFun$variable %>% as.character() %>% strsplit(split = " ") %>% lapply(FUN = function(x){x[[1]]}) %>% unlist %>% 
      as.Date(format = "%m/%d/%y")
    dataFun[is.na(value)]$value <- 0
    setnames( dataFun, old = names(dataFun), new = c("Prov", "Coun", "lat", "lon", "var","varSum", "Fecha"))
    dataFun <- dataFun[  , .(varSum = 
                               sum(varSum))
                         , by = .(Coun, Prov,Fecha) ][ order( Coun, Prov, Fecha)]
    setnames(dataFun, old = "varSum", new = varTxt)
    return(dataFun)
  }


# new <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
#                 , varTxt = "newCases")
# dea <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
#                 , varTxt = "newCases")
# rec <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
#                 , varTxt = "newCases")
# 
# new2 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_2019-ncov-Confirmed.csv")
#                  , varTxt = "newCases")
# dea2 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_2019-ncov-Deaths.csv")
#                  , varTxt = "newCases")
# rec2 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_2019-ncov-Recovered.csv")
#                  , varTxt = "newCases")
# new3 <- rbind(new, new2)[ order(Coun, Prov, Fecha, decreasing = FALSE ) ]
# dea3 <- rbind(dea, dea2)[ order(Coun, Prov, Fecha, decreasing = FALSE ) ]
# rec3 <- rbind(rec, rec2)[ order(Coun, Prov, Fecha, decreasing = FALSE ) ]


new3 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
                , varTxt = "newCases")
dea3 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
                , varTxt = "newCases")
rec3 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
                , varTxt = "newCases")


new3$Coun <- gsub(pattern = " ", replacement = "_", new3$Coun) %>% tolower
dea3$Coun <- gsub(pattern = " ", replacement = "_", dea3$Coun) %>% tolower
rec3$Coun <- gsub(pattern = " ", replacement = "_", rec3$Coun) %>% tolower

new4 <- new3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
dea4 <- dea3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
rec4 <- rec3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]

new5 <- dcast(data = new4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)
dea5 <- dcast(data = dea4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)
rec5 <- dcast(data = rec4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)

# Soluciono el problema del decalaje
new5$Fecha <- new5$Fecha # - days(1)
dea5$Fecha <- dea5$Fecha # - days(1)
rec5$Fecha <- rec5$Fecha # - days(1)

new5 <- new5 [ new5$Fecha <= Sys.Date() - days(n) ]
dea5 <- dea5 [ dea5$Fecha <= Sys.Date() - days(n) ]
rec5 <- rec5 [ rec5$Fecha <= Sys.Date() - days(n) ]

datos <- new5 %>%  select(Fecha, china, `korea,_south`,italy, spain, france, germany, united_kingdom, us)  %>%  data.frame()
xts::xts(x = datos[ , names(datos)[names(datos) != "Fecha"]], order.by = datos$Fecha  ) %>%  dygraphs::dygraph()



foo <- function(x){
    x %>% diff %>% c(0,.) %>% ifelse(test = . < 0, yes = 0, no = .)
}
DTnames <- names(datos)[names(datos) !=  "Fecha"]
datos <- data.table(datos)
datos[ , (DTnames) := lapply( .SD, foo ), .SDcol = DTnames ]
datos  
datosNew <- data.frame(datos[ Fecha > ymd(fecIni) & Fecha <= Sys.Date() ])
xts::xts(x = datosNew[ , names(datosNew)[names(datosNew) != "Fecha"]], order.by = datosNew$Fecha  ) %>%  dygraphs::dygraph()

datos <- rec5 %>%  select(Fecha, china, `korea,_south`,italy, spain, france, germany, united_kingdom, us)  %>%  data.frame()
datos <- data.table(datos)
datos[ , (DTnames) := lapply( .SD, foo ), .SDcol = DTnames ]
datos  
datosRecover <- data.frame(datos[ Fecha > ymd(fecIni) & Fecha <= Sys.Date() ])
xts::xts(x = datosRecover[ , names(datosRecover)[names(datosRecover) != "Fecha"]], order.by = datosRecover$Fecha  ) %>%  dygraphs::dygraph()

datosAll <- merge(x = datosNew, y = datosRecover, by = "Fecha", all = TRUE)
datosAll <- datosAll[ order(datosAll$Fecha), ]
names(datosAll) <- gsub(pattern = ".x", replacement = "_newCase", x = names(datosAll), fixed = TRUE)
names(datosAll) <- gsub(pattern = ".y", replacement = "_newRecover", x = names(datosAll), fixed = TRUE)
xts::xts(x = datosAll[ , names(datosAll)[grep( pattern = "china", x = names(datosAll) )   ]]
         , order.by = datosAll$Fecha  ) %>%  dygraphs::dygraph()
xts::xts(x = datosAll[ , names(datosAll)[grep( pattern = "korea", x = names(datosAll) )   ]]
         , order.by = datosAll$Fecha  ) %>%  dygraphs::dygraph()
xts::xts(x = datosAll[ , names(datosAll)[grep( pattern = "spain", x = names(datosAll) )   ]]
         , order.by = datosAll$Fecha  ) %>%  dygraphs::dygraph()
xts::xts(x = datosAll[ , names(datosAll)[grep( pattern = "italy", x = names(datosAll) )   ]]
         , order.by = datosAll$Fecha  ) %>%  dygraphs::dygraph()
xts::xts(x = datosAll[ , names(datosAll)[grep( pattern = "france", x = names(datosAll) )   ]]
         , order.by = datosAll$Fecha  ) %>%  dygraphs::dygraph()
xts::xts(x = datosAll[ , names(datosAll)[grep( pattern = "germany", x = names(datosAll) )   ]]
         , order.by = datosAll$Fecha  ) %>%  dygraphs::dygraph()
xts::xts(x = datosAll[ , names(datosAll)[grep( pattern = "united_kingdom", x = names(datosAll) )   ]]
         , order.by = datosAll$Fecha  ) %>%  dygraphs::dygraph()
xts::xts(x = datosAll[ , names(datosAll)[grep( pattern = "united_kingdom", x = names(datosAll) )   ]]
         , order.by = datosAll$Fecha  ) %>%  dygraphs::dygraph()

datos <- dea5 %>%  select(Fecha, china, `korea,_south`,italy, spain, france, germany, united_kingdom, us)  %>%  data.frame()
datos <- data.table(datos)
datos[ , (DTnames) := lapply( .SD, foo ), .SDcol = DTnames ]
datos  
datosDeath <- data.frame(datos[ Fecha > ymd(fecIni) & Fecha <= Sys.Date() ])
xts::xts(x = datosDeath[ , names(datosDeath)[names(datosDeath) != "Fecha"]], order.by = datosDeath$Fecha  ) %>%  dygraphs::dygraph()
names(datosDeath)[-1] <- paste0(names(datosDeath)[-1] , "_death")
datosAll <- merge(x = datosAll, y = datosDeath, by = "Fecha", all = TRUE)

# hay algo raro en los datos korea del sur el dia 8, esta en los originales
# rec5[ rec5$Fecha > ymd( 20200305) , c("Fecha", "korea,_south")]
# rec[ rec$Fecha > ymd( 20200305) & grepl( pattern = "orea", rec$Coun) , c("Fecha", "newCases")]
# rec3[ rec3$Fecha > ymd( 20200305) & rec3$Coun == "korea,_south", c("Fecha", "newCases")]
# x <- fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
# x[ grepl(pattern = "orea", `Country/Region` )]
# x <- fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
# x[ grepl(pattern = "orea", `Country/Region` )]

# https://www.elconfidencial.com/multimedia/video/mundo/2020-03-07/como-se-hace-una-cuarentena-masiva-wuhan-la-mayor-cuarentena-de-la-historia_2486287/
# 20200128
# https://www.dw.com/es/coronavirus-italia-el-primer-d%C3%ADa-de-la-cuarentena/a-52701099
# 20200303
# Espa�a
# 20200114
plotActivos <- 
  function(counTxt){
    x <- data.frame( 
      Activos = c (cumsum( datosAll[ , paste0(counTxt,"_newCase")] ) 
                   - cumsum( datosAll[ , paste0(counTxt,"_newRecover")] ) 
                   - cumsum(datosAll[ , paste0(counTxt,"_death")] ) )
      , recuperados = c( cumsum( datosAll[ , paste0(counTxt,"_newRecover")] ) )
      , muertos = c( cumsum( datosAll[ , paste0(counTxt,"_death")] ) )
      , Totales = c (cumsum( datosAll[ , paste0(counTxt,"_newCase")] ) )
    )
    x %>% tail(1) ->.;  (.$muertos / (.$muertos + .$recuperados) * 100) %>%  
      round(2) %>% cat("Ratio Mortalidad sobre muertos y recuperados", . ,"\n")
    x %>% tail(1) ->.;  (.$muertos / (.$Totales) * 100) %>%  
      round(2) %>% cat( "Ratio Mortalidad sobre totales", . ,"\n")
    x$Totales <- rm()
    zoo( x =  x, order.by = datos$Fecha ) %>% dygraph(main = counTxt) %>%  print
    cbind(x, Fecha =  datos$Fecha )
}

plotActivos("china")
plotActivos("korea._south")
plotActivos("italy")
plotActivos("spain")
plotActivos("france")
plotActivos("germany")
plotActivos("united_kingdom")
plotActivos("us")


datos <- dea5 %>%  select(Fecha
                          # , china
                          # , `korea,_south`
                          ,italy
                          , spain
                          # , iran
                          , us
                          # , netherlands
                          , france
                          , germany
                          # , japan
                          , united_kingdom)  %>%  data.frame()

# log( datos[ , -1])  

datos %>% xts::xts(x = log(.[ , -1]), order.by = .$Fecha) %>% dygraph()

datosCentrados <- datos
datosCentrados[datos > -1] <- NA
nfil <- nrow(datos)
for( i in names(datos[,-1])){
  x <- datos[,i]
  index <- which( x!= 1)[1]
  datosCentrados[1:(nfil - index + 1) , i]  <- x[index:nfil]
}
x <- datos[,"spain"]



index <- which( x!= 1)[1]
FechaDiaUnoSpain <- datos$Fecha[index]
datosCentrados$Fecha <- FechaDiaUnoSpain + days(1:nfil)
datosCentrados %>% xts::xts(x = log(.[ , -1]), order.by = .$Fecha) %>% dygraph()
datosCentrados %>% xts::xts(x = (.[ , -1]), order.by = .$Fecha) %>% dygraph()

datosCentrados %>%  tail

