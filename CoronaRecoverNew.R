rm(list =ls());gc()
library(data.table)
library(dplyr)
library(zoo)
library(forecast)
library(lubridate)

setwd("C:/Users/hermesh/Dropbox/Investigaciones/Coronavirus/")

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


new <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
                , varTxt = "newCases")
dea <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
                , varTxt = "newCases")
rec <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
                , varTxt = "newCases")

new2 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_2019-ncov-Confirmed.csv")
                 , varTxt = "newCases")
dea2 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_2019-ncov-Deaths.csv")
                 , varTxt = "newCases")
rec2 <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_2019-ncov-Recovered.csv")
                 , varTxt = "newCases")

new3 <- rbind(new, new2)[ order(Coun, Prov, Fecha, decreasing = FALSE ) ]
dea3 <- rbind(dea, dea2)[ order(Coun, Prov, Fecha, decreasing = FALSE ) ]
rec3 <- rbind(rec, rec2)[ order(Coun, Prov, Fecha, decreasing = FALSE ) ]

new3$Coun <- gsub(pattern = " ", replacement = "_", new3$Coun) %>% tolower
dea3$Coun <- gsub(pattern = " ", replacement = "_", dea3$Coun) %>% tolower
rec3$Coun <- gsub(pattern = " ", replacement = "_", rec3$Coun) %>% tolower

new4 <- new3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
dea4 <- dea3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
rec4 <- rec3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]

new5 <- dcast(data = new4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)
dea5 <- dcast(data = dea4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)
rec5 <- dcast(data = rec4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)

datos <- new5 %>%  select(Fecha, china, `korea,_south`,italy, spain)  %>%  data.frame()
xts::xts(x = datos[ , names(datos)[names(datos) != "Fecha"]], order.by = datos$Fecha  ) %>%  dygraphs::dygraph()



foo <- function(x){
    x %>% diff %>% c(0,.) %>% ifelse(test = . < 0, yes = 0, no = .)
}
DTnames <- names(datos)[names(datos) !=  "Fecha"]
datos <- data.table(datos)
datos[ , (DTnames) := lapply( .SD, foo ), .SDcol = DTnames ]
datos  
datosNew <- data.frame(datos[ Fecha > ymd(20200215) & Fecha != Sys.Date() - days(1)])
xts::xts(x = datosNew[ , names(datosNew)[names(datosNew) != "Fecha"]], order.by = datosNew$Fecha  ) %>%  dygraphs::dygraph()

datos <- rec5 %>%  select(Fecha, china, `korea,_south`,italy, spain)  %>%  data.frame()
datos <- data.table(datos)
datos[ , (DTnames) := lapply( .SD, foo ), .SDcol = DTnames ]
datos  
datosRecover <- data.frame(datos[ Fecha > ymd(20200215) & Fecha != Sys.Date() - days(1)])
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


# hay algo raro en los datos korea del sur el dia 8, esta en los originales
# rec5[ rec5$Fecha > ymd( 20200305) , c("Fecha", "korea,_south")]
# rec[ rec$Fecha > ymd( 20200305) & grepl( pattern = "orea", rec$Coun) , c("Fecha", "newCases")]
# rec3[ rec3$Fecha > ymd( 20200305) & rec3$Coun == "korea,_south", c("Fecha", "newCases")]
# x <- fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
# x[ grepl(pattern = "orea", `Country/Region` )]
# x <- fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
# x[ grepl(pattern = "orea", `Country/Region` )]
