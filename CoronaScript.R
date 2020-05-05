rm(list =ls());gc()
library(data.table)
library(dplyr)
library(zoo)
library(forecast)
library(tidyverse)


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
  dataFun <- dataFun[  , .(varSum = sum(varSum)), by = .(Coun, Prov,Fecha) ][ order( Coun, Prov, Fecha)]
  setnames(dataFun, old = "varSum", new = varTxt)
  return(dataFun)
}
new <- funcMelt(dataFun = fread(input = "ncov.txt"), varTxt = "newCases")
dea <- funcMelt(dataFun = fread(input = "muertes2.txt"), varTxt = "newCases")
rec <- funcMelt(dataFun = fread(input = "recover.txt"), varTxt = "newCases")

fread(input = "ncovNew.txt")
new2 <- funcMelt(dataFun = fread(input = "ncovNew.txt"), varTxt = "newCases")
dea2 <- funcMelt(dataFun = fread(input = "muertesNew.txt"), varTxt = "newCases")
rec2 <- funcMelt(dataFun = fread(input = "recoverNew.txt"), varTxt = "newCases")

new3 <- rbind(new, new2)
dea3 <- rbind(dea, dea2)
rec3 <- rbind(rec, rec2)

new3$Coun <- gsub(pattern = " ", replacement = "_", new3$Coun) %>% tolower
dea3$Coun <- gsub(pattern = " ", replacement = "_", dea3$Coun) %>% tolower
rec3$Coun <- gsub(pattern = " ", replacement = "_", rec3$Coun) %>% tolower

new4 <- new3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
dea4 <- dea3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
rec4 <- rec3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]

new5 <- dcast(data = new4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)
dea5 <- dcast(data = dea4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)
rec5 <- dcast(data = rec4[ , .(var = sum(newCases, na.rm = TRUE)), by = .(Coun, Fecha) ], formula = Fecha ~  Coun, value.var = "var", fill =  0)

datos <- new5 %>%  select(Fecha, iran, italy, spain)  %>%  data.frame()
xts::xts(x = datos[ , names(datos)[names(datos) != "Fecha"]], order.by = datos$Fecha  ) %>%  dygraphs::dygraph()

# model Iran --------------------------------------------------------------
y <- new5[ , iran] %>% .[ .> 0]
length(y)
y2 <- y[1:( length(y)-3)]
auto.arima(y2)
y %>% diff() %>% diff %>% ggtsdisplay(main="")
fit <- arima(y2, order=c( 0,2,0))
predicted <- forecast(fit, h = 3)
yControl <- y[ ( length(y)-2):( length(y))]
data.frame( predicted = predicted$mean, real = yControl)
mean( abs(  predicted$mean - yControl )/ yControl )
mean( abs(  predicted$mean - yControl ) )
plot( y = as.vector(predicted$mean),  x = yControl, xlab = "Real" , ylab = "Fitted"  )
abline(a = 0, b = 1, col = 2)
title("Iran")
checkresiduals(fit)
autoplot(forecast(fit))
forecast(fit, h = 11)



# model Italy -------------------------------------------------------------
y <- new5[ , italy] %>% .[ .> 0]
length(y)
y2 <- y[1:( length(y)-3)]
auto.arima(y2)
y %>% diff() %>% diff %>% ggtsdisplay(main="")
fit <- arima(y2, order=c( 0,2,0))
predicted <- forecast(fit, h = 3)
yControl <- y[ ( length(y)-2):( length(y))]
data.frame( predicted = predicted$mean, real = yControl)
mean( abs(  predicted$mean - yControl )/ yControl )
mean( abs(  predicted$mean - yControl ) )
plot( y = as.vector(predicted$mean),  x = yControl, xlab = "Real" , ylab = "Fitted"  )
abline(a = 0, b = 1, col = 2)
title("Italy")
checkresiduals(fit)
autoplot(forecast(fit))
forecast(fit, h = 11)


# model Spain -------------------------------------------------------------
y <- new5[ , spain] %>% .[ .> 0]
length(y)
y2 <- y[1:( length(y))]
auto.arima(y2)
y %>% diff() %>% diff %>% ggtsdisplay(main="")
fit <- arima(y2, order=c( 0,2,0))
checkresiduals(fit)
autoplot(forecast(fit))
forecast(fit, h = 8)





