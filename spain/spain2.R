# https://rubenfcasal.github.io/COVID-19/#enlacesq
# pacman::p_load(data.table, dplyr, reshape2, lubridate, sp)
# pacman::p_load(tigris)
# setwd(dir = "C:/Users/hermesh/Dropbox/Investigaciones/Coronavirus/ncov-19/spain/")
library(data.table)
library(dplyr)
library(zoo )
library(forecast )
library(lubridate )
library(dygraphs )
library(RColorBrewer )
library(xts )
library(shiny )
library(shinydashboard )
library(htmlwidgets )
library(DT )
library(plotly )
library(reshape2 )
library(shinycssloaders )
library(sp )
library(leaflet)
library(tigris)
rm(list = ls())


# leo datos ---------------------------------------------------------------
# https://github.com/rubenfcasal/COVID-19/blob/master/acumula22.RData
# https://github.com/rubenfcasal/COVID-19/blob/master/acumula2_hist/acumula22_hist.RData
# https://cnecovid.isciii.es/covid19/#documentaci%C3%B3n-y-datos
# https://cnecovid.isciii.es/covid19/resources/agregados.csv

# con <- url('https://github.com/rubenfcasal/COVID-19/blob/master/acumula22.RData?raw=true') # Create connexion
# load(con) #Load the data
# close(con) #close connexion
covidIcii <- fread(input = "https://cnecovid.isciii.es/covid19/resources/agregados.csv")
Notas <- covidIcii[ is.na(FECHA) | FECHA ==  "", .(Notas= CCAA)]
# NOTA 1: El objetivo de los datos que se publican en esta web es saber el número de casos acumulados a la fecha y que por tanto no se puede deducir que la diferencia entre un día y el anterior es el número de casos nuevos ya que esos casos pueden haber sido recuperados de fechas anteriores. Cualquier inferencia que se haga sobre las diferencias de un día para otro deben hacerse con precaución y son únicamente la responsabilidad del autor.
# NOTA 2: Los datos de estas comunidades son datos de prevalencia (personas ingresadas a fecha de hoy). No reflejan el total de personas que han sido hospitalizadas o ingresadas en UCI  a lo largo del periodo de notificación(CL(UCIs*)-GA(UCIS**)-CM***-MD****)
# * Desde el día 17/04/2020 las cifras de UCIs de CL son casos acumulados. Previamente se refieren a personas ingresadas ese día.
# **Desde el día 28/04/2020 las cifras  UCIs de GA  son casos acumulados. Previamente se refieren a personas ingresadas ese día. Se actualizará la serie cuando esté disponible.
# *** Desde el día 11/04/2020 las cifras de hospitalizados de CM son casos acumulados. Previamente se refieren a personas ingresadas ese día.
# *** Desde el día 12/04/2020 las cifras de UCIs de CM son casos acumulados. Previamente se refieren a personas ingresadas ese día.
# **** Desde el día 26/04/2020 las cifras de Hospitalizados y UCIs de MD son casos acumulados. Previamente se refieren a personas ingresadas ese día. Se actualizará la serie cuando esté disponible
# NOTA 3:Se excluyen de la serie las notificaciones de personas  con anticuerpos positivos sin síntomas en el momento de realización de la prueba en los que no se puede establecer un momento de contagio ni si han padecido o no la enfermedad.
covidIcii <- covidIcii[ !is.na(FECHA) & FECHA !=  ""]
covidIcii[ , ( names(covidIcii)) := lapply( .SD, FUN = function(x){
  ifelse(is.na(x), yes = 0, no =  x )
}), .SDcol = names(covidIcii) ] 
covidIcii[ , CASOS := `PCR+` + `TestAc+`]

shp <- readRDS("gadm36_ESP_1_sp.rds")
shp@data$HASC_1 <- gsub(pattern = "ES.", replacement = "", x = shp@data$HASC_1,fixed = TRUE)
shp@data$HASC_1[ shp@data$HASC_1 == "ML"] <- "ME"
shp@data$HASC_1[ shp@data$HASC_1 == "LO"] <- "RI"
shp@data$HASC_1[ shp@data$HASC_1 == "PM"] <- "IB"
shp@data[ order(shp@data$HASC_1),]$NAME_1 <- c( "Andalucia", "Aragon", "Asturias",  "Cantabria"
                                                ,  "Castilla_y_Leon", "Castilla_la_Mancha", "Canarias", "Catalunya"
                                                , "Extremadura", "Galicia", "Islas_Baleares", "Madrid"
                                                , "Melilla", "Murcia", "Navarra", "Euskadi"
                                                , "Rioja", "Valencia")

pop <- fread("population.csv", header = TRUE)

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

pop
ArrDatFun <- function(x){
  x <- reshape2::dcast( data = x, formula = fecha ~ iso, fun.aggregate = mean, value.var = "observado", fill = 0) %>% data.table
  x[ , ES := AN +  AR + AS + CB + CE + CL + CM + CN + CT + EX + GA + IB + MC + MD + ML + NC + PV + RI + VC ] 
  x <- x[ , .(fecha, ES, AN,  AR, AS, CN, CB, CL, CM, CT, CE, VC, EX, GA, IB, RI, MD, ME = ML, MC, NC, PV) ] 
  x <- data.frame(x)
  x %>% tail
  return(x)
}

x <- covidIcii[ , .(fecha = as.Date(FECHA, format = "%d/%m/%Y"), iso = CCAA ,observado = (CASOS) ) ]
new <- ArrDatFun(x)
x <- covidIcii[ , .(fecha = as.Date(FECHA, format = "%d/%m/%Y"), iso = CCAA ,observado = (`PCR+`) ) ]
newPCR <- ArrDatFun(x)
x <- covidIcii[ , .(fecha = as.Date(FECHA, format = "%d/%m/%Y"), iso = CCAA ,observado = (`TestAc+`) ) ]
newTest <- ArrDatFun(x)

x <- covidIcii[ , .(fecha = as.Date(FECHA, format = "%d/%m/%Y"), iso = CCAA ,observado = ( Hospitalizados) ) ]
# Hago una estimacion simple
x[ fecha <= ymd(20200425) & iso == "MD", observado := cumsum(observado)  ]
x[ fecha <= ymd(20200425) & iso == "MD", observado := round( observado/ max(observado) * x[ fecha == ymd(20200426 + 1) & iso == "MD"]$observado) ]
x[ iso == "MD"]

hos <- ArrDatFun(x)

x <- covidIcii[ , .(fecha = as.Date(FECHA, format = "%d/%m/%Y"), iso = CCAA ,observado = ( UCI) ) ]
# Hago una estimacion simple
x[ fecha <= ymd(20200425) & iso == "MD", observado := cumsum(observado)  ]
x[ fecha <= ymd(20200425) & iso == "MD", observado := round( observado/ max(observado) * x[ fecha == ymd(20200426 + 1) & iso == "MD"]$observado) ]
x[ iso == "MD"]
uci <- ArrDatFun(x)
uci[ , c("fecha","CL")]
uci[ , c("fecha","GA")]
uci[ , c("fecha","CM")]
uci[ , c("fecha","MD")]

x <- covidIcii[ , .(fecha = as.Date(FECHA, format = "%d/%m/%Y"), iso = CCAA ,observado = ( Fallecidos) ) ]
dea <- ArrDatFun(x)

# x <- covidIcii[ , .(fecha = as.Date(FECHA, format = "%d/%m/%Y"), iso = CCAA ,observado = ( Recuperados) ) ]
x <- covidIcii[ , .(fecha = as.Date(FECHA, format = "%d/%m/%Y"), iso = CCAA ,observado = ( 0) ) ]
rec <- ArrDatFun(x)



# popBool = FALSE
# pop = pop
# new3 = new
# dea3 = dea
# rec3 = rec2
# uci3 = uci
# hos3 = hos
# funciones ---------------------------------------------------------------
# funciones ---------------------------------------------------------------
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

dygGGany <- function(datzoo, anyo = as.numeric(fecIni), centrados = FALSE
                     , colores = NA, main = "", sizeN = 1, miny = NA, maxy = NA){
  library(RColorBrewer)
  if(class(datzoo)[1] != "SharedData"){
    if( any( class(datzoo) %in% c("zoo", "xts")) ){
      dat <- data.frame( date = index(datzoo), data.frame(datzoo))
      dat <- reshape2::melt(data = dat, id.vars = "date", measure.vars = names(dat)[names(dat) != "date"], fill = 0
                            ,variable.name = "tipo")
      dat <- dat[ , c("date", "tipo", "value" )]
    } else{
      dat <- datzoo
      if( all( c("date", "tipo", "value" ) %in% names(dat)) ){
        dat <- dat[ , c("date", "tipo", "value" )]
      } else{
        stop("CH: la tabla tiene que tener estas columnas c(\"date\", \"tipo\", \"value\" )")
      }
    }
  }else{
    warning("CH si es de tipo dinamico mejor hacer las transformacion fuera (melt2)")
    dat <- datzoo
  }
  
  if( is.na(colores) ){
    colN <- ifelse( uniqueN(dat$tipo) < 3, yes = 3,no = uniqueN(dat$tipo))
    colores <- brewer.pal(colN, name = "Set1")
  }
  if(centrados == TRUE){
    datFecAux <- data.frame( date = dat$date %>%  unique, date2 = NA )
    datFecAux <- datFecAux[ order(dat$date, decreasing = FALSE),]
    datFecAux$date2 <-   1:nrow(datFecAux)
    dat <- merge(dat, datFecAux, by = "date")
    dat$date <- dat$date2
    dat$date2 <- rm()
  }
  
  main <- paste0(toupper(substr(main, 1, 1)), substr(main, 2, nchar(main)) )
  p <- ggplot(dat, aes(x=date, y=value
                       , colour= tipo , group= tipo
  )) +
    # geom_point() +
    geom_line( size= sizeN) + scale_color_manual(values= colores) + 
    theme_bw() + ylab("# casos") + ggtitle(main)  + theme(plot.title = element_text(hjust = 0.5)) 
  
  if( !is.na(miny) & !is.na(maxy)){
    p <- p + ggplot2::ylim(c(miny, maxy) )
  }
  
  if( centrados == TRUE){
    p <- p + ggplot2::xlab(c("Dias desde '# casos frontera' ") )
  }else{
    p <- p + ggplot2::xlab(c("Fecha") )
  }
  
  return(p)
}

selectCou <- function( CounTxt, datosfun ){
  cumsum2 <-function(x){ # ya estan acumulados
    # ifelse(test = is.na(x), yes = 0, no = x) %>%  cumsum() 
    x %>% return
  }
  
  x <- data.frame( 
    Fecha = datosfun$Fecha,
    Activos = c (cumsum2( datosfun[ , paste0(CounTxt,"_newCase")] ) 
                 - cumsum2( datosfun[ , paste0(CounTxt,"_newRecover")] ) 
                 - cumsum2(datosfun[ , paste0(CounTxt,"_death")] ) )
    , recuperados = c( cumsum2( datosfun[ , paste0(CounTxt,"_newRecover")] ) )
    , muertos = c( cumsum2( datosfun[ , paste0(CounTxt,"_death")] ) )
    , Totales = c (cumsum2( datosfun[ , paste0(CounTxt,"_newCase")] ) )
  )
  x <- x[ !is.na(x$Activos), ]
  return(x)
}

plotActivos <- 
  function(counTxt, datosfun, ... ){
    x <- selectCou(counTxt, datosfun)
    x$Totales <- rm()
    
    x <- zoo( x =  x[, -1], order.by =x$Fecha ) 
    
    x <- x %>% dygGGany(main = counTxt, ...)
    return(x)
  }

plotSeriesAcum <- 
  function(counTxt, datosfun, datosNoRep, centrados = TRUE,nombreSerieTxt, nCasFrontera = 10 , mainTxt = "",... ){
    datos <- datosfun %>%  select("Fecha", nombreSerieTxt )  %>%  data.frame()
    datosNoRep <- datosNoRep %>%  select("Fecha", nombreSerieTxt )  %>%  data.frame()
    
    IndiceFrontera <- datosNoRep[ , -1] %>%  apply(MARGIN = 2, function(x){x < nCasFrontera }) %>% apply(MARGIN = 2, function(x){all(x == TRUE) }) 
    nombresConDato <- names(as.data.frame(datosNoRep[ , -1]))[ !as.vector(IndiceFrontera)]
    
    datosCentrados <- datos
    if(centrados == TRUE){
      
      datosCentrados <- datosCentrados[ , c("Fecha", nombresConDato)]  
      datosNoRep <- datosNoRep[ , c("Fecha", nombresConDato)]  
      
      datosCentrados[datosCentrados > -1] <- NA
      nfil <- nrow(datosCentrados)
      for( i in names(datosNoRep[,-1])){
        x <- datosNoRep[,i]
        index <- which( x >= nCasFrontera)[1]
        datosCentrados[1:(nfil - index + 1) , i]  <- datos[,i][index:nfil]
      }
      x <- datos[,nombreSerieTxt]
      
      index <- which( datosNoRep[,2] >= nCasFrontera)[1]
      FechaDiaUnoPais1 <- datos$Fecha[index]
      datosCentrados$Fecha <- FechaDiaUnoPais1 + days(1:nfil)
      indice1 <- !datosCentrados[ , -1] %>%  apply(MARGIN = 1, function(x){is.na(x)})  %>% apply(MARGIN = 2, function(x){all(x == TRUE) })
      indice2 <- !datosCentrados[ , -1] %>%  apply(MARGIN = 1, function(x){x <= 0 })  %>% apply(MARGIN = 2, function(x){all(x == TRUE) })
      indice <- indice1 & indice2
      
      x <- zoo( x =  datosCentrados[indice, -1], order.by = datosCentrados$Fecha[indice] ) %>%  xts %>% .[paste0(FechaDiaUnoPais1, "/")]  %>%
        dygGGany(main = mainTxt, centrados = TRUE, ...)     %>%
        ggplotly(dynamicTicks = TRUE) %>%
        layout(
          xaxis = list(
            rangeslider = list(type = "Dias despues de # casos frontera"))
          
        )
    }else{        
      
      indice1 <- !datosCentrados[ , -1] %>%  apply(MARGIN = 1, function(x){is.na(x)})  %>% apply(MARGIN = 2, function(x){all(x == TRUE) })
      indice2 <- !datosCentrados[ , -1] %>%  apply(MARGIN = 1, function(x){x <= 0 })  %>% apply(MARGIN = 2, function(x){all(x == TRUE) })
      indice <- indice1 & indice2
      
      x <- zoo( x =  datosCentrados[indice,nombresConDato], order.by = datosCentrados$Fecha[indice] ) %>% dygGGany(main = mainTxt, ...) %>%
        ggplotly(dynamicTicks = TRUE) %>%
        layout(
          xaxis = list(
            rangeslider = list(type = "date"))
          
        )
    }
    
    
    return(x)
  }

plotSerieNormal <-  function(datfun, CounTxt, mavg_mmed = "" , forMa = 1, mainTxt = "", ...){
  ma <- function(x, n = 5){
    stats::filter(x, filter = rep(1 / n, n), sides = 2)
  }
  # x <- zoo( x =  datosCentrados[indice,-1], order.by = datosCentrados$Fecha[indice] ) %>% dygGGany(main = mainTxt, ...)
  FechaFun <- datfun$Fecha
  
  # print( c(paste0(CounTxt, "_newRecover"), paste0(CounTxt, "_newCase")))
  datfun <- data.frame(datfun)[ ,  c(paste0(CounTxt, "_newCase"), paste0(CounTxt, "_newRecover")) ]
  
  datfun <- datfun  %>% apply(X = ., MARGIN = 2, FUN = function( x) {diff(x, lag = 1) })
  if(mavg_mmed == "Media"){
    for( i in 1:forMa){ # Por si queremos suavizar mas
      datfun <- datfun %>% apply(MARGIN = 2, FUN = ma)
    }
  }
  
  datfun <- datfun %>% data.frame()
  names(datfun) <- c( paste0(CounTxt, "_Casos_Nuevos"), paste0(CounTxt, "_Casos_Recuperados") )
  
  xts::xts(x =  datfun
           , order.by = FechaFun[-1]  ) %>% dygGGany(main = mainTxt, ...) %>%  return()
}


# leo datos ---------------------------------------------------------------
leeDatos <-  function( popBool = FALSE, pop, new3 = new, dea3 = dea, rec3 = data.frame(rec), uci3 = uci, hos3 = hos){
  
  if(popBool){
    for( i in pop$Coun){
      new3[, i] <- new3[, i]/pop[ Coun == i]$hab2018
      dea3[, i] <- dea3[, i]/pop[ Coun == i]$hab2018
      rec3[, i] <- rec3[, i]/pop[ Coun == i]$hab2018
      uci3[, i] <- uci3[, i]/pop[ Coun == i]$hab2018
      hos3[, i] <- hos3[, i]/pop[ Coun == i]$hab2018
    }
    
  }
  
  
  
  lista <- list()
  
  lista$new <- new3 [ , names(new3) ]
  lista$dea <- dea3 [ , names(new3) ]
  lista$rec <- data.frame(rec3) [ , names(new3) ]
  lista$act <- cbind( Fecha = new3$fecha, 
                      data.frame(new3)[ , names(new3)[-1] ] - 
                        data.frame(dea3)[ , names(new3)[-1]] - 
                        data.frame(rec3)[ rec3$fecha %in% new3$fecha , names(new3)[-1]])
  names(new3)
  names(rec3)
  lista$uci <- uci3[ , names(new3) ]
  lista$hos <- hos3[ , names(new3) ]
  
  
  nombresPaises <- c( "Total", "Andalucia", "Aragon", "Asturias", "Canarias", "Cantabria", "Castilla_y_Leon"
                      , "Castilla_la_Mancha", "Catalunya","Ceuta", "Valencia", "Extremadura", "Galicia"
                      , "Islas_Baleares", "Rioja", "Madrid", "Melilla", "Murcia", "Navarra", "Euskadi" )
  
  lista$nombresPaises <- nombresPaises
  
  names(lista$new) <- c("Fecha", nombresPaises)
  names(lista$dea) <- c("Fecha", nombresPaises)
  names(lista$rec) <- c("Fecha", nombresPaises)
  names(lista$act) <- c("Fecha", nombresPaises)
  names(lista$uci) <- c("Fecha", nombresPaises)
  names(lista$hos) <- c("Fecha", nombresPaises)
  
  datosNew     <- lista$new[ , c("Fecha", nombresPaises)] 
  datosRecover <- lista$rec[ , c("Fecha", nombresPaises)]
  datosDeath   <- lista$dea[ , c("Fecha", nombresPaises)]
  datosUci     <- lista$uci[ , c("Fecha", nombresPaises)]
  datosHospi   <- lista$hos[ , c("Fecha", nombresPaises)]
  
  names(datosNew)[-1]     <- paste0(names(datosNew)[-1] , "_newCase")
  names(datosRecover)[-1] <- paste0(names(datosRecover)[-1] , "_newRecover")
  names(datosDeath)[-1]   <- paste0(names(datosDeath)[-1] , "_death")
  names(datosUci)[-1]     <- paste0(names(datosUci)[-1] , "_uci")
  names(datosHospi)[-1]   <- paste0(names(datosHospi)[-1] , "_hospi")
  
  datosAll <- merge(x = datosNew, y = datosRecover, by = "Fecha", all = TRUE)
  datosAll <- merge(x = datosAll, y = datosDeath, by = "Fecha", all = TRUE)
  datosAll <- merge(x = datosAll, y = datosUci, by = "Fecha", all = TRUE)
  datosAll <- merge(x = datosAll, y = datosHospi, by = "Fecha", all = TRUE)
  datosAll <- datosAll[ order(datosAll$Fecha), ]
  lista$datosAll <- datosAll
  
  return(lista)
}
listaSinPop <- leeDatos()
listaConPop <- leeDatos(popBool = TRUE, pop = pop) 
nombresPaises <- listaSinPop$nombresPaises

# listaSinPop$new %>% names
# encabezado --------------------------------------------------------------
header <- dashboardHeader(title = "Covid-19 Espanya")


# barra lateral -----------------------------------------------------------
sidebar <- dashboardSidebar( # se conecta con dashboardBody
  sidebarMenu(id = "Menu",
              menuItem("Mapa Provincias", tabName = "dashboard0", icon = icon("atlas")),
              menuItem("Activos detectados", tabName = "dashboard1", icon = icon("bed")),
              menuItem("Muertos detectados", tabName = "dashboard2", icon = icon("skull")),
              menuItem("Contagiados detectados", tabName = "dashboard3", icon = icon("ambulance")),
              menuItem("Recuperados detectados", tabName = "dashboard4", icon = icon("stethoscope")),
              menuItem("Ucis ocupadas", tabName = "dashboard6", icon = icon("procedures")),
              menuItem("Hospitalizados", tabName = "dashboard7", icon = icon("hospital")),
              menuItem("Notas y Descargas", tabName = "dashboard5", icon = icon("book")),
              
              numericInput(inputId = "nGrupos", label = "# grupos cluster", value = 3), 
              
              selectInput(inputId = "Pais1", label = "Comunidad  1",
                          choices =  nombresPaises, selected = "Madrid" ),
              selectInput(inputId = "Pais2", label = "Comunidad  2",
                          choices =  nombresPaises, selected = "Catalunya"  ),
              selectInput(inputId = "Pais3", label = "Comunidad  3",
                          choices =  nombresPaises, selected = "Euskadi"  ),
              selectInput(inputId = "Pais4", label = "Comunidad  4",
                          choices =  nombresPaises, "Aragon" ),
              
              
              numericInput(inputId = "nCasosFrontera", label = "# Casos Frontera Para el centrado", value = 10),
              
              
              checkboxInput(inputId = "RepesoPob", label = ("Repesa por poblacion"), value = TRUE),
              
              checkboxInput(inputId = "CentradosVar", label = ("Centra los datos por el primer pais"), value = TRUE)
              
              
              
  ) )


# cuerpo ------------------------------------------------------------------
body <- dashboardBody( # se conecta con dashboardSidebar   
  tags$head(
    tags$style(
      HTML('
          /* body */
          .content-wrapper, .right-side {
          background-color: #ffffff;
          }

          ') ) ),
  #     tags$style(type="text/css", 
  # )
  
  
  tabItems(
    
    tabItem(tabName = "dashboard0",
            fluidRow( 
              h1("Mapa"),
              h4("Los datos que se muestran es la variacion de la ultima semana con la excepcion de activos. Lo grupos se computan con las variacioens de muertos, uci,hospitalizados y el total de activos relativos a la poblacion"),
              # withSpinner(  plotlyOutput('plotDeaTodosCen') )  ,
              withSpinner(  leafletOutput('clusterMap') )
              , withSpinner(  dataTableOutput('cluster') )
             
            )
    ),
    
    # First tab content
    tabItem(tabName = "dashboard1",
            
            
            br(),h1("Activos"),br(),
            h4("La fecha de partida esta centrada en el Comunidad  1 por defecto a partir de # Casos Frontera (Por defecto 10). 
                   Puedes deseleccionarlo en la caja "),
            
            splitLayout(cellWidths = c('5%','90%', '5%'),
                        '',
                        withSpinner(  plotlyOutput('plotActTodos') ),
                        ''
            ),
            
            hr(), br(),h1("Elige valores de los ejes en las comparaciones"),br(),
            splitLayout( 
              cellWidths = c('10%','40%','40%', '10%') , 
              ' ',
              box(numericInput(inputId = "minimoYAct", label = "Minimo de # Casos", value = NA), background = "blue"),
              
              box(numericInput(inputId = "maximoYAct", label = "Maximo de # Casos", value = NA), background = "blue") 
              
            ),
            
            
            br(),h1("Comunidad 1 Vs Comunidad 2"),br(),
            
            splitLayout(cellWidths = c('45%','10%', '45%'),
                        withSpinner(  plotlyOutput('plotAct1') ),
                        '',
                        withSpinner(plotlyOutput('plotAct2'))
            ),
            
            
    ),
    
    tabItem(tabName = "dashboard2",
            fluidRow( 
              # withSpinner(  plotlyOutput('plotDeaTodosCen') )  ,
              withSpinner(  plotlyOutput('plotDeaTodos') )   
            )
    ),
    
    tabItem(tabName = "dashboard3",
            # withSpinner(  plotlyOutput('plotNewTodosCen') ) ,
            withSpinner(  plotlyOutput('plotNewTodos') ) 
    ),
    
    
    tabItem(tabName = "dashboard4",
            
            br(),h1(""),br(),
            
            # splitLayout(cellWidths = c('5%','90%', '5%'),
            #             '',
            #             withSpinner(  plotlyOutput('plotRecTodosCen') ),
            #             ''
            # ),
            
            splitLayout(cellWidths = c('5%','90%', '5%'),
                        '',
                        withSpinner(  plotlyOutput('plotRecTodos') ),
                        ''
            ),
            
            hr(), br(),h1("Elige valores de los ejes en las comparaciones y correcciones"),br(),
            splitLayout( 
              cellWidths = c('10%','30%','30%', '30%') , 
              ' ',
              box(numericInput(inputId = "minimoYRec", label = "Minimo de # Casos", value = NA), background = "blue"),
              
              box(numericInput(inputId = "maximoYRec", label = "Maximo de # Casos", value = NA), background = "blue") ,
              
              box(selectInput(inputId = "tipCorRec", label = "Correccion", choices = c("Ninguna", "Media"), selected = "Media"), background = "blue"),
              
              br()
            ),
            
            
            br(),h1("Comunidad 1 Vs Comunidad  2"),br(),
            h2("Esta curva es util para ver si el Comunidad  se esta recuperando"), br(),
            
            splitLayout(cellWidths = c('45%','10%', '45%'),
                        withSpinner(  plotlyOutput('plotRec1') ),
                        '',
                        withSpinner(plotlyOutput('plotRec2'))
            ),
            
            
    ),
    
    tabItem(tabName = "dashboard6",
            fluidRow( h1("Personas en UCI"),
                      # withSpinner(  plotlyOutput('plotNewTodosCen') ) ,
                      withSpinner(  plotlyOutput('plotUci') )  )
    ),
    
    tabItem(tabName = "dashboard7",
            h1("Personas hospitalizados"),
            # withSpinner(  plotlyOutput('plotNewTodosCen') ) ,
            withSpinner(  plotlyOutput('plotHos') ) 
    ),
    
    tabItem(tabName = "dashboard5",
            h2("Origen de los datos"), br()
            , h4("Los datos  a nivel comunidad son exportados del respositorio del Instituto de Salud carlos tercero (https://cnecovid.isciii.es/covid19/resources/agregados.csv)")
            , br()
            , h4("Los datos de poblacion son descargados del INE")
            , br()
            , h2("Datos de activos")
            , br()
            , h4("Son datos que nos permiten conocer en que estado se encuentra nuestro Comunidad  y ver si las curvas de datos activos(detectados), tienen un nivel aceptable")
            , br()
            , h2("Datos de Contagiados"), br()
            , br()
            , h4("Un buen dato de contagiados es cuando la curva de recuperados esta por encima de la de nuevos datos")
            , br()
            , h2("Controles")
            , br()
            , h4("Los controles 'Minimo de # Casos' y 'Maximo de # Casos' requieren un ajuste manual. Por degracia no encontre una manera de hacer mejor")
            , br()
            , h2("Acentos")
            , br()
            , h4("Evite los acentos, para evitar problemas en la subida de datos")
            , br()
            , h2("Descargas")
            , br()
            ,  sidebarLayout(
              
              # Sidebar panel for inputs ----
              sidebarPanel(
                
                # Input: Choose dataset ----
                selectInput("dataset", "Choose a dataset:",
                            choices = c(
                              "TodosDatosISCIII"
                              , "NuevosCasosISCIII"
                              , "MuertosISCIII"
                              , "RecuperadosISCIII"
                              , "ActivosISCIII"
                              , "TodosDatosPorHabitanteISCIII"
                              , "NuevosCasosPorHabitanteISCIII"
                              , "MuertosPorHabitanteISCIII"
                              , "RecuperadosPorHabitanteISCIII"
                              , "ActivosPorHabitanteISCIII"
                              , "PoblacionBancoMundial"
                              , "DatosOurWorldInData"
                              , "DatosTestOWD"
                            )),
                
                # Button
                downloadButton(outputId = "downloadData", label = "Descarga")
                
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                
                dataTableOutput("tableDescargas")
                
              )
              
            )
            
            
    ),
    
    # tabItem(tabName = "dashboard6",
    #         fluidRow( box(numericInput(inputId = "nCasosTest", label = "Mostrar n primeros", value = 20), background = "blue")
    #                   , infoBox("% paises informando test"
    #                             , value = paste0 ( round( nrow(datosOxfordTest) /  ncol(listaSinPop$dea) * 100) , "%")
    #                             , icon = icon("chart-bar"))
    #         ), 
    #         withSpinner(  plotlyOutput('plotTest') ) ,
    #         withSpinner(  plotlyOutput('plotTest1000h') ) ,
    #         withSpinner(  plotlyOutput('plotCasosXtest') ) ,
    #         dataTableOutput("TestTable")
    # ),
    
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)  


# ui ----------------------------------------------------------------------
ui <- dashboardPage(
  header ,
  sidebar,
  body
)




# sever -------------------------------------------------------------------
server <- function(input, output) {
  
  
  # tag activos -------------------------------------------------------------
  output$plotAct1 <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosAll <- listaConPop$datosAll
      }else{
        datosAll <- listaSinPop$datosAll
      }
      plotActivos(counTxt = input$Pais1,datosfun = datosAll
                  , miny = as.numeric(input$minimoYAct), maxy = as.numeric(input$maximoYAct) )
    } )
  
  output$plotAct2 <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosAll <- listaConPop$datosAll
      }else{
        datosAll <- listaSinPop$datosAll
      }
      
      plotActivos(counTxt = input$Pais2,datosfun = datosAll 
                  , miny = as.numeric(input$minimoYAct), maxy = as.numeric(input$maximoYAct) )
    } )
  
  
  output$plotActTodos  <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosPlotActTodos <- listaConPop$act
      }else{
        datosPlotActTodos <- listaSinPop$act
      }
      datosNoRep <- listaSinPop$act
      plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = input$CentradosVar
                     ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
                     , mainTxt = "Activos", nCasFrontera = input$nCasosFrontera)
      
    } )
  
  # muertos plots -----------------------------------------------------------
  output$plotDeaTodos  <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosPlotActTodos <- listaConPop$dea
      }else{
        datosPlotActTodos <- listaSinPop$dea
      }
      datosNoRep <- listaSinPop$dea
      plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = input$CentradosVar
                     ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
                     , mainTxt = "Muertos", nCasFrontera = input$nCasosFrontera)
      
    } )
  
  
  # output$plotDeaTodosCen  <-
  #     renderPlotly( {
  #         
  #         if( input$RepesoPob){
  #             datosPlotActTodos <- listaConPop$dea
  #         }else{
  #             datosPlotActTodos <- listaSinPop$dea
  #         }
  #         datosNoRep <- listaSinPop$dea
  #         plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = TRUE
  #                    ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
  #                    , mainTxt = "Muertos Centrados", nCasFrontera = input$nCasosFrontera)
  #         
  #     } )
  
  
  # Datos Total contagios detectados ----------------------------------------
  output$plotNewTodos  <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosPlotActTodos <- listaConPop$new
      }else{
        datosPlotActTodos <- listaSinPop$new
      }
      datosNoRep <- listaSinPop$new
      plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = input$CentradosVar
                     ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
                     , mainTxt = "Nuevos casos", nCasFrontera = input$nCasosFrontera)
      
    } )
  
  
  output$plotUci  <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosPlotActTodos <- listaConPop$uci
        # datosPlotActTodos %>% sapply(X = ., FUN = function())
      }else{
        datosPlotActTodos <- listaSinPop$uci
      }
      datosNoRep <- listaSinPop$new
      plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = input$CentradosVar
                     ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
                     , mainTxt = "Personas en UCI", nCasFrontera = input$nCasosFrontera)
      
    } )
  
  output$plotHos  <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosPlotActTodos <- listaConPop$hos
      }else{
        datosPlotActTodos <- listaSinPop$hos
      }
      datosNoRep <- listaSinPop$new
      plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = input$CentradosVar
                     ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
                     , mainTxt = "Personas hospitalizadas", nCasFrontera = input$nCasosFrontera)
      
    } )
  
  output$cluster  <-
    renderDataTable( {
      datosCluster <- 
        data.frame( 
          # new = as.vector( t( listaConPop$new[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) )
          dea = as.vector( t( listaConPop$dea[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaConPop$dea[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          # , rec = as.vector( t( listaConPop$rec[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) )
          , act = as.vector( t( listaConPop$act[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) )# -
            # as.vector( t( listaConPop$act[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          , uci = as.vector( t( listaConPop$uci[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaConPop$uci[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          , hos = as.vector( t( listaConPop$hos[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaConPop$hos[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha) - days(7), -(1:2) ]) )
        )
      cluster <- kmeans(x = datosCluster, centers = input$nGrupos)
      datosCluster$cluster <- cluster$cluster
      datosCluster$Provincia <- names( listaConPop$new[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) 
      datosCluster <- datosCluster[ order(datosCluster$cluster),]
      datosCluster
      datatable(  datosCluster )
    } )
  
  
  output$clusterMap  <-
    renderLeaflet( {
      datosCluster <- 
        data.frame( 
          # new = as.vector( t( listaConPop$new[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) )
          dea = as.vector( t( listaConPop$dea[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaConPop$dea[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          # , rec = as.vector( t( listaConPop$rec[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) )
          , act = as.vector( t( listaConPop$act[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) ) # -
            # as.vector( t( listaConPop$act[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          , uci = as.vector( t( listaConPop$uci[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaConPop$uci[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          , hos = as.vector( t( listaConPop$hos[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaConPop$hos[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha) - days(7), -(1:2) ]) )
        )
      cluster <- kmeans(x = datosCluster, centers = input$nGrupos)
      datosCluster$cluster <- cluster$cluster
      datosCluster$NAME_1 <- names( listaConPop$new[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) 
      datosCluster <- datosCluster[ order(datosCluster$cluster),]
      # datosCluster
      
      datosClusterNetos <- 
        data.frame( 
          # new = as.vector( t( listaSinPop$new[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha), -(1:2) ]) )
          dea = as.vector( t( listaSinPop$dea[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaSinPop$dea[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          # , rec = as.vector( t( listaSinPop$rec[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha), -(1:2) ]) )
          , act = as.vector( t( listaSinPop$act[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha), -(1:2) ]) ) # -
            # as.vector( t( listaSinPop$act[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          , uci = as.vector( t( listaSinPop$uci[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaSinPop$uci[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha) - days(7), -(1:2) ]) )
          , hos = as.vector( t( listaSinPop$hos[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha), -(1:2) ]) ) -
            as.vector( t( listaSinPop$hos[ listaSinPop$datosAll$Fecha == max(listaSinPop$datosAll$Fecha) - days(7), -(1:2) ]) )
        )
      datosClusterNetos$cluster <- cluster$cluster
      datosClusterNetos$NAME_1 <- names( listaConPop$new[ listaConPop$datosAll$Fecha == max(listaConPop$datosAll$Fecha), -(1:2) ]) 
      datosClusterNetos <- datosClusterNetos[ order(datosCluster$cluster),]
      
      if( input$RepesoPob){
        datosCl <- datosCluster
      }else{
        datosCl <- datosClusterNetos
      }
      
      shp_df <- tigris::geo_join(shp, datosCl , by = "NAME_1")
      print( shp_df@data )
      
      bins <- c(0 , shp_df@data$cluster %>% unique %>% .[order(.)], Inf)
      pal <- colorFactor(brewer.pal(n = 11, name ="Spectral") , shp_df@data$cluster)
      if( input$RepesoPob){
        tooltip <- sprintf("<strong>%s</strong><br/>Cluster: %.f<br/>Activos: %.4f<br/>Muertos: %.6f<br/>UCI: %.6f<br/>Hospitalizados: %.6f"
                           ,shp_df@data$NAME_1
                           ,shp_df@data$cluster
                           ,shp_df@data$act
                           ,shp_df@data$dea
                           ,shp_df@data$uci
                           ,shp_df@data$hos
        ) %>% lapply(htmltools::HTML)
      }else{
        tooltip <- sprintf("<strong>%s</strong><br/>Cluster: %.f<br/>Activos: %.f<br/>Muertos: %.f<br/>UCI: %.f<br/>Hospitalizados: %.f"
                           ,shp_df@data$NAME_1
                           ,shp_df@data$cluster
                           ,shp_df@data$act
                           ,shp_df@data$dea
                           ,shp_df@data$uci
                           ,shp_df@data$hos
        ) %>% lapply(htmltools::HTML)
      }
      
      
      
      leaflet() %>% addTiles() %>%
        addPolygons(
          data = shp_df,
          stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
          color = "grey",
          fillColor = ~pal( cluster)
          # layerId = shp_df@data$Provincia
          ,label = tooltip 
          )
      
        
                        
    } )
  
  # output$plotNewTodosCen  <-
  #     renderPlotly( {
  #         
  #         if( input$RepesoPob){
  #             datosPlotActTodos <- listaConPop$new
  #         }else{
  #             datosPlotActTodos <- listaSinPop$new
  #         }
  #         datosNoRep <- listaSinPop$new
  #         plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = TRUE
  #                    ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
  #                    , mainTxt = "Nuevos casos Centrados", nCasFrontera = input$nCasosFrontera)
  #         
  #     } )
  
  
  
  # recuperados -------------------------------------------------------------
  
  output$plotRecTodos  <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosPlotActTodos <- listaConPop$rec
      }else{
        datosPlotActTodos <- listaSinPop$rec
      }
      datosNoRep <- listaSinPop$rec
      plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = input$CentradosVar
                     ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
                     , mainTxt = "Recuperados", nCasFrontera = input$nCasosFrontera)
      
    } )
  
  
  # output$plotRecTodosCen  <-
  #     renderPlotly( {
  #         
  #         if( input$RepesoPob){
  #             datosPlotActTodos <- listaConPop$rec
  #         }else{
  #             datosPlotActTodos <- listaSinPop$rec
  #         }
  #         datosNoRep <- listaSinPop$rec
  #         plotSeriesAcum(datosfun = datosPlotActTodos, datosNoRep = datosNoRep, centrados = TRUE
  #                    ,  nombreSerieTxt = c(input$Pais1, input$Pais2, input$Pais3, input$Pais4)
  #                    , mainTxt = "Recuperados Centrados", nCasFrontera = input$nCasosFrontera)
  #         
  #     } )
  
  
  output$plotRec1  <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosPlotActTodos <- listaConPop$datosAll
      }else{
        datosPlotActTodos <- listaSinPop$datosAll
      }
      # datosNoRep <- listaSinPop$datosAll
      plotSerieNormal(datfun = datosPlotActTodos, CounTxt = input$Pais1, mainTxt = input$Pais1, mavg_mmed = input$tipCorRec
                      , miny = input$minimoYRec , maxy = input$maximoYRec )
      
    } )
  
  output$plotRec2  <-
    renderPlotly( {
      
      if( input$RepesoPob){
        datosPlotActTodos <- listaConPop$datosAll
      }else{
        datosPlotActTodos <- listaSinPop$datosAll
      }
      # datosNoRep <- listaSinPop$datosAll
      plotSerieNormal(datfun = datosPlotActTodos, CounTxt = input$Pais2, mainTxt = input$Pais2, mavg_mmed = input$tipCorRec 
                      , miny = input$minimoYRec , maxy = input$maximoYRec )
      
    } )
  
  # plot test  --------------------------------------------------------------
  # output$plotTest <- 
  #   renderPlotly(({
  #     pTot <- ggplot(data= datosOxfordTest[ order(Test_Totales, decreasing = TRUE)][1:input$nCasosTest]
  #                    , aes(x=reorder(Coun, Test_Totales), y= Test_Totales)) +
  #       geom_bar(stat="identity", fill = "steelblue") + coord_flip()  + theme_bw() + xlab("Pais") + ylab("# Test Covid") +
  #       ggtitle("Tests Totales")
  #     pTot %>% ggplotly()
  #   }))
  # 
  # output$plotTest1000h <- 
  #   renderPlotly(({
  #     pProp <- ggplot(data= datosOxfordTest[ order(TestPorMilHab, decreasing = TRUE)][1:input$nCasosTest]
  #                     , aes(x=reorder(Coun, TestPorMilHab), y= round(TestPorMilHab, 2) ) ) +
  #       geom_bar(stat="identity", fill = "steelblue") + coord_flip()  + theme_bw() + xlab("Pais") + ylab("Test Covid x 1000 hab") + 
  #       ggtitle("Test de Covid por mil habitantes") 
  #     pProp %>% ggplotly()
  #   }))
  # 
  # output$plotCasosXtest <- 
  #   renderPlotly(({
  #     pcast <- ggplot(data= datosOxfordTest[ order(CasosPorTest, decreasing = TRUE)][1:input$nCasosTest]
  #                     , aes(x=reorder(Coun, CasosPorTest), y= round(CasosPorTest, 3) ) ) +
  #       geom_bar(stat="identity", fill = "steelblue") + coord_flip()  + theme_bw() + xlab("Pais") + ylab("Casos por Test") + 
  #       ggtitle("Casos de Covid por test realizado") 
  #     pcast %>% ggplotly()
  #   }))
  # 
  # 
  # output$TestTable <- 
  #   renderDataTable(({
  #     DT::datatable(datosOxfordTest)
  #   }))
  # 
  # Descargas ---------------------------------------------------------------
  datasetInput <- reactive({
    switch(input$dataset,
           "TodosDatosISCIII" = listaSinPop$datosAll,
           "NuevosCasosISCIII" = listaSinPop$new,
           "MuertosISCIII" = listaSinPop$dea,
           "RecuperadosISCIII" = listaSinPop$rec,
           "ActivosISCIII" = listaSinPop$act,
           "TodosDatosPorHabitanteISCIII" = listaConPop$new,
           "NuevosCasosPorHabitanteISCIII" = listaConPop$new,
           "MuertosPorHabitanteISCIII" = listaConPop$new,
           "RecuperadosPorHabitanteISCIII" = listaConPop$new,
           "ActivosPorHabitanteISCIII" = listaConPop$new,
           "PoblacionINE" = Pop
    )
  })
  
  # Table of selected dataset ----
  output$tableDescargas <- renderDataTable({ 
    n <- ifelse( test = ncol(datasetInput()) > 5
                 , yes =  5
                 , no = ncol(datasetInput())
    )
    DT::datatable(
      datasetInput()[,  ]
    )
  }) 
  
  # Downloadable csv of selected dataset ----
  output$downloadData <-  downloadHandler(
    filename = function() {
      paste0(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      fwrite(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
} 

shinyApp(ui, server)


# output$Tabla <- renderDataTable({
#     dataAct1 <- datosAll[ , c("Fecha", paste0(input$Pais1, "_newCase")
#                               , paste0(input$Pais1, "_newRecover")
#                               , paste0(input$Pais1, "_death") )] 
#     
#     dataAct1$Fecha <- dataAct1$Fecha %>% format("%Y-%m-%d")
#     datatable(dataAct1, rownames = F)
# } )

