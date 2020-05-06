#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://momo.isciii.es/public/momo/dashboard/momo_dashboard.html#nacional
rm(list =ls());gc()

options(scipen = 100) 
options(encoding = 'UTF-8')

# if(!require("pacman")){ # lo quito para el subirlo
#     install.packages("pacman")
# }
# pacman::p_load(data.table, dplyr, zoo, forecast, lubridate, dygraphs, RColorBrewer,xts
               # , shiny, shinydashboard, htmlwidgets, DT, plotly, reshape2, shinycssloaders)

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
# seismicRoll


# parametros --------------------------------------------------------------
fecIni <- 20200101
n <- 0 # Puede ser que el dato del dia no se fiable.
cat("Esta es la fecha tope del script", as.character(format( Sys.Date() - days(n), "%d-%m-%Y") ) , "\n")
# setwd("C:/Users/hermesh/Dropbox/Investigaciones/Coronavirus/")
# setwd("C:/Users/herme///Dropbox/Investigaciones/Coronavirus/")


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
    x <- data.frame( 
        Activos = c (cumsum( datosfun[ , paste0(CounTxt,"_newCase")] ) 
                     - cumsum( datosfun[ , paste0(CounTxt,"_newRecover")] ) 
                     - cumsum(datosfun[ , paste0(CounTxt,"_death")] ) )
        , recuperados = c( cumsum( datosfun[ , paste0(CounTxt,"_newRecover")] ) )
        , muertos = c( cumsum( datosfun[ , paste0(CounTxt,"_death")] ) )
        , Totales = c (cumsum( datosfun[ , paste0(CounTxt,"_newCase")] ) )
    )
    return(x)
}

plotActivos <- 
    function(counTxt, datosfun, ... ){
        x <- selectCou(counTxt, datosfun)

        x$Totales <- rm()
        x <- zoo( x =  x, order.by = datosfun$Fecha ) %>% dygGGany(main = counTxt, ...)
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

    print( c(paste0(CounTxt, "_newRecover"), paste0(CounTxt, "_newCase")))
    datfun <- data.frame(datfun)[ ,  c(paste0(CounTxt, "_newCase"), paste0(CounTxt, "_newRecover")) ]

    if(mavg_mmed == "Media"){
        for( i in 1:forMa){ # Por si queremos suavizar mas
            datfun <- datfun %>% apply(MARGIN = 2, FUN = ma)
        }
    }
    
    datfun <- datfun %>% data.frame()
    names(datfun) <- c( paste0(CounTxt, "_Casos_Nuevos"), paste0(CounTxt, "_Casos_Recuperados") )

    xts::xts(x =  datfun
             , order.by = FechaFun  ) %>% dygGGany(main = mainTxt, ...) %>%  return()
}
    

# leo datos ---------------------------------------------------------------

Pop <- fread(input = "Population.csv", integer64 = "double")
Pop$Coun <- Pop$`Country Name` %>%  gsub(pattern = " ", replacement = "_", .) %>% tolower

Pop$Coun[ Pop$Coun %>%  grepl(pattern = "sahara")]
Pop[ Coun == "russian_federation"]$Coun <- "russia"
Pop[ Coun == "brunei_darussalam"]$Coun <- "brunei"
Pop[ Coun == "czech_republic"]$Coun<- "czechia"
Pop[ Coun == "korea"] $Coun<- "korea,_south"
Pop[ Coun == "kyrgyz_republic"]$Coun<- "kyrgyzstan"
Pop[ Coun == "lao_pdr"]$Coun<- "laos"
Pop[ Coun == "st._kitts_and_nevis"] $Coun<- "saint_kitts_and_nevis"
Pop[ Coun == "st._lucia"]$Coun<- "saint_lucia"
Pop[ Coun == "st._vincent_and_the_grenadines"] $Coun<- "saint_vincent_and_the_grenadines"
Pop[ Coun == "slovak_republic"]$Coun<- "slovakia"
Pop[ Coun == "syrian_arab_republic"] $Coun<- "syria"
Pop[ Coun == "north_america"]$Coun<- "us"
Pop[ Coun == "sub-saharan_africa"]$Coun<- "western_sahara"
Pop[ Coun == ""]$Coun<- ""
Pop <- Pop[, .(Coun, Hab2018)] 

datosOxford <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
Aux <- datosOxford[ !is.na(total_tests), .( Test_Totales = max(total_tests, na.rm = TRUE)
                                            ,  Ultima_actualizacion = max(date))
                    , by = .(Coun = tolower(location) )][ 
  !is.na(Test_Totales) & !is.infinite(Test_Totales)][order(Test_Totales, decreasing = TRUE)]
# Pop$Coun[ Pop$Coun %>%  grepl(pattern = "tai")]
Aux[ , Coun := gsub(pattern = " ", replacement = "_", Coun)]
Aux[ Coun == "czech_republic", Coun := "czechia"]
Aux[ Coun == "hong_kong", Coun := "hong_kong_sar"]
Aux[ Coun == "south_korea", Coun := "korea,_south"]
# Aux[ Coun == "taiwan", Coun := ""] #No reconocido, politica
datosOxfordTest <- merge(x = Aux, y = Pop
                         , by = "Coun", all.x = TRUE, all.y = FALSE )
if( nrow( datosOxfordTest[ is.na(Hab2018)]) > 0 ){
  warning("CH: actualizar nombres de :", paste(datosOxfordTest[ is.na(Hab2018)]$Coun, collapse = ", "))
}
datosOxfordTest[ , TestPorMilHab := Test_Totales/Hab2018*1000]
datosOxfordTest <- datosOxfordTest[ !duplicated(Coun)]
datosOxfordTest[ order(TestPorMilHab, decreasing = TRUE), rankTest1000h := 1:nrow(datosOxfordTest)]
datosOxfordTest[ , .(Coun, Ultima_actualizacion, rankTest1000h, Test_Totales, TestPorMilHab)]

new <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
                 , varTxt = "newCases")
dea <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
                 , varTxt = "newCases")
rec <- funcMelt(dataFun = fread(input = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
                 , varTxt = "newCases")


leeDatos <- function( popBool = FALSE, pop = pop, new3 = new, dea3 = dea, rec3 = rec){
    
    new3$Coun <- gsub(pattern = " ", replacement = "_", new3$Coun) %>% tolower
    dea3$Coun <- gsub(pattern = " ", replacement = "_", dea3$Coun) %>% tolower
    rec3$Coun <- gsub(pattern = " ", replacement = "_", rec3$Coun) %>% tolower

    new4 <- new3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
    dea4 <- dea3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
    rec4 <- rec3[ !duplicated( paste(Coun, Prov, Fecha ) ) ][ order(Coun, Prov, Fecha, decreasing = FALSE )]
    
    if((popBool) == TRUE){
     dataHabFun <- function(data){
         data <- merge(data, Pop, by = "Coun")   
         
         if( nrow( data[ is.na(Hab2018)]) > 0 ){
           warning("CH: actualizar nombres de :", unique( paste(data[ is.na(Hab2018)]$Coun), collapse = ", "))
         }
         
         data[ , newCases := newCases / Hab2018]
         data[ , Hab2018 := rm() ]

         return(data)
     }
     new4 <- dataHabFun(data = copy(new4) )
     dea4 <- dataHabFun(data = copy(dea4) )
     rec4 <- dataHabFun(data = copy(rec4) )
    }    
    
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
    act5 <-cbind( Fecha = new5$Fecha,  new5[ , -1] - dea5[ , -1] - rec5[ , -1]) 
    
    
    datos <- new5 %>%  data.frame()
    
    foo <- function(x){ # quito los negativos  
        x <- x %>% diff %>% c(0,.) 
        # %>% ifelse(test = . < 0, yes = 0, no = .)
        return(x)
    }
    DTnames <- names(datos)[names(datos) !=  "Fecha"]
    datos <- data.table(datos)
    datos[ , (DTnames) := lapply( .SD, foo ), .SDcol = DTnames ]
    datos
    datosNew <- data.frame(datos[ Fecha > ymd(fecIni) & Fecha <= Sys.Date() ])
    # datosNew <- datos 
    
    datos <- rec5  %>%  data.frame()
    datos <- data.table(datos)
    DTnames <- names(datos)[names(datos) !=  "Fecha"]
    datos[ , (DTnames) := lapply( .SD, foo ), .SDcol = DTnames ]
    datosRecover <- data.frame(datos[ Fecha > ymd(fecIni) & Fecha <= Sys.Date() ])
    
    
    datos <- dea5 %>%  data.frame()
    datos <- data.table(datos)
    DTnames <- names(datos)[names(datos) !=  "Fecha"]
    datos[ , (DTnames) := lapply( .SD, foo ), .SDcol = DTnames ]
    datosDeath <- data.frame(datos[ Fecha > ymd(fecIni) & Fecha <= Sys.Date() ])
    
    nombresPaises <- names(datosNew)[ names(datosNew) %in% names(datosRecover)]
    nombresPaises <- nombresPaises[nombresPaises %in% names(datosDeath)]
    
    
    datosNew <- datosNew[ , nombresPaises] 
    datosRecover <- datosRecover[ , nombresPaises]
    datosDeath <- datosDeath[ , nombresPaises]
    
    names(datosNew)[-1] <- paste0(names(datosNew)[-1] , "_newCase")
    names(datosRecover)[-1] <- paste0(names(datosRecover)[-1] , "_newRecover")
    names(datosDeath)[-1] <- paste0(names(datosDeath)[-1] , "_death")
    
    datosAll <- merge(x = datosNew, y = datosRecover, by = "Fecha", all = TRUE)
    datosAll <- merge(x = datosAll, y = datosDeath, by = "Fecha", all = TRUE)
    datosAll <- datosAll[ order(datosAll$Fecha), ]
    
    
    
    return(list( datosAll = datosAll, nombresPaises = nombresPaises, new = new5, dea = dea5, rec = rec5, act = act5))
    
}

listaSinPop <- leeDatos()
listaConPop <- leeDatos(popBool = TRUE) 
nombresPaises <- listaSinPop$nombresPaises


# encabezado --------------------------------------------------------------
header <- dashboardHeader(title = "Covid19")


# barra lateral -----------------------------------------------------------
sidebar <- dashboardSidebar( # se conecta con dashboardBody
    sidebarMenu(id = "Menu",
                menuItem("Activos detectados", tabName = "dashboard1", icon = icon("bed")),
                menuItem("Muertos detectados", tabName = "dashboard2", icon = icon("skull")),
                menuItem("Contagiados detectados", tabName = "dashboard3", icon = icon("ambulance")),
                menuItem("Recuperados detectados", tabName = "dashboard4", icon = icon("stethoscope")),
                menuItem("Test", tabName = "dashboard6", icon = icon("vial")),
                menuItem("Notas y Descargas", tabName = "dashboard5", icon = icon("book")),
                
                selectInput(inputId = "Pais1", label = "Pais 1",
                            choices =  nombresPaises[-1], selected = "spain" ),
                selectInput(inputId = "Pais2", label = "Pais 2",
                            choices =  nombresPaises[-1], selected = "italy"  ),
                selectInput(inputId = "Pais3", label = "Pais 3",
                            choices =  nombresPaises[-1], selected = "france"  ),
                selectInput(inputId = "Pais4", label = "Pais 4",
                            choices =  nombresPaises[-1], "united_kingdom" ),
                 
                
                numericInput(inputId = "nCasosFrontera", label = "# Casos Frontera Para el centrado", value = 10),
                
                
                checkboxInput(inputId = "RepesoPob", label = ("Repesa por poblacion 2018"), value = TRUE),
                
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
        
        # First tab content
        tabItem(tabName = "dashboard1",

                
                br(),h1("Activos"),br(),
                h4("La fecha de partida esta centrada en el pais 1 por defecto a partir de # Casos Frontera (Por defecto 10). 
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

                
                br(),h1("Pais 1 Vs pais 2"),br(),
                
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
                
                br(),h1("Contagiados"),br(),
                h4("La fecha de partida esta centrada en el pais 1 por defecto a partir de # Casos Frontera (Por defecto 10). 
                   Puedes deseleccionarlo en la caja"),
                
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
                
                
                br(),h1("Pais 1 Vs Pais 2"),br(),
                h2("Esta curva es util para ver si el pais se esta recuperando"), br(),
                
                splitLayout(cellWidths = c('45%','10%', '45%'),
                            withSpinner(  plotlyOutput('plotRec1') ),
                            '',
                            withSpinner(plotlyOutput('plotRec2'))
                ),
                
                
        ),
        
        tabItem(tabName = "dashboard5",
                h2("Origen de los datos"), br()
                , h4("Los datos de contagios, recuperados y muertos vienen de la universidad Johns Hopkins (JSU, https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/85320e2ea5424dfaaa75ae62e5c06e61)")
                , br()
                , h4("Los datos de test vienen de 'our world in Data' de la universidad de Oxford (OWD, https://ourworldindata.org/)")
                , br()
                , h4("Los datos de poblacion son descargados de  in Banco Mundial (https://data.worldbank.org/indicator/sp.pop.totl)")
                , br()
                , h2("Datos de activos"), br()
                ,h4("Son datos que nos permiten conocer en que estado se encuentra nuestro pais y ver si las curvas de datos activos(detectados), tienen un nivel aceptable")
                , br()
                , h2("Datos de Contagiados"), br()
                , h4("Un buen dato de contagiados es cuando la curva de recuperados esta por encima de la de nuevos datos")
                , h2("Controles")
                , h4("Los controles 'Minimo de # Casos' y 'Maximo de # Casos' requieren un ajuste manual. Por degracia no encontre una manera de hacer mejor")
                , h2("Acentos")
                , h4("Evite los acentos, para evitar problemas en la subida de datos")
                ,  sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Input: Choose dataset ----
                    selectInput("dataset", "Choose a dataset:",
                                choices = c(
                                  "TodosDatosJHU"
                                  , "NuevosCasosJHU"
                                  , "MuertosJHU"
                                  , "RecuperadosJHU"
                                  , "ActivosJHU"
                                  , "TodosDatosPorHabitanteJHU"
                                  , "NuevosCasosPorHabitanteJHU"
                                  , "MuertosPorHabitanteJHU"
                                  , "RecuperadosPorHabitanteJHU"
                                  , "ActivosPorHabitanteJHU"
                                  , "PoblacionBancoMundial"
                                  , "DatosOurWorldInData"
                                  , "DatosTestOWD"
                                )),
                    
                    # Button
                    downloadButton(outputId = "downloadData", label = "Descarga")
                    
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    tableOutput("table")
                    
                  )
                  
                )
                
                
        ),
        
        tabItem(tabName = "dashboard6",
                withSpinner(  plotlyOutput('plotTest') ) ,
                withSpinner(  plotlyOutput('plotTest1000h') ) ,
                dataTableOutput("TestTable")
        ),
        

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
    output$plotTest <- 
      renderPlotly(({
        pTot <- ggplot(data= datosOxfordTest[ order(Test_Totales, decreasing = TRUE)][1:30]
                       , aes(x=reorder(Coun, Test_Totales), y=Test_Totales)) +
          geom_bar(stat="identity", fill = "steelblue") + coord_flip()  + theme_bw() + xlab("Pais") + ylab("# Test") + ggtitle("Casos Total")
        pTot %>% ggplotly()
      }))
    
    output$plotTest1000h <- 
      renderPlotly(({
        pProp <- ggplot(data= datosOxfordTest[ order(TestPorMilHab, decreasing = TRUE)][1:30]
                        , aes(x=reorder(Coun, TestPorMilHab), y= round(TestPorMilHab, 2) ) ) +
          geom_bar(stat="identity", fill = "steelblue") + coord_flip()  + theme_bw() + xlab("Pais") + ylab("Test x 1000 hab") + ggtitle("Casos por mil habitantes") 
        pProp %>% ggplotly()
      }))

    
    output$TestTable <- 
      renderDataTable(({
        DT::datatable(datosOxfordTest)
      }))

# Descargas ---------------------------------------------------------------
    datasetInput <- reactive({
      switch(input$dataset,
             "TodosDatosJHU" = listaSinPop$datosAll,
             "NuevosCasosJHU" = listaSinPop$new,
             "MuertosJHU" = listaSinPop$dea,
             "RecuperadosJHU" = listaSinPop$rec,
             "ActivosJHU" = listaSinPop$act,
             "TodosDatosPorHabitanteJHU" = listaConPop$new,
             "NuevosCasosPorHabitanteJHU" = listaConPop$new,
             "MuertosPorHabitanteJHU" = listaConPop$new,
             "RecuperadosPorHabitanteJHU" = listaConPop$new,
             "ActivosPorHabitanteJHU" = listaConPop$new,
             "PoblacionBancoMundial" = Pop,
             "DatosOurWorldInData" = datosOxford,
             "DatosTestOWD" = datosOxfordTest
             )
    })
    
    # Table of selected dataset ----
    output$table <- renderTable({
      datasetInput()
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
