library(shiny)
library(leaflet)
library(dplyr)
library(curl)
library(geojsonio)

padronizar <- function(x, ...){
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}

limpar <- function(x) {
  resultado <- gsub("Á","A",x)
  resultado <- gsub("á","a",resultado)
  resultado <- gsub("é","e",resultado)
  resultado <- gsub("í","i",resultado)
  resultado <- gsub("ó","o",resultado)
  resultado <- gsub("ú","u",resultado)
  resultado <- gsub("á","a",resultado)
  resultado <- gsub("á","a",resultado)
  resultado <- gsub("ã","a",resultado)
  resultado <- gsub("ô","o",resultado)
  resultado <- gsub("ç","c",resultado)
  resultado <- gsub("Caixa D´Agua","Caixa D'Agua",resultado)
  resultado
}

# Atualiza os dados da prefeitura a cada 3h
marcador <- read.csv("marcador.csv")
inicio <- strptime(marcador$inicio, format="%Y-%m-%d %H:%M:%S")
agora <- Sys.time()
if (agora > inicio+60*60*3) {
  marcador$inicio <- inicio+60*60*3
  write.csv(marcador,"marcador.csv",row.names = F)
  handle <- new_handle()
  handle_setopt(handle, customrequest = "POST")
  handle_setopt(handle, postfields='tipo=censoPorBairro')
  curl_download("http://www.saude.salvador.ba.gov.br/wp-content/graficos/graficosjson.php",
                "tabela.csv", handle=handle)
}

# Carrega os dados da prefeitura
tabela <- read.csv("tabela.csv")
tabela <- tabela %>% mutate(N_RECUPERADOS=((CASOS_CONFIRMADOS-RECUPERADOS)/POPULACAO)*10^3)

# Carrega as delimitações dos bairros e une com os dados da prefeitura
salvador_bairros <- geojson_read("Bairros_Salvador.json", what = "sp")
bairros <- limpar(salvador_bairros$nome)
mapa <- data.frame(BAIRRO=bairros)
mapa <- left_join(mapa,tabela)
salvador_bairros@data[["populacao"]] <- mapa$POPULACAO
salvador_bairros@data[["incidencia"]] <- mapa$COEFICIENTE_INCIDENCIA
salvador_bairros@data[["confirmados"]] <- mapa$CASOS_CONFIRMADOS
salvador_bairros@data[["recuperados"]] <- mapa$RECUPERADOS
salvador_bairros@data[["n_recuperados"]] <- mapa$N_RECUPERADOS

ui <- fluidPage(
  tags$style("body {background-color:#d5dadc;}"),
  tags$style(type = "text/css", ".container-fluid {padding-left:0px;padding-right:0px;}"),
  leafletOutput("mapa",height = "100vh"),
  div(style="top:8px;right:8px;z-index:1001;position:absolute;",
      actionButton("mudar",NULL,icon = icon("refresh"))),
)
server <- function(session,input,output) {
  
  showModal(modalDialog(
    title = "Covid-19 em Salvador",
    paste0("Este app traz dados da Covid-19 em Salvador, Bahia.
            A fonte é o site da Secretaria Municipal de Saúde.
            Clique no mapa para ver informações detalhadas por bairro."),
    easyClose = TRUE,
    footer = NULL
  ))
  
  p <-
    leaflet(salvador_bairros,options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addControl("<span style='font-size:12px;'>
               Desenvolvido por <a href='https://github.com/murilogmamaral' target='_blank' style='margin-top:3px;'>
               murilogmamaral</a></span>",position = "topleft")
  
  plotar1 <- function(){
    pal <- colorNumeric(colorRamp(c("white","red3")), padronizar(salvador_bairros$incidencia,na.rm=T))
    output$mapa <- renderLeaflet({
      p %>%
        addPolygons(stroke = T,fillOpacity = 0.6,
                    color = "black",
                    weight = 1,
                    fillColor = ~pal(padronizar(incidencia,na.rm=T)),
                    popup = ~paste0("<b>",nome,"</b><br>",
                                    "• Coeficiente de incidência: ",incidencia,"<br>",
                                    "• ",populacao," habitantes<br>",
                                    "• ",confirmados," casos confirmados<br>",
                                    "• ",recuperados," recuperados<br>"))
    })
  }
  plotar1()
  
  observeEvent(input$mudar,{
    if (input$mudar%%2==0) {
      plotar1()
    }
    else {
      pal <- colorNumeric(colorRamp(c("white","blue4")),salvador_bairros$n_recuperados)
      output$mapa <- renderLeaflet({
        p %>%
          addPolygons(stroke = T,fillOpacity = 0.6,
                      color = "black",
                      weight = 1,
                      fillColor = ~pal(n_recuperados),
                      popup = ~paste0("<b>",nome,"</b><br>",
                                      "• ",populacao," habitantes<br>",
                                      "• ",confirmados-recuperados,
                                      ifelse(confirmados-recuperados==1,
                                             " óbito ou ainda não recuperado<br>",
                                             " entre óbitos e ainda não recuperados<br>"),
                                      " (média de ",round(n_recuperados,2)," por 1000 habitantes)"
                      ))
      })
    }
  })
}

shinyApp(ui,server)