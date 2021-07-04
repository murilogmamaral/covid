library(shiny)
library(leaflet)
library(dplyr)
library(curl)
library(geojsonio)

options(scipen = 999)

f <- function(x){formatC(as.numeric(x), format="f", digits=0, big.mark=".",decimal.mark = ",")}

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

ui <- fluidPage(
  tags$style("body {background-color:#d5dadc;}"),
  tags$style(type = "text/css", ".container-fluid {padding-left:0px;padding-right:0px;}"),
  leafletOutput("mapa",height = "100vh"),
  div(style="top:10px;right:10px;z-index:1001;position:absolute;",
      actionButton("mudar","mudar")),
)
server <- function(session,input,output) {
  
  # Atualiza os dados da prefeitura a cada 24h
  marcador <- read.csv("marcador.csv")
  inicio <- strptime(marcador$inicio, format="%Y-%m-%d %H:%M:%S")
  agora <- Sys.time()
  if (agora > inicio+60*60*24) {
    marcador$inicio <- inicio+60*60*24
    write.csv(marcador,"marcador.csv",row.names = F)
    handle <- new_handle()
    handle_setopt(handle, customrequest = "POST")
    handle_setopt(handle, postfields='tipo=censoPorBairro')
    curl_download("http://www.saude.salvador.ba.gov.br/wp-content/graficos/graficosjson.php",
                  "tabela.csv", handle=handle)
    handle_setopt(handle, postfields='tipo=leitosDisponiveisOcupados')
    curl_download("http://www.saude.salvador.ba.gov.br/wp-content/graficos/graficosjson.php",
                  "leitos.csv", handle=handle)
    handle_setopt(handle, postfields='tipo=confirmadoCuradoObito')
    curl_download("http://www.saude.salvador.ba.gov.br/wp-content/graficos/graficosjson.php",
                  "obitos.csv", handle=handle)
  }
  
  # Carrega os dados da prefeitura
  leitos <- read.csv("leitos.csv")
  obitos <- read.csv("obitos.csv")
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
  
  showModal(modalDialog(
    title = "Covid-19 em Salvador",
    paste0("Este app traz dados atualizados da Covid-19 em Salvador, Bahia.
            A fonte é o site da Secretaria Municipal de Saúde.
            Clique no mapa para ver estatísticas por bairro."),
    easyClose = TRUE,
    footer = NULL
  ))
  
  ocupado_adulto <- leitos$OCUP_UTI_ADULTO[length(leitos$OCUP_UTI_ADULTO)]
  total_adulto <- leitos$DISP_UTI_ADULTO[length(leitos$DISP_UTI_ADULTO)]
  
  ocupado_crianca <- leitos$OCUP_UTI_PEDIATRICO[length(leitos$OCUP_UTI_PEDIATRICO)]
  total_crianca <- leitos$DISP_UTI_PEDIATRICO[length(leitos$DISP_UTI_PEDIATRICO)]

  p <-
    leaflet(salvador_bairros,options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addControl(paste(#"<b>Salvador, Bahia</b><br>",
                     "<span style='font-size:12px;'><b>Habitantes:</b>", f(sum(salvador_bairros$populacao,na.rm=T)),
                     "<br><b>Casos confirmados:</b>", f(obitos$CONFIRMADO[length(obitos$CONFIRMADO)]),
                     "<br><b>Curados:</b>", f(obitos$CURADO[length(obitos$CURADO)]),
                     "<br><b>Percentual de curados:</b>",paste0(round(obitos$CURADO[length(obitos$CURADO)]/obitos$CONFIRMADO[length(obitos$CONFIRMADO)],2)*100,"%"),
                     "<br><b>Óbitos:</b>", f(obitos$OBITO[length(obitos$OBITO)]),
                     "<br><b>Ocupação de leitos:</b>",
                     "<br> • UTI adulto:", paste(f(ocupado_adulto),"de",f(total_adulto),paste0("(",round(ocupado_adulto/total_adulto,2)*100,"%)")),
                     "<br> • UTI criança:",paste(f(ocupado_crianca),"de",f(total_crianca),paste0("(",round(ocupado_crianca/total_crianca,2)*100,"%)")),
                     "<br><span style='font-size:10px;font-style:italic;'>Atualizado em",obitos$DATA[length(obitos$DATA)],"</span>",
                     "</span>"),position = "topleft") %>%
    addControl("<span style='font-size:10px;'>
               Desenvolvido por <a href='https://github.com/murilogmamaral' target='_blank' style='margin-top:3px;'>murilogmamaral</a></span>",position = "topleft")

  plotar1 <- function(){
    maximo <- round(max(salvador_bairros$incidencia,na.rm = T))
    pal <- colorNumeric(colorRamp(c("white","red3")), 0:max(600,maximo))
    output$mapa <- renderLeaflet({
      p %>%
        addPolygons(stroke = T,fillOpacity = 0.6,
                    color = "black",
                    weight = 1,
                    fillColor = ~pal(round(incidencia)),
                    popup = ~paste0("<b>",nome,"</b><br>",
                                    "• Coeficiente de incidência:<br>",
                                    "&nbsp;&nbsp;",round(incidencia)," por 1000 habitantes<br>",
                                    "<span style='font-size:12px;color:gray;'>",
                                    "• ",populacao," habitantes<br>",
                                    "• ",confirmados," casos confirmados<br>",
                                    "• ",recuperados," curados<br></span>"))
    })
  }
  plotar1()
  
  observeEvent(input$mudar,{
    if (input$mudar%%2==0) {
      plotar1()
    }
    else {
      pal <- colorNumeric(colorRamp(c("green",rep("yellow2",2),rep("red3",10))),0:50)
      output$mapa <- renderLeaflet({
        p %>%
          addPolygons(stroke = T,fillOpacity = 0.6,
                      color = "black",
                      weight = 1,
                      fillColor = ~pal(100-(round(recuperados/confirmados,2)*100)),
                      popup = ~paste0("<b>",nome,"</b><br>",
                                      "• Percentual de curados: ",
                                      round(recuperados/confirmados,2)*100,"%<br>",
                                      "<span style='font-size:12px;color:gray;'>",
                                      "• ",populacao," habitantes<br>",
                                      "• ",confirmados," casos confirmados<br>",
                                      "• ",recuperados," curados<br></span>"))

      })
    }
  })
}

shinyApp(ui,server)