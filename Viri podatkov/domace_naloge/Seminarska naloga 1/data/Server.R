# load the library for developing web apps with R (http://shiny.rstudio.com/)

library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)
library(maptools)
library(raster)
library(plyr)
library(rgdal)
library(RDS)
library(DT)
library(shinyWidgets)
library(dplyr)

# load data 
podatki_pn <- readRDS("podatki_pn_ocisceno")
Slovenia2_UTM <- readRDS("Slovenia2_UTM")

# define the server-side logic of the Shiny application
shinyServer(function(input, output) {
  
  data <- reactive({
    data <- podatki_pn %>% 
      filter(DatumPN <= input$daterange[2] & DatumPN >= input$daterange[1], 
             Starost >= 0)
  })
  
  data2 <- reactive({
    data <- podatki_pn %>% 
      filter(DatumPN <= input$daterange_druga[2] & DatumPN >= input$daterange_druga[1],
             Spol %in% c(input$spol),
             Starost >=0)
  })
  
  data_tabela <- reactive({
    data <- podatki_pn %>% dplyr::select(DatumPN, Spol, Starost, KlasifikacijaNesrece, UpravnaEnotaStoritve,
                              TekstCesteNaselja, TekstOdsekaUlice, VzrokNesrece, 
                              TipNesrece, VrstaUdelezenca)
  })
  
  # casovni okvir za zemljevid
  
  pn_filtered <- reactive({
    pn_filtered <- podatki_pn %>% 
      filter(DatumPN <= input$zemljevid_daterange[2] & DatumPN >= input$zemljevid_daterange[1])
  })
  
  # urejanje filtriranih podatkov za zemljevid
  Slovenia2_podatki <- reactive({
    count_df <- data.frame(count(pn_filtered(), "UpravnaEnotaStoritve"),
                           aggregate(pn_filtered()[, "VrednostAlkotesta"], list(pn_filtered()$UpravnaEnotaStoritve), mean)[,2],
                           aggregate(pn_filtered()[, "Starost"], list(pn_filtered()$UpravnaEnotaStoritve), mean)[,2],
                           aggregate(pn_filtered()[, "VozniskiStazVLetih"], list(pn_filtered()$UpravnaEnotaStoritve), mean)[,2])
    colnames(count_df) = c("NAME_2", "st_nesrec", "vrednost_alkotesta", "starost", "vozniski_staz")
    
    Slovenia2_UTM@data <- merge(Slovenia2_UTM@data, count_df, by="NAME_2", all.x = TRUE)
    
    
    Slovenia2_df <- fortify(Slovenia2_UTM)
    Slovenia2_df$id = as.factor(as.integer(Slovenia2_df$id)-1)
    Slovenia2_df <- merge(Slovenia2_df,Slovenia2_UTM@data, by="id", all.x = TRUE)
    
    return(Slovenia2_df) })
  
  output$hist <- renderPlotly({
    
    
    if (input$xaxis == "DatumPN"){
      if (input$fill != "None" & input$relativno == FALSE){ 
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis), fill = !!as.name(input$fill))) + 
          geom_bar(position = "stack") + labs(x = "", y = "Frekvenca", color = NULL) + scale_x_date(labels = wrap_format(10))
      } else if (input$fill == "None" & input$relativno == FALSE) {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis))) + 
          geom_bar(position = "stack") + labs(x = "", y = "Frekvenca", color = NULL) + scale_x_date(labels = wrap_format(10))
      } else if (input$fill != "None" & input$relativno == TRUE) {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis), fill = !!as.name(input$fill))) + 
          geom_bar(position = "fill") + labs(x = "", y = "Delež", color =  NULL) + 
          scale_y_continuous(labels = scales::percent) + scale_x_date(labels = wrap_format(10))
      } else {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis))) + 
          geom_bar(position = "fill") + labs(x = "", y = "Delež", color = NULL) + 
          scale_y_continuous(labels = scales::percent) + scale_x_date(labels = wrap_format(10))
      }
      
      ggplotly(p)
    
    } else if (input$xaxis == "UraPN") {
      if (input$fill != "None" & input$relativno == FALSE){ 
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis), fill = !!as.name(input$fill))) + 
          geom_histogram(position = "stack", bins = 24) + labs(x = "", y = "Frekvenca", color = NULL)
      } else if (input$fill == "None" & input$relativno == FALSE) {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis))) + 
          geom_histogram(position = "stack", bins = 24) + labs(x = "", y = "Frekvenca", color = NULL)
      } else if (input$fill != "None" & input$relativno == TRUE) {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis), fill = !!as.name(input$fill))) + 
          geom_histogram(position = "fill", bins = 24) + labs(x = "", y = "Delež", color =  NULL) + 
          scale_y_continuous(labels = scales::percent)
      } else {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis))) + 
          geom_histogram(position = "fill", bins = 24) + labs(x = "", y = "Delež", color = NULL) + 
          scale_y_continuous(labels = scales::percent)
      }
      
      ggplotly(p)
    } else {
      if (input$fill != "None" & input$relativno == FALSE){ 
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis), fill = !!as.name(input$fill))) + 
          geom_bar(position = "stack") + labs(x = "", y = "Frekvenca", color = NULL) + scale_x_discrete(labels = wrap_format(10))
      } else if (input$fill == "None" & input$relativno == FALSE) {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis))) + 
          geom_bar(position = "stack") + labs(x = "", y = "Frekvenca", color = NULL) + scale_x_discrete(labels = wrap_format(10))
      } else if (input$fill != "None" & input$relativno == TRUE) {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis), fill = !!as.name(input$fill))) + 
          geom_bar(position = "fill") + labs(x = "", y = "Delež", color =  NULL) + 
          scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = wrap_format(10))
      } else {
        p <- ggplot(data = data(), aes(x = !!as.name(input$xaxis))) + 
          geom_bar(position = "fill") + labs(x = "", y = "Delež", color = NULL) + 
          scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = wrap_format(10))
      }
      
      ggplotly(p)
    }
    
  })

  output$bar <- renderPlotly({
    
    ggplotly(ggplot(data = data2(), aes(x = !!as.name(input$neki), y = !!as.name(input$starost_staz), fill = Spol)) + 
      geom_boxplot() + coord_flip() + labs(x = "")) %>% layout(boxmode = "group")
    
  })
  
  output$tabela <- DT::renderDataTable({
    
    data_tabela() %>% filter(DatumPN <= input$daterange_tabela[2] & DatumPN >= input$daterange_tabela[1],
                             Starost <= input$starost_tabela[2] & Starost >= input$starost_tabela[1],
                             Spol %in% c(input$spol_tabela),
                             KlasifikacijaNesrece %in% c(input$klasifikacija_tabela),
                             UpravnaEnotaStoritve %in% c(input$upravna_enota_tabela),
                             VzrokNesrece %in% c(input$vzrok_tabela),
                             TipNesrece %in% c(input$tip_tabela),
                             VrstaUdelezenca %in% c(input$udelezenec_tabela))

    
  })
  
  output$zemljevid <- renderPlot({
    
    if (input$zemljevid_id != "prazno"){
      if (input$zemljevid_id =="starost"){naslov = "Starost povzročitelja nesreče v letih"}
      else if (input$zemljevid_id =="st_nesrec"){naslov = "Število nesreč"}
      else if (input$zemljevid_id =="vozniski_staz"){naslov = "Vozniški staž povzročitelja nesreče v letih"}
      else {naslov = "Vrednost alkotesta povzročitelja nesreče"}
      
      ggplot() + 
        geom_polygon(data = Slovenia2_podatki(), aes(x = long, y = lat, group = group, fill =
                                                       !!as.name(input$zemljevid_id)), color = NA) +
        ggtitle(naslov) +
        scale_fill_gradient(low="blue", high="red") +
        theme(panel.grid.minor = element_blank(),
              aspect.ratio=2/3,
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
              legend.position = c(.8, .2),
              legend.title = element_blank(),
              legend.key.size = unit(1, 'cm'),
              legend.text = element_text(size=15))
    } else {
      ggplot() + 
        geom_polygon(data = Slovenia2_podatki(), aes(x = long, y = lat, group = group)) +
        ggtitle("") +
        theme(panel.grid.minor = element_blank(),
              aspect.ratio=2/3,
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              axis.line = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
    }
  })
  
})