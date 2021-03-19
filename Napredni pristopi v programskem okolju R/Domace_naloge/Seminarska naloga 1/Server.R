# Namestimo potrebne knjižnice
library(shiny)
library(rvest)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(ggplot2)

########################################################################################################################
# UREDIMO VSE PODATKE, DA BODO PRIPRAVLJENI ZA SHINY PREDSTAVITEV

# get website
top_100_avti_net_full <- read_html("https://www.avto.net/Ads/results_100.asp?oglasrubrika=1&prodajalec=2")

# get car nodes
top_100_nodes <- top_100_avti_net_full %>%
  html_nodes(xpath = "//div[@class='row bg-white position-relative GO-Results-Row GO-Shadow-B']")

# get URL for each car so we can extract ID for each ad IN CORRECT ORDER
# currently not used, but a way to ID ads as repeated - Future upgrade
top_100_ids_url <- top_100_nodes %>%
  html_nodes(xpath="//a[@class='stretched-link']") %>%
  html_attr("href") %>%
  str_extract_all(pattern="id=\\d{8}") %>%
  unlist()

node_to_df <- function(node) {
  # Function to convert a single HTML node of a care to df row
  # input:
  #    - node: represents an HTML node with classes
  # output:
  #    - data.frame of nrow length 1 which is a transformation of data for a single car

  # pre-cleaning data for data extraction
  tmp <- c(str_match(node %>%
                       html_text() %>%
                       str_remove_all(pattern="\\r") %>%
                       str_remove_all(pattern="\\n") %>%
                       str_remove_all(pattern="\\t") %>%
                       str_trim() %>%
                       gsub("\\s+", " ", .) %>%
                       gsub("(km)(Gorivo)", "\\1 \\2", .) %>%
                       gsub("(motor)(Menjalnik)", "\\1 \\2", .) %>%
                       gsub("(motor)(Menjalnik)", "\\1 \\2", .) %>%
                       gsub("(menjalnik)(Motor)", "\\1 \\2", .),
                     "(.+)(1.registracija.+|Starost.+)(Gorivo.+)(Menjalnik.+)(Motor.+/ [0-9]+ KM)(.+)"))[-1] %>%
    str_trim()

  if (tmp[[2]] == "Starost NOVO") {
    tmp[[2]] <- paste0(tmp[[2]], " SPLITTHISPrevoženih 0") %>%
      str_replace(pattern = "NOVO", replacement = "0000")
  } else {
    tmp[[2]] <- gsub("(1.registracija.+)(Prevoženih.+)", "\\1SPLITTHIS\\2", tmp[[2]]) %>%
      str_replace(pattern = "1.registracija", replacement = "Starost") %>%
      str_remove(pattern = " km")
  }

  # some cars don't have km while not being new (registered this year)
  if (!str_detect(tmp[[2]], "Prevoženih.+")){
    tmp[[2]] <- paste0(tmp[[2]], " SPLITTHISPrevoženih 0")
  }

  tmp[[5]] <- gsub("(.+ccm, )(.+)", "\\1SPLITTHIS\\2", tmp[[5]])

  tmp <- strsplit(tmp,"SPLITTHIS") %>%
    unlist() %>%
    str_trim()

  # Only car brand (does not include model) - this is a possible upgrade down the road
  tmp[1] <- tmp[1] %>%
    str_extract(pattern="(Abarth|AEV|Aixam|Alfa Romeo|Alpine|Artega|Aston Martin|Audi|Austin|Autobianchi|Bentley|BMW|Borgward|Brilliance|Bugatti|Buick|Cadillac|Casalini|Chatenet|Chevrolet|Chrysler|Citroen|Cobra|Cupra|Dacia|Daewoo|DAF|Daihatsu|DKW|Dodge|Donkervoort|DS Automobiles|EV|Ferrari|Fiat|Fisker|Ford|GMC|Greatwall|Grecav|Hansa|Honda|Hummer|Hyundai|Infiniti|Iso|Isuzu|Iveco|JAC|Jaguar|JDM|Jeep|Kia|KTM|Lada|Lamborghini|Lancia|LandRover|LandWind|Lexus|Ligier|Lincoln|London Taxi|Lotus|LuAZ|Mahindra|Maruti|Maserati|Maybach|Mazda|McLaren|Mercedes-Benz|MG|Microcar|Mini|Mitsubishi|Morgan|Moskvič|Nissan|NSU|Oldsmobile|Opel|Peugeot|Piaggio|Plymouth|Polestar|Pontiac|Porsche|Proton|Puch|Renault|Replica|Rolls-Royce|Rosengart|Rover|Saab|Saturn|Seat|Shuanghuan|Simca|Smart|Spyker|SsangYong|Subaru|Suzuki|Škoda|Talbot|Tata|Tavria|Tazzari|Tesla|Toyota|Trabant|Triumph|TVR|UAZ|Vauxhall|Venturi|Volga|Volvo|Volkswagen|Wartburg|Westfield|Wiesmann|Zastava|ZAZ|Zhidou)") %>%
    paste0("ZnamkaSPLITTHIS", .)

  # Only 4 digit year of the car, 0000 if the car is brand new, dealt with when converting
  # the column to integer
  tmp[2] <- tmp[2] %>%
    str_extract(pattern="\\d{4}") %>%
    paste0("StarostSPLITTHIS", .)

  # Any number of digits as a representation of km driven with the car
  # if the car is brand new this is set to 0 beforehand
  tmp[3] <- tmp[3] %>%
    str_extract(pattern="[0-9]+") %>%
    paste0("KilometrinaSPLITTHIS", .)

  # What type of engine does the car have
  tmp[4] <- tmp[4] %>%
    str_replace(pattern = " ", replacement = "SPLITTHIS") %>%
    str_replace(pattern = "Gorivo", replacement = "Tip_motorja")

  # What type of transmission does the car have
  tmp[5] <- tmp[5] %>%
    str_replace(pattern = " ", replacement = "SPLITTHIS")

  # The displacement/size of the engine in cubic centimeters
  tmp[6] <- tmp[6] %>%
    str_replace(pattern = " ccm,", replacement = "") %>%
    str_replace(pattern = " ", replacement = "SPLITTHIS")

  # The power of the engine in kW
  tmp[7] <- tmp[7] %>%
    str_replace(pattern = " kW.+", replacement = "") %>%
    paste0("MocSPLITTHIS", .)

  # the largest price of the car
  #   this takes the first price found
  #       this is usually the price without any sort of sales, or leasing sales etc.
  #       but it could be wrong at times I assume - possible upgrade is to do more checks
  tmp[8] <- tmp[8] %>%
    str_extract(pattern = "([0-9]+\\.\\d{3}\\.\\d{3} €|[0-9]+\\.\\d{3} €|\\d{3} €)") %>%
    str_remove(pattern = " €") %>%
    str_remove(pattern = "\\.") %>%
    paste0("CenaSPLITTHIS", .)

  # converting our list of lists to matrix to data.frame of correct size
  # then selecting first row as names for the columns
  return(
    janitor::row_to_names(
      as.data.frame(
        matrix(
          unlist(
            str_split(tmp, pattern = "SPLITTHIS")
          ),
          nrow=length(
            unlist(
              str_split(tmp, pattern = "SPLITTHIS")[1]
            )
          )
        )
      ),
      row_number = 1
    ))

}

node_to_df2 <- function(node) {
  # Function to convert a single HTML node of a care to df row
  # input:
  #    - node: represents an HTML node with classes
  # output:
  #    - data.frame of nrow length 1 which is a transformation of data for a single car

  # pre-cleaning data for data extraction

  # get each string cleaned and separated
  # full_string <- top_100_nodes[[30]] %>%
  full_string <- node %>%
    html_text() %>%
    str_remove_all(pattern="\\r") %>%
    str_remove_all(pattern="\\n") %>%
    str_remove_all(pattern="\\t") %>%
    str_trim() %>%
    gsub("\\s+", " ", .)

  # get age 2 strings
  if (str_detect(full_string, "(1.registracija \\d{4}|Starost NOVO)")){
    age_string_extr <- str_extract(full_string, "(1.registracija \\d{4}|Starost NOVO)")
    if (age_string_extr == "Starost NOVO"){
      age_string_mod <- paste0("LetnikSPLITTHIS", format(Sys.Date(), "%Y"))
    } else {
      age_string_mod <- paste0("LetnikSPLITTHIS",
                               str_extract(age_string_extr, "\\d{4}"))
    }
  } else {
    age_string_mod <- "LetnikSPLITTHISNA"
  }

  # get mileage 2 strings
  if (str_detect(full_string, "Prevoženih [0-9]+ km")){
    mile_string_extr <- str_extract(full_string, "Prevoženih [0-9]+ km")
    mile_string_mod <- paste0("KilometrinaSPLITTHIS",
                              str_extract(mile_string_extr, "[0-9]+"))

  } else {
    if (age_string_extr == "Starost NOVO"){
      mile_string_extr <- NA_character_
      mile_string_mod <- "KilometrinaSPLITTHIS0"
    } else{
      mile_string_extr <- NA_character_
      mile_string_mod <- "KilometrinaSPLITTHISNA"
    }
  }

  # get engine type 2 strings
  if (str_detect(full_string, "Gorivo.+(motor|plin|pogon)")){
    engt_string_extr <- str_extract(full_string, "Gorivo.+?(motor|plin|pogon)")
    engt_string_mod <- paste0("Tip_motorjaSPLITTHIS",
                              str_remove(engt_string_extr, "Gorivo "))
  } else {
    engt_string_extr <- NA_character_
    engt_string_mod <- "Tip_motorjaSPLITTHISNA"
  }


  # get gearbox 2 strings
  if (str_detect(full_string, "Menjalnik.+menjalnik")){
    gear_string_extr <- str_extract(full_string, "Menjalnik.+?menjalnik")
    gear_string_mod <- paste0("MenjalnikSPLITTHIS",
                              str_remove(gear_string_extr, "Menjalnik "))
  } else {
    gear_string_extr <- NA_character_
    gear_string_mod <- "MenjalnikSPLITTHISNA"
  }

  # get engine size 2 strings
  if (str_detect(full_string, " [0-9]+ ccm")){
    engs_string_extr <- str_extract(full_string, " [0-9]+ ccm")
    engs_string_mod <- paste0("MotorSPLITTHIS",
                              str_extract(engs_string_extr, "[0-9]+"))
  } else {
    engs_string_extr <- NA_character_
    engs_string_mod <- "MotorSPLITTHISNA"
  }

  # get engine power in kW
  if (str_detect(full_string, " [0-9]+ kW / ")){
    engp_string_extr <- str_extract(full_string, " [0-9]+ kW / ")
    engp_string_mod <- paste0("MocSPLITTHIS",
                              str_extract(engp_string_extr, "[0-9]+"))
  } else {
    engp_string_extr <- NA_character_
    engp_string_mod <- "MocSPLITTHISNA"
  }

  # get price in EUR
  if (str_detect(full_string, "([0-9]+\\.\\d{3}\\.\\d{3} €|[0-9]+\\.\\d{3} €|\\d{3} €)")){
    price_string_extr <- str_extract(full_string,
                                     "([0-9]+\\.\\d{3}\\.\\d{3} €|[0-9]+\\.\\d{3} €|\\d{3} €)") %>%
      str_remove(pattern = " €") %>%
      str_remove(pattern = "\\.")
    price_string_mod <- paste0("CenaSPLITTHIS",
                               str_extract(price_string_extr, "[0-9]+"))
  } else {
    price_string_extr <- NA_character_
    price_string_mod <- "CenaSPLITTHISNA"
  }

  # create vectors of extracted data and modified data
  mod_data <- c(age_string_mod,
                mile_string_mod,
                engt_string_mod,
                gear_string_mod,
                engs_string_mod,
                engp_string_mod,
                price_string_mod)

  extr_data <- c(age_string_extr,
                 mile_string_extr,
                 engt_string_extr,
                 gear_string_extr,
                 engs_string_extr,
                 engp_string_extr,
                 price_string_extr)

  # create a matrix of extracted data and modified data
  full_matrix <- matrix(c(extr_data, mod_data),
                        ncol = 2,
                        byrow = FALSE
  )

  # Get car brand
  # [1,1] for first extracted element where there are no NAs, hence data exists
  # [[1]] for first element of list as str_split returns a list of vectors
  # [1] for first element of vector
  str_split(full_string,
            pattern=full_matrix[complete.cases(full_matrix), ][1,1])[[1]][1]

  car_brand <- str_split(full_string,
                         pattern=full_matrix[complete.cases(full_matrix), ][1,1])[[1]][1] %>%
    str_extract(pattern="(Abarth|AEV|Aixam|Alfa Romeo|Alpine|Artega|Aston Martin|Audi|Austin|Autobianchi|Bentley|BMW|Borgward|Brilliance|Bugatti|Buick|Cadillac|Casalini|Chatenet|Chevrolet|Chrysler|Citroen|Cobra|Cupra|Dacia|Daewoo|DAF|Daihatsu|DKW|Dodge|Donkervoort|DS Automobiles|EV|Ferrari|Fiat|Fisker|Ford|GMC|Greatwall|Grecav|Hansa|Honda|Hummer|Hyundai|Infiniti|Iso|Isuzu|Iveco|JAC|Jaguar|JDM|Jeep|Kia|KTM|Lada|Lamborghini|Lancia|LandRover|LandWind|Lexus|Ligier|Lincoln|London Taxi|Lotus|LuAZ|Mahindra|Maruti|Maserati|Maybach|Mazda|McLaren|Mercedes-Benz|MG|Microcar|Mini|Mitsubishi|Morgan|Moskvič|Nissan|NSU|Oldsmobile|Opel|Peugeot|Piaggio|Plymouth|Polestar|Pontiac|Porsche|Proton|Puch|Renault|Replica|Rolls-Royce|Rosengart|Rover|Saab|Saturn|Seat|Shuanghuan|Simca|Smart|Spyker|SsangYong|Subaru|Suzuki|Škoda|Talbot|Tata|Tavria|Tazzari|Tesla|Toyota|Trabant|Triumph|TVR|UAZ|Vauxhall|Venturi|Volga|Volvo|Volkswagen|Wartburg|Westfield|Wiesmann|Zastava|ZAZ|Zhidou)") %>%
    paste0("ZnamkaSPLITTHIS", .)

  tmp <- c(car_brand, mod_data)



  # converting our list of lists to matrix to data.frame of correct size
  # then selecting first row as names for the columns
  return(
    janitor::row_to_names(
      as.data.frame(
        matrix(
          unlist(
            str_split(tmp, pattern = "SPLITTHIS")
          ),
          nrow=length(
            unlist(
              str_split(tmp, pattern = "SPLITTHIS")[1]
            )
          )
        )
      ),
      row_number = 1
    ))
}

# function to catch errors
# adds NULL to list in lapply() if error
error_catch <- function (node, type=2) {
  if (type == 1){
    return(tryCatch(node_to_df(node), error=function(e) NULL))
  } else if (type == 2) {
    return(tryCatch(node_to_df2(node), error=function(e) NULL))
  } else {
    return(tryCatch(node_to_df2(node), error=function(e) NULL))
  }
}

data_tmp <- lapply(top_100_nodes, error_catch, type=2)

# remove NULL values in list
# forgot why I added this
data_tmp <- data_tmp[lengths(data_tmp) != 0]

# finally transform data to correct types, etc.
data_final <- data.table::rbindlist(data_tmp, idcol = "index") %>%
  as_tibble() %>%
  select(-index) %>%
  mutate(Znamka = as.factor(Znamka),
         Letnik = as.integer(Letnik),
         Kilometrina = as.integer(Kilometrina),
         Tip_motorja = as.factor(Tip_motorja),
         Menjalnik = as.factor(Menjalnik),
         Motor = as.integer(Motor),
         Moc = as.integer(Moc),
         Cena = as.integer(Cena)) %>%
  mutate(Letnik = if_else(Letnik == 0, max(Letnik), Letnik, Letnik))

# removes incomplete cases
data_final_no_na <- data_final[complete.cases(data_final),]

# samo vektorji glede na tip za lazje delo
fct_cols <- data_final %>% select_if(is.factor) %>% colnames()
num_cols <- data_final %>% select_if(is.numeric) %>% colnames()

# vse spremenljivke
all_vars <- c("Znamka",
              "Letnik",
              "Kilometrina",
              "Tip_motorja",
              "Menjalnik",
              "Motor",
              "Moc",
              "Cena")

type_vars <- c("Factor",
               "Integer",
               "Integer",
               "Factor",
               "Factor",
               "Integer",
               "Integer",
               "Integer")

desc_vars <- c("Znamka avtomobila",
               "Letnica ko je bil avto registriran (v primeru novega vozila trenutno leto)",
               "Prevoženi kilometri z avtomobilom (v primeru novega vozila nastavljeno na 0)",
               "Vrsta motorja oz. na kakšno gorivo se poganja avtomobil",
               "Vrsta menjalnika v avtomobilu",
               "Velikost motorja v kubičnih centimetrih (ccm)",
               "Moč motorja v kW",
               "Cena avtomobila v EUR")

desc_table <- cbind(all_vars, type_vars, desc_vars)

colnames(desc_table) <- c("Spremenljivka", "Tip spremenljivke", "Opis spremenljivke")

#######################################################################################################################
# SHINY

# Server - pišemo kar poganjamo v Ui.

shinyServer(function(input, output, session){

  #### ZAVIHEK INFO
  # Ustvarim tabelo za izpis v Ui z imenom TabelaInfo, zgoraj ustvarjeni tabeli je ime desc_table.
  output$TabelaInfo <- renderTable({desc_table})

  #### ZAVIHEK ENODIMENZIONALNO
  # Reactive del, ki preverja izbiro spremenljivke za prikaz (input$var_x1) in na podlagi tega določi možne izbire grafa za prikaz.
  geom_list_check <- reactive({
    if (input$var_x1 %in% fct_cols){geom_list <- list("barplot" = "barplot")}
    else {geom_list <- list("density" = "density", "histogram" = "histogram", "ECDF" = "ECDF", "QQ" = "QQ")}
    return(geom_list)
  })
  # Okenček, ki generira možnosti grafičnega prikaza v odvisnosti od var_x1 in s tem geom_list_check.
  output$geom_selector <- renderUI({selectInput(
    # Na tem inputId temeljijo nadaljna okenca generirana v odvisnosti od izbire geom_selected
    inputId="geom_selected", label="Grafični prikaz", choices=geom_list_check(), selected=NULL)})

  # Da nam predlaga možnosti za dodatno spremenljivko, kjer smo nastavili choice=NULL je potrebno definirati funkcije observe.
  # Za različne izbrane možnosti, nam poda vse spremenljivke, ki so tipa factor (možnosti barvanja) vendar so različne od prve izbrane spremenljivke.
  observe({updateSelectInput(session, inputId="dens_select1", choices=fct_cols[fct_cols != input$var_x1])})
  observe({updateSelectInput(session, inputId="hist_select1", choices=fct_cols[fct_cols != input$var_x1])})
  observe({updateSelectInput(session, inputId="bar_select1", choices=fct_cols[fct_cols != input$var_x1])})

  # Z vsemi možnostmi tj. Inputi, ki smo jih nastavili v Ui narišemo graf.
  output$plot_1d <- renderPlot({
    # Definiramo osnovni graf, ki mu bomo spreminjali atributi glede na izbrane Inpute. Kot osnovni input je vedno input$var_x1 tj. izbrana spremenljivka.
    p <- ggplot(data=data_final_no_na, aes_string(x=input$var_x1))

    if (!is.null(input$geom_selected)) {
        # Kot grafični prikaz izberemo barplot in znotraj njega imamo spet različne možnosti.
        if (input$geom_selected == "barplot"){
          p <- p + geom_bar(
            # Določimo barvanje glede na izbrano spremenljivko v bar_select1
            aes_string(fill=if(input$barplot_color_ind1) input$bar_select1 else NULL),
            # Določimo tip barvanja glede na izbrano vrednost v bar_type1
                            position=if(input$barplot_color_ind1) input$bar_type1 else "stack")
        }

        # Kot grafični prikaz izberemo density in znotraj njega sper različne možnosti.
        else if (input$geom_selected == "density"){
          # Določimo barvanje glede na izbrano spremenljivko v dens_select.
          p <- p + geom_density(aes_string(col=if(input$dens_color_ind1) input$dens_select1 else NULL))
          # Če smo izbrali prikaz povprečja/mediane izbrane spremenljivke input$var_x1 ga narišemo.
          if(input$dens_mean_ind1) {p <- p + geom_vline(xintercept=mean(data_final_no_na[,input$var_x1][[1]]), lty=2, col="red")}# Tak zapis ker gre za tibble in ne data.frame
          if(input$dens_med_ind1) {p <- p + geom_vline(xintercept=median(data_final_no_na[,input$var_x1][[1]]), lty=2, col="blue")}
        }

        # Kot grafični prikaz izberemo histogram in znotraj njega spet različne možnosti.
       else  if(input$geom_selected == "histogram"){
          # Določimo barvanje glede na izbrano spremenljivko v hist_select in glede na slider hist_slider_value_1 določimo še število stolpcev
          p <- p + geom_histogram(aes_string(fill=if(input$hist_color_ind1) input$hist_select1), bins=input$hist_slider_value_1, alpha=if(input$hist_color_ind1) 0.5 else 1, position="identity")
          # Če smo izbrali prikaz povprečja/mediane izbrane spremenljivke input$var_x1 ga narišemo.
          if(input$hist_mean_ind1) {p <- p + geom_vline(xintercept=mean(data_final_no_na[,input$var_x1][[1]]), lty=2, col="red")}
          if(input$hist_med_ind1) {p <- p + geom_vline(xintercept=median(data_final_no_na[,input$var_x1][[1]]), lty=2, col="blue")}
        }

        # Kot grafični prikaz izberemo ECDF in znotraj njega spet različne možnosti.
        else if(input$geom_selected == "ECDF"){
          p <- p + stat_ecdf()
          # Narišemo izbran percentil prek ecdf_slider_value1 spremenljivke input$var_x1
         p <- p + geom_vline(xintercept = quantile(data_final_no_na[ ,input$var_x1][[1]], probs=input$ecdf_slider_value1)[[1]], lty=2)
        }

        # Kot grafični prikaz izberemo QQ in znotraj njega spet različne možnosti.
        # Samo povozimo p ker aes_string potrebuje sample za geom_qq()
        else if(input$geom_selected == "QQ") {
          p <- ggplot(data_final_no_na, aes_string(sample=input$var_x1)) + geom_qq()
        }
      }

    # Narišemo graf, ki smo ga ustvarili.
    print(p)
  })

  #### ZAVIHEK DVODIMENZIONALNO
  # Naredimo, da ob izbrani neodvisni spremenljivki ni možnosti za le to odvisno spremenljivko.
  observeEvent(input$var_x2,{
    updateSelectInput(session, inputId="var_y2", choices=all_vars[all_vars != input$var_x2])
  })

  # Z vsemi možnostmi tj. Inputi, ki smo jih nastavili v Ui narišemo graf.
  output$plot_2d <- renderPlot({
    # Znebimo se error-ja.
    if (input$var_x2 == input$var_y2) return(NULL)
    # Naredimo osnoven graf z izbrano neodvisno in odvisno spremenljivko, ki ga bomo nato posodabljali glede na inpute.
    p <- ggplot(data=data_final_no_na, aes_string(x=input$var_x2, y=input$var_y2))

    # Če sta obe spremenljivki številski narišemo razsevni diagram.
    if (input$var_x2 %in% num_cols && input$var_y2 %in% num_cols){
      # Dodamo barvanje glede na tretjo izbrano spremenljivko
      p <- p + geom_point(aes_string(col=if(input$color_ind2) input$var_color2 else NULL))
      # Dodamo še izbrano glajenje in senčenje (če je izbrano).
      if (input$smooth_ind2){
        p <- p + geom_smooth(aes_string(col=if(input$color_ind2) input$var_color2 else NULL),
                             method=input$smooth_method2, se=input$smooth_se_ind2)
      }
    }

    # Če je ena spremenljivka številska in druga faktorska (natanko ena faktorska) narišemo boxplote tj. škatla z brki.
    else if (xor(input$var_x2 %in% fct_cols, input$var_y2 %in% fct_cols)) {
      # Dodamo barvanje glede na tretjo izbrano spremenljivko.
      p <- p + geom_boxplot(aes_string(col=if(input$color_ind2) input$var_color2 else NULL))
    }

    # Če sta obe spremenljivki faktorji narišemo nekaj čudnega.
    else if (input$var_x2 %in% fct_cols && input$var_y2 %in% fct_cols) {
      # Začasno si zapomnimo število skupin in število elementov znotraj njih za vsako spremenljivko.
      temp_df <- reactive(data_final_no_na[, c(input$var_x2, input$var_y2)] %>% group_by(across()) %>% summarize(count = n()))
      # Ustvarimo čuden graf.
      p <- ggplot(data=temp_df(), aes_string(x=input$var_x2, y=input$var_y2, fill="count")) +
        geom_tile() +
        geom_text(aes_string(label = "count"), size=6) +
        scale_fill_gradient(low = "white", high = "red")
    }

    # Izrišemo končni ustvarjen graf.
    print(p)
  })

  #### ZAVIHEK NAPREDNI GRAF
  # Shranimo napredni graf za prenos v Ui.
  output$tmp_plot <- renderPlot({
    ggplot(data=data_final_no_na, aes(x=Kilometrina, y=Cena)) +
      geom_point()
  })

  # Kako se spreminjajo prikazi s premikanjem miške po ekranu.
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(data_final_no_na,
                        hover,
                        threshold = 10,
                        maxpoints = 1,
                        addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # Izračunamo položaj miške na enkranu, relativno tj. [0,1] in ne na dimenzije v grafu.
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # Izračunam oddaljenost od spodnjega dela grafa in ooddaljenost od levega dela grafa.
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

    # Stil prikaza lastnosti točk.
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # Prikazani izpisi z lastnostmi točk.
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Znamka: </b>", point$Znamka, "<br/>",
                    "<b> Menjalnik: </b>", point$Menjalnik, "<br/>",
                    "<b> Tip motorja: </b>", point$Tip_motorja, "<br/>")))
    )
  })

})












