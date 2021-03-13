library(shiny)
library(rvest)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(scales)

options(scipen=9999)

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

# define the server-side logic of the Shiny application
shinyServer(function(input, output, session) {

    # check this if this would be better
    # https://cmsdk.com/javascript/r-shiny--conditionalpanel-with-condition-check-item-in-list.html
    # Samo da nista X in Y spremenljivka enaki pri 2D prikazu
    # gleda kak je var_x2
    observeEvent(input$var_x2,{
        # posodobi izbiro za var_y2 tako da ni var_x2 v mozni izbiri
        updateSelectInput(session,
                          'var_y2',
                          choices=all_vars[all_vars != input$var_x2])
    })



    # reactive ki preverja pri 1D kaj je bila izbrana spremenljivka
    # da potem doloci seznam moznih izbir geom_() funkcij za prikaz
    geom_list_check <- reactive({
        if (input$var_x1 %in% fct_cols){
            geom_list <- list("barplot" = "barplot")
        } else {
            geom_list <- list("density" = "density",
                              "histogram" = "histogram",
                              "ECDF" = "ECDF",
                              "QQ" = "QQ")
        }

        return(geom_list)
    })

    output$desc_table <- renderTable({desc_table})

    # funkcija za generiranje dropdown menija z izbori od geom_list_check (glede na var_x1)
    output$geom_selector <- renderUI({
        selectInput('geom_selected',
                    'Plot type:',
                    choices=geom_list_check(),
                    selected=NULL)
    })

    # narisi graf glede na izbran geom iz geom_selector-ja - 1D
    output$plot_1d <- renderPlot({
        # primarno shranimo podatke z var_x1 inputom
        p <- ggplot(data_final_no_na, aes_string(input$var_x1))

        # preverjamo kaj je bila izbrana geom_funkcija (omejene glede na tip var_x1)
        if(input$geom_selected == "barplot") {
            p <- p + geom_bar(aes_string(fill=if(input$barplot_color_ind1) input$bar_select1 else NULL),
                              position=if(input$barplot_color_ind1) input$bar_type1 else "stack")
        } else if (input$geom_selected == "density") {
            p <- p + geom_density(aes_string(col=if(input$dens_color_ind1) input$dens_select1 else NULL))
            if (input$dens_mean_ind1) {
                # [[1]] due to data_final_no_na[ , input$var_x1] being a tibble and mean
                # doesn't work on list, so we have to select first element of list
                p <- p + geom_vline(xintercept = mean(data_final_no_na[ , input$var_x1][[1]]),
                                    linetype = "dashed",
                                    colour = "red")
            }
            if (input$dens_med_ind1) {
                p <- p + geom_vline(xintercept = median(data_final_no_na[ , input$var_x1][[1]]),
                                    linetype = "dashed",
                                    colour = "blue")
            }
        } else if (input$geom_selected == "histogram") {
            p <- p + geom_histogram(aes_string(fill=if(input$hist_color_ind1) input$hist_select1 else NULL),
                                    alpha=if(input$hist_color_ind1) 0.5 else 1,
                                    bins=input$hist_slider_value1,
                                    position="identity")
            if (input$hist_mean_ind1) {
                # [[1]] due to data_final_no_na[ , input$var_x1] being a tibble and mean
                # doesn't work on list, so we have to select first vector of list
                p <- p + geom_vline(xintercept = mean(data_final_no_na[ , input$var_x1][[1]]),
                                    linetype = "dashed",
                                    colour = "red")
            }

            if (input$hist_med_ind1) {
                p <- p + geom_vline(xintercept = median(data_final_no_na[ , input$var_x1][[1]]),
                                    linetype = "dashed",
                                    colour = "blue")
            }
        } else if (input$geom_selected == "ECDF") {
            p <- p + stat_ecdf() +
                geom_vline(xintercept = quantile(data_final_no_na[ , input$var_x1][[1]],
                                                 probs=input$ecdf_slider_value1)[[1]],
                           linetype = "dashed")
        } else if (input$geom_selected == "QQ") {
            # sam povozimo p ker aes_string potrebuje sample za geom_qq()
            p <- ggplot(data_final_no_na, aes_string(sample=input$var_x1)) +
                geom_qq()
        }
        # narisi graf, karkoli je
        print(p)
    })

    output$tmp_plot <- renderPlot({
        ggplot(data_final_no_na, aes(Cena,
                                     Kilometrina)) +
            geom_point()

    })

    output$hover_info <- renderUI({
        hover <- input$plot_hover
        point <- nearPoints(data_final_no_na,
                            hover,
                            threshold = 10,
                            maxpoints = 1,
                            addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

        # calculate point position INSIDE the image as percent of total dimensions
        # from left (horizontal) and from top (vertical)
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")

        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Znamka: </b>", point$Znamka, "<br/>",
                          "<b> Menjalnik: </b>", point$Menjalnik, "<br/>",
                          "<b> Tip motorja: </b>", point$Tip_motorja, "<br/>")))
        )
    })

    # narisi graf glede na tip dveh izbranih spremenljivk - 2D
    output$plot_2d <- renderPlot({
        # da ni error
        if (input$var_x2 == input$var_y2) return(NULL)
        p <- ggplot(data_final_no_na, aes_string(input$var_x2,
                                                 input$var_y2
                                                 )
                    )
        # ce sta obe stevilski narisi scatter plot
        if (input$var_x2 %in% num_cols && input$var_y2 %in% num_cols) {
            # col je sam za barvo ce je color_ind2 (checkmark) TRUE, ce je,
            # potem var_color2 doloca barvo
            p <- p + geom_point(aes_string(col=if(input$color_ind2) input$var_color2 else NULL))

            # ce sta obe numericni lahko dodamo tudi smoothing glede na smooth_ind2
            # checkmark.
            # metoda je smooth_method2 iz UI
            # ravno tako se iz smooth_se_ind2 checkmark
            if (input$smooth_ind2) {
                p <- p + geom_smooth(aes_string(col=if(input$color_ind2) input$var_color2 else NULL),
                                     method=input$smooth_method2,
                                     se=input$smooth_se_ind2)
            }

        # xor funkcija da risemo boxplote ko je ena spremenljivka faktor ena pa stevilska
        } else if (xor(input$var_x2 %in% fct_cols, input$var_y2 %in% fct_cols)){
            # narisemo boxplote z barvo kot pri scatterplotu
            p <- p + geom_boxplot(aes_string(col=if(input$color_ind2) input$var_color2 else NULL))

        # ce sta oba faktorja
        } else if (input$var_x2 %in% fct_cols && input$var_y2 %in% fct_cols) {

            # zacasno sam prestejemo posamezne skupine, da loh narisemo
            temp_df <- reactive(data_final_no_na[, c(input$var_x2, input$var_y2)] %>%
                                    group_by(across()) %>%
                                    summarize(count = n())
            )

            p <- ggplot(temp_df(),
                        aes_string(x = input$var_x2, y = input$var_y2, fill = "count")) +
                geom_tile() +
                geom_text(aes_string(label = "count"),
                          size=6) +
                scale_fill_gradient(low = "white", high = "red")
        }
        # izrisi graf, karkoli je.
        print(p)
    })

    observe({
        updateSelectInput(session,
                          'dens_select1',
                          choices=fct_cols[fct_cols != input$var_x1])
    })

    observe({
        updateSelectInput(session,
                          'hist_select1',
                          choices=fct_cols[fct_cols != input$var_x1])
    })

    observe({
        updateSelectInput(session,
                          'bar_select1',
                          choices=fct_cols[fct_cols != input$var_x1])
    })
})







