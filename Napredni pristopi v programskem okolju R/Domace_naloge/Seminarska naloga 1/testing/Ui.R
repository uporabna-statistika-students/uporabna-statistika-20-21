library(shinythemes)
all_vars <- c("Znamka",
              "Letnik",
              "Kilometrina",
              "Tip_motorja",
              "Menjalnik",
              "Motor",
              "Moc",
              "Cena")

# User inference, kar uporabnik vidi.
fluidPage(
  # Tema spletne strani
  theme = shinytheme("united"),
  # Glavni naslov
          navbarPage("Zadnjih 100 avto.net podatkov"),
          # Okenčki, ki ti odprejo nove stvari.
          tabsetPanel(
            # Okenček informacije
            tabPanel(title="Informacije",
                     # Besedilo, ki opisuje spletno stran.
                     p(""),
                     p("Enostavna aplikacija za pregled podatkov zadnjih 100 oglasov na spletni strani avto.net, ki je namenjena objavljanju oglasov za prodajo avtov."),
                     p("Ob zagonu aplikacije zajamemo zadnjih 100 oglasov iz surovega HTML-ja. Nato z Regex nastavitvami aplikacija ustrezno zajame podatke in jih nato preoblikuje v podatkovni okvir, ki jih prikažemo na več načinov."),
                     p("V zavihkih je mogoče izbirati način analize podatkov (univariatno, multivariatno). V spodnji tabeli so pa opisane uporabljene spremenljivke."),
                     # Tabela, ki opisuje uporabljene podatke, v Server ji je ime TabelaInfo.
                     tableOutput(outputId="TabelaInfo")
                     ),

            # Okenček za enodimenzionalni prikaz
            tabPanel(title="Enodimenzionalno",
                     # Stranski meni
                     sidebarPanel(
                       p(""),
                       # Okenček z možnostmi spremenljivke za prikaz
                       selectInput(inputId="var_x1", label="Spremenljivka", choices=all_vars, selected=NULL),
                       # Okenček z možnostmi grafa za prikaz. Ta je odvisen od izbire spremenljivke v var_x1
                       uiOutput(outputId="geom_selector"),
                       # V odvisnosti od izbire pod geom_selected (Server) se pokažejo dodatna okenca, ki ponujajo izbire.
                       conditionalPanel(condition="input.geom_selected == 'density'",
                                        # Izbire, ki se pojavijo, če izberemo density.
                                        checkboxInput(inputId="dens_mean_ind1", label="Prikaz skupnega povprečja [rdeča]"),
                                        checkboxInput(inputId="dens_med_ind1", label="Prikaz skupne mediane [modra]"),
                                        checkboxInput(inputId="dens_color_ind1", label="Barvni prikaz dodatne spremenljivke"),
                                        # Če izberemo Barvni prikaz dodatne spremenljivke se pojavijo dodatne možnosti, tudi v odvisnosti od izbire grafa.
                                        conditionalPanel(condition="input.dens_color_ind1 == true",
                                                         selectInput(inputId="dens_select1", label="Dodatna spremenljivka", choices=NULL)
                                                         )
                                        ),
                       conditionalPanel(condition="input.geom_selected == 'ECDF'",
                                        # Izbire, ki se pojavijo, če izberemo ECDF
                                        sliderInput(inputId="ecdf_slider_value1", label="Izbira percentila", min=0, max=1, value=0.5, step=0.01)
                                        ),
                       conditionalPanel(condition="input.geom_selected == 'histogram'",
                                        # Izbire, ki se pojavijo, če izberemo histogram
                                        checkboxInput(inputId="hist_mean_ind1", label="Prikaz skupnega povprečja [rdeča]"),
                                        checkboxInput(inputId="hist_med_ind1", label="Prikaz skupne mediane [modra]"),
                                        sliderInput(inputId="hist_slider_value_1", label="Število stolpcev", min=1, max=50, value=25, step=1),
                                        checkboxInput(inputId="hist_color_ind1", label="Barvni prikaz dodatne spremenljivke"),
                                        # Če izberemo Barvni prikaz dodatne spremenljivke se pojavijo dodatne možnosti, tudi v odvisnosti od izbire grafa
                                        conditionalPanel(condition="input.hist_color_ind1 == true",
                                                         selectInput(inputId="hist_select1", label="Dodatna spremenljivka", choices=NULL)
                                                         )
                                        ),
                       conditionalPanel(condition="input.geom_selected == 'barplot'",
                                        # Izbire, ki se pojavijo če izberemo barplot
                                        checkboxInput(inputId="barplot_color_ind1", label="Barvni prikaz dodatne spremenljivke"),
                                        # Če izberemo Barvni prikaz dodatne spremenljivke se pojavijo dodatne možnosti, tudi v odvisnosti od izbire grafa
                                        conditionalPanel(condition="input.barplot_color_ind1 == true",
                                                         selectInput(inputId="bar_select1", label="Dodatna spremenljivka", choices=NULL),
                                                         radioButtons(inputId="bar_type1", label="Izbira barvanja", c("stack", "dodge", "fill"))
                                                         )
                                        )
                     ),
                     # Dodamo še graf, ki ga konstruiramo v Server, v glavno okno.
                     mainPanel(plotOutput(outputId="plot_1d", height="700px"))
                     ),


            # Okenček za dvodimenzionalni prikaz
            tabPanel(title="Dvodimenzionalno",
                     sidebarPanel(
                       # Okenci z možnimi izbirami neodvisne in odvisne spremenljivke.
                       selectInput(inputId="var_x2", label="Neodvisna spremenljivka", choices=all_vars[all_vars != "Znamka"], selected="Letnik"),
                       selectInput(inputId="var_y2", label="Odvisna spremenljivka", choices=all_vars, selected="Cena"),
                       # Okvirček za možnost Barvnega prikaza dodatne spremenljivke.
                       checkboxInput(inputId="color_ind2", label="Barvni prikaz dodatne spremenljivke", value=FALSE),
                       p("[Barva deluje, če je vsaj ena od izbranih spremenljivk številska.]"),
                       # Če zberemo Barvni prikaz dodatne spremenljivke se pojavi okence z izbiro spremenljivke za prikaz.
                       conditionalPanel(condition="input.color_ind2 == true",
                                        selectInput(inputId="var_color2", label="Dodatna (tretja) spremenljivka", choices=all_vars[all_vars != "Znamka"], selected=NULL)),
                       # Okvirček za možnost dodatka krivulje za Glajenje.
                       checkboxInput(inputId="smooth_ind2", label="Dodatek krivulje za glajenje", value=FALSE),
                       # Če izberemo dodatek Glajenja, se odprejo tri možnosti za obkljukati izbiro glajenja, možna je samo ena izbira. Doda se tudi izbira senčenja.
                       p("[Glajenje deluje, če sta obe spremenljivki številski.]"),
                       conditionalPanel(condition="input.smooth_ind2 == true",
                                        radioButtons(inputId="smooth_method2", label="Metoda glajenja", choices=list("lm","gam","loess")),
                                        checkboxInput(inputId="smooth_se_ind2", label="Senčenje", value=FALSE))
                     ),
                     # Dodamo še graf, ki ga konstruiramo v Server, v glavno okno.
                     mainPanel(div(style="position:relative", plotOutput(outputId="plot_2d", height="700px")))
                     ),

            # Okenček za napredni graf
            tabPanel(title="Napredni graf",
                     sidebarPanel(
                       p("Z miško se premikaj po grafu in si oglej katere avtomobile predstavljajo posamezne točke.")),
                     mainPanel(div(style="position:relative",
                                   plotOutput(outputId="tmp_plot",
                                              hover=hoverOpts(id="plot_hover", delay = 100, delayType = "debounce"), height="700px"),
                                   uiOutput("hover_info")))
                     )


          )

)
