# define the user interface

library(shinythemes)

all_vars <- c("Znamka",
     "Letnik",
     "Kilometrina",
     "Tip_motorja",
     "Menjalnik",
     "Motor",
     "Moc",
     "Cena")

fluidPage(theme = shinytheme("united"),
    navbarPage("Zadnjih 100 avto.net podatki",
               tabPanel("Info",
                        mainPanel(
                            p("Enostavna aplikacija za pregled podatkov zadnjih 100 oglasov na spletni strani avto.net, ki je namenjena objavljanju oglasov za prodajo avtov."),
                            p("Ob zagonu aplikacije zajamemo zadnjih 100 oglasov iz surovega HTML-ja. Nato z Regex nastavitvami aplikacija ustrezno zajame podatke in jih nato preoblikuje v podatkovni okvir, ki jih prikažemo na več načinov."),
                            p("V zavihkih je mogoče izbirati način analize podatkov (univariatno, multivariatno). V spodnji tabeli so pa opisane uporabljene spremenljivke."),
                            tableOutput("desc_table")
                            )
                        ),
               tabPanel("Univariatno",
                        sidebarPanel(
                            selectInput("var_x1",
                                        "Spremenljivka:",
                                        choices=all_vars,
                                        selected="Znamka"),
                            uiOutput('geom_selector'),
                            conditionalPanel(condition = "input.geom_selected == 'density'",
                                             checkboxInput("dens_mean_ind1",
                                                           "Prikaz skupnega povprečja (rdeča)"),
                                             checkboxInput("dens_med_ind1",
                                                           "Prikaz skupne mediane (modra)"),
                                             checkboxInput("dens_color_ind1",
                                                           "Barva"),
                                             conditionalPanel(condition="input.dens_color_ind1 == true",
                                                              selectInput("dens_select1",
                                                                          "Izbor spremenljivke:",
                                                                          choices = NULL)
                                                              )

                                             ),
                            conditionalPanel(condition = "input.geom_selected == 'ECDF'",
                                             sliderInput("ecdf_slider_value1",
                                                         "Prikazovalnik percentilne črte",
                                                         0,
                                                         1,
                                                         0.5,
                                                         step=0.01)
                                             ),
                            conditionalPanel(condition = "input.geom_selected == 'histogram'",
                                             checkboxInput("hist_mean_ind1",
                                                           "Prikaz skupnega povprečja (rdeča)"),
                                             checkboxInput("hist_med_ind1",
                                                           "Prikaz skupne mediane (modra)"),
                                             sliderInput("hist_slider_value1",
                                                         "Število stolpcev",
                                                         1,
                                                         50,
                                                         30,
                                                         step=1),
                                             checkboxInput("hist_color_ind1",
                                                           "Barva"),
                                             conditionalPanel(condition="input.hist_color_ind1 == true",
                                                              selectInput("hist_select1",
                                                                          "Izbor spremenljivke:",
                                                                          choices = NULL)
                                                              )
                                             ),

                            conditionalPanel(condition = "input.geom_selected == 'barplot'",
                                             checkboxInput("barplot_color_ind1",
                                                           "Barva"),
                                             conditionalPanel(condition="input.barplot_color_ind1 == true",
                                                              selectInput("bar_select1",
                                                                          "Izbor spremenljivke:",
                                                                          choices = NULL),
                                                              radioButtons("bar_type1", "Način barve:",
                                                                           c("stack", "dodge", "fill")
                                                                           )
                                                              )
                                             )
                            ),

                        mainPanel(plotOutput("plot_1d", height="700px")
                                  )
               ),
               tabPanel("Multivariatno",
                        sidebarPanel(
                            selectInput("var_x2",
                                        "Neodvisna spremenljivka:",
                                        choices=all_vars[all_vars != "Znamka"],
                                        selected="Letnik"),
                            selectInput("var_y2",
                                        "Odvisna spremenljivka:",
                                        choices=all_vars,
                                        selected="Cena"),
                            p("Barva deluje če je vsaj ena spremenljivka številska"),

                            checkboxInput("color_ind2", "Barva", FALSE),
                            conditionalPanel(
                                condition = "input.color_ind2 == true",
                                selectInput("var_color2",
                                            "Izbor spremenljivke:",
                                            choices=all_vars[all_vars != "Znamka"],
                                            selected=NULL)),
                            p("Glajenje deluje če sta obe spremenljivki številski"),
                            checkboxInput("smooth_ind2", "Glajenje", FALSE),
                            conditionalPanel(
                                condition = "input.smooth_ind2 == true",
                                radioButtons("smooth_method2", "Metoda:",
                                             list("lm", "gam", "loess")),
                                checkboxInput("smooth_se_ind2", "SE", FALSE)
                            )),

                        # define content of the main part of the page ####
                        mainPanel(
                            div(style="position:relative",
                                plotOutput("plot_2d",
                                           hover = hoverOpts(id ="plot_hover", delay = 100, delayType = "debounce"),
                                           height="700px"),
                                # uiOutput("hover_info")
                                )
                            )
                        ),
               tabPanel("tmp",
                        sidebarPanel(p("teeeeeeext")),
                        mainPanel(
                            div(style="position:relative",
                                plotOutput("tmp_plot",
                                           hover = hoverOpts(id ="plot_hover", delay = 100, delayType = "debounce"),
                                           height="700px"),
                                uiOutput("hover_info")
                            )
                        ))
    )

    # # define type of page layout
    # pageWithSidebar(
    #
    # # define content of page header ####
    # headerPanel("Nastavitve"),
    #
    # # define content of left side of the page ####
    # sidebarPanel(
    #     selectInput("var_x",
    #                 "Neodvisna spremenljivka:",
    #                 choices=all_vars[all_vars != "Znamka"],
    #                 selected="Letnik"),
    #     selectInput("var_y",
    #                 "Odvisna spremenljivka:",
    #                 choices=all_vars,
    #                 selected="Cena"),
    #     checkboxInput("color_ind", "Barva", FALSE),
    #     conditionalPanel(
    #         condition = "input.color_ind == true",
    #         selectInput("var_color",
    #                 "Barva:",
    #                 choices=all_vars[all_vars != "Znamka"],
    #                 selected=NULL)),
    #     checkboxInput("smooth_ind", "Glajenje", FALSE),
    #     conditionalPanel(
    #         condition = "input.smooth_ind == true",
    #         radioButtons("smooth_method", "Metoda:",
    #                     list("lm", "gam", "loess")),
    #         checkboxInput("smooth_se_ind", "SE", FALSE)
    #     )),
    #
    # # define content of the main part of the page ####
    # mainPanel(
    #     plotOutput("plot_plot", height="700px"),
    #
    #     )
    )
# )

