# collin data excercise
collin_data <- as_tibble(read.table("COLLIN.txt",
                                    header=TRUE,
                                    sep="\t",
                                    encoding="UTF-8",
                                    dec=".",
                                    na.strings=c("", " ")))

ggplot(collin_data, aes(x=windspeed, y=time)) +
    geom_point() +
    labs(x="Hitrost vetra v hrbet [m/s]",
         y="Čas teka [s]",
         title="Podatki prikazani na razsevnem diagramu")

cor(collin_data$windspeed, collin_data$time)


lm_collin <- lm(time~windspeed, data=collin_data)

lm_collin$coef

df <- augment(lm_collin)

ggplot(collin_data, aes(x=windspeed, y=time)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) +
    labs(x="Hitrost vetra v hrbet [m/s]",
         y="Čas teka [s]",
         title="Podatki z regresijsko premico")




# par(mfrow = c(2, 2), oma=c(0,0,3,0))
# plot(lm_collin)

ggplot(df, aes(x = .fitted, y = .resid)) +
    geom_point() +
    labs(x="Hitrost vetra v hrbet [m/s]",
         y="Čas teka [s]")

ggplot(df, aes(sample=time)) +
    geom_qq() +
    stat_qq_line()


