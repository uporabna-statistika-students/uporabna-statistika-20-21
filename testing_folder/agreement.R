library(tidyverse)
library(vcd)
library(readxl)

# uvozimo podatke
data <- read_excel("project.xlsx")

# izberemo le potrebne podatke
data <- data %>%
    select(ESK0.123, CEUS0.123)

# nadomestime pike z NA vrednostmi
data$ESK0.123[data$ESK0.123 == "."] <- NA
data$CEUS0.123[data$CEUS0.123 == "."] <- NA

data$ESK0.123[data$ESK0.123 == "1.0"] <- "Aktivna"
data$ESK0.123[data$ESK0.123 == "0.0"] <- "Neaktivna"
data$CEUS0.123[data$CEUS0.123 == "1.0"] <- "Aktivna"
data$CEUS0.123[data$CEUS0.123 == "0.0"] <- "Neaktivna"

# # spremenimo iz tipa character v tip double
# data <- data %>%
#     mutate_if(is.character, as.double)

# za kontingencno tabelo
cnt_tbl <- table(data)
cnt_tbl

# da vidimo se v drugacni obliki katere vrednosti so kje (NA vrednosti so vkljucene)
count(data %>% group_by(ESK0.123, CEUS0.123))

# narisemo agreementPlot() s transponirano cnt_tbl tabelo.
agreementplot(t(cnt_tbl))
