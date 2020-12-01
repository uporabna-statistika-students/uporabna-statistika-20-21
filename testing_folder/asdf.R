temp <- read.csv("country-wise-average.csv",
           sep=",",
           dec=".",
           header=TRUE,
           na.strings=c("", " "))



write.table(temp,
            "data_test.txt",
            sep="\t",
            dec=",",
            row.names=FALSE,
            col.names=TRUE)

read.table("data_test.txt",
           sep="\t",
           dec=",",
           header=TRUE)
