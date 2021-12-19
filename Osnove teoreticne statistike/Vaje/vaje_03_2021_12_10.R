# 3.1

razmerje <- function(N, n=1000) {
    return(sqrt((N-n)/(N-1)))
}

curve(razmerje(x), from=1000, to=2000000)

pivot_longer(tvoj_df,
             cols = c("PM10",
                      "SO2",
                      "CO",
                      "O3",
                      "NO2"),
             names_to = "polutant",
             values_to = "polutant_value")
