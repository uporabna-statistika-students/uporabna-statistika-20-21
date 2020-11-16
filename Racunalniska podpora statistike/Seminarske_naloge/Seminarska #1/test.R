library(tidyverse)

# defining ocenaA() function
ocenaA <- function(n){
    ### returns an approximate of number e - method a
    ### Input:
    ###     n - integer, number of samples

    # sample units with replacement (a simulation of drawing numbers from units)
    # from a vector of 1 and 0, where probability is 1/n for 1 and 1-1/n for 0.
    # replicate the drawing process n number of times for simulation purposes.
    # and sum column wise as it returns a matrix
    sampled_units <- colSums(replicate(n, sample(c(1, 0),
                                                 n,
                                                 replace=TRUE,
                                                 prob=c((1/n),
                                                        (1-(1/n)))
                                                 )
                                       )
                             )
    # if sum of values was equal or larger then 1 then we set to 0 as it does contain
    # number 1, otherwise we set to 1 as it does not contain number 1.
    does_not_contain_1 <- ifelse(sampled_units >= 1, 0, 1)
    # returning the inverse of mean of does_not_contain_1 which is an approximate of e.
    return(1/mean(does_not_contain_1))
}

ocenaB <- function(n){
    ### returns an approximate of number e - method b
    ### Input:
    ###     n - integer, number of samples

    # generate 2 numbers (x_1 and x_2) with mean=0 and sd=1.
    # square them and them sum them.
    # replicate n number of times
    sampled_units <- replicate(n, sum(rnorm(2, mean=0, sd=1)^2))
    # check if it's larger or equal to 2, set value to 1, otherwise set to 0
    larger_equal_2 <- ifelse(sampled_units >= 2, 1, 0)
    # returning the inverse of mean of larger_equal_2 as this is an approximate of e.
    return(1/mean(larger_equal_2))
}

# test n
n_size <- 1000

# this is a test for ocenaA()
ocenaA(n_size)

# this is a test for ocenaB()
ocenaB(n_size)

rm(n_size)

# defining different n
n_sizes <- c(32, 128, 512, 2048, 8192)

# create a results list of length = length(n_sizes)
resultsA <- vector("list", length = length(n_sizes))
resultsB <- vector("list", length = length(n_sizes))
# change names of lists for easier handling and not creating more than needed lists within
# resultsA
names(resultsA) <- as.character(n_sizes)
names(resultsB) <- as.character(n_sizes)

# defining m for number of simulation of number e.
m_reps <- 1000

set.seed(8)

# loop over n_sizes
for (n in n_sizes) {
    # just for viewing the current stage
    print(n)
    # repeating ocenaA() function with units and n size m_reps number of times
    # converting to list as to properly save to resultsA list.
    resultsA[as.character(n)] <- list(replicate(m_reps, ocenaA(n)))
    resultsB[as.character(n)] <- list(replicate(m_reps, ocenaB(n)))
}

summary_helper <- function(data, conf.interval=0.95) {
    ### helper function that returns, count, SD, SE and CI at specified conf.interval
    ### input:
    ###     data - a named list of lists of simulated number e values.

    # creating a tibble from data.frame (as data.frame correctly changes a list of lists)
    tb_data <- tibble(data.frame(data))
    # changing to long data format to work with other functions
    tb_data <- pivot_longer(tb_data,
                            cols=colnames(tb_data),
                            names_to="n",
                            values_to="simulated_e")

    tb_data <- tb_data %>%
        # removing infinite values to not return inf when creating a summary, therefore
        filter(!is.infinite(simulated_e)) %>%
        # grouping by n
        group_by(n) %>%
        # creating new summary columns
        summarise(m = n(), # count
                  mean = mean(simulated_e), # mean
                  sd = sd(simulated_e)) %>% # standard deviation
        mutate(se = sd/sqrt(m), # standard error
               n_temp = str_remove_all(n, "X")) %>% # temp column
        # change n_temp to double type
        mutate_at("n_temp", as.double) %>%
        # copy to n column
        mutate(n = n_temp) %>%
        # remove n_temp
        select(-n_temp)


    # calculate CI multiplier
    tb_data <- tb_data %>% mutate(conf_int = conf.interval/2 + 0.5,
                                  df = m - 1) %>%
        mutate(ci_mult = qt(conf_int, df)) %>%
        select(-conf_int, -df)
    # calculate CI
    tb_data <- tb_data %>%
        mutate(ci = se * ci_mult) %>%
        select(-ci_mult) %>%
        arrange(n)

    # return the summary tibble
    return(tb_data)
}


