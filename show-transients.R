# dotnet run transients --demo > transients-demo.csv

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

d <- read_csv("transients-demo.csv")

p <- ggplot(d %>%
    select(-raw2, -smoothed2, -attacks2) %>%
    filter(Key < 301) %>%
    gather(series, value, -Key), aes(x=Key, y=value, color=series)) + geom_point()
