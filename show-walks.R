library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

show_walks <- function(file_name) {
    d <- read_csv(file_name) %>% gather(property, value, -step, -walker)
    ggplot(d3, aes(x=step, y=value, color=property)) +
        geom_point() +
        facet_wrap(~walker)
}

show_pos <- function(file_name) {
    d <- read_csv(file_name)
    ggplot(d, aes(x=step, y=pos, color=walker)) + geom_line()
}
