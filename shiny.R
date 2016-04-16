library(shiny)
runExample("01_hello")

setwd("/home/lanzheng")
runApp("kfzx")
library(ggvis)
head(pressure)
head(mtcars)
ggvis(pressure, props(x = ~temperature, y = ~pressure, y2 = 0)) +
  mark_rect(props(width := 10))


library(ggvis)
library(dplyr)

pressure %>%   ggvis() %>%  layer_bars(x=~temperature, y=~pressure,width = 10)



set.seed(1014)
df <- data.frame(x1 = runif(5), x2 = runif(5), y1 = runif(5), y2 = runif(5))
df %>% ggvis(~x1, ~y1, x2 = ~x2, y2 = ~y2, fillOpacity := 0.1) %>% layer_rects()