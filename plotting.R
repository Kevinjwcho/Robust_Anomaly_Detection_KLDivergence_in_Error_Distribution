# install.packages("data.table")
# install.packages("lubridate")
# install.packages("dplyr")
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)

err_files <- list.files(path = "C:/Users/kevin/OneDrive/Desktop/우사이먼교수님_프로젝트/newnew", full.names = TRUE, recursive = TRUE)
err_list <- lapply(err_files, fread, key = "time")
err_list


err_list <-
  lapply(err_list, function(x) x[,
                                 time := ymd_hms(time)])

err_tr <-
  lapply(err_list, function(x) {
    tidx <- x[1, time] + days(7) # first seven days
    x[time < as.Date(tidx)]
  })


err_plot <-
  lapply(err_tr, function(x) {
    x %>% 
      melt(id.vars = c("time", "attack")) %>% 
      ggplot(aes(x = time, y = value, group = variable)) +
      geom_path(color = "red") +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_x_datetime() 
    # +
    #   scale_colour_discrete(
    #     name = "anomaly",
    #     label = c("normal (0)", "anomaly (1)")
    #   ) +
    #   theme(legend.position = "bottom")
  })


err_plot[[1]]
