#Jacob Gutierrez 
#3/13/19 
#Multiplot function for ggplot 2 found at:
#https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs


#I personalized this function so that it accepts a list of plot gorbs directly instead of that weird p1, p2, p3 nonsense. 


#Arguments >>>
  #plots : list of gorbs
  #ncol: how many columns in the plot
  #nrow: how many rows in the plot
  #position: Where the shared legend will go
  #title: MAKE THE TITLE HERE JACOB 

# Libraries 
library(gridExtra)
library(grid)

# Normally first argument is '...'
grid_arrange_shared_legend <- function(plots, ncol = length(plots), nrow = 1, position = c("bottom", "right"), title = 'Title') {
  
  #plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            top = title,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           top = title,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}