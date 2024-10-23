library(grid)

x <- runif(20, 0, 1000)
y <- runif(20, 0, 2000)
lim.buffer = 0.05
xylim <- range(x,y)
xylim <- c(xylim[1] - diff(xylim) * lim.buffer  , xylim[2] + diff(xylim) * lim.buffer)

x.plots <- 3
y.plots <- 3

# Compute margins needed to accomodate labels
label <- as.character(max(xylim))
label_width <- convertWidth(stringWidth(label), "lines", valueOnly = TRUE)
x_margin <- unit(label_width + 0.5, "lines")
y_margin <- unit(4, "lines")  # Horizontal margin for x-axis text

grid.newpage()


#main <- viewport(width = 0.90, height = 0.90, layout=grid.layout(3, 3, respect = TRUE))
main <- viewport(width = unit(1, "npc") - x_margin,
                 height = unit(1, "npc") - y_margin,
                 layout=grid.layout(3, 3, respect = TRUE))
pushViewport(main)

for (j in 1:x.plots){
  for (i in 1:y.plots){
    vp<-viewport(layout.pos.col = i, layout.pos.row = j, xscale = xylim, yscale = xylim)
    pushViewport(vp)
    grid.rect()

    if( i != j){
      grid.points(x=unit(x,"native"), y=unit(y,"native"), gp = gpar(fill = "red", col = "green"), pch = 20)
      grid.lines()
    } else {
      grid.text("DIAG", gp = gpar(col = "grey"))
    }

    # Add y-axis
    if( (i == 1) & (j %% 2 !=0)) {
      grid.yaxis(gp = gpar(cex = 0.8))
    } else if( (i == x.plots) & (j %% 2 ==0)) {
      grid.yaxis(gp = gpar(cex = 0.8), main = FALSE)
    }

    # Add x-axis
    if(j == 1 & (i %% 2 == 0)) {
      grid.xaxis(main = FALSE, gp = gpar(cex = 0.8))
    } else if (j == y.plots & (i %% 2 != 0)){
      grid.xaxis( gp = gpar(cex = 0.8))
    }
    #  grid.text(paste(i,j)) # Used to debug plot placement

    upViewport()
  }
}

popViewport(0)
