.pardef <- par(no.readonly = TRUE)
on.exit(par(.pardef))

w   <-  dev.size("in")[1]
h   <-  dev.size("in")[2]
lmin <- min(w,h)
if(h < w){
  omi.w <- (w - lmin)/2
  omi.l <- 0.0 * lmin
} else {
  omi.l <- (h - lmin)/2
  omi.w <- 0.0 *lmin
}

# Set the margins to zero
par(mar = c(0,0,0,0), pty = "s", mfrow = c(3,3),
    omi = c(omi.l,omi.w,omi.l,omi.w),
    mai = c(0, 0 ,0 ,0 ))

# Create the plots within the grid
for (i in 1:9) {
  plot(1:10, main = paste("Plot", i), xaxt = "s", yaxt = "s")
}
#par(mfrow = c(1,1))
par(.pardef)


###############################



eda_qqmat(mtcars, mpg, cyl)
eda_qqmat(mtcars, mpg, am, resid= TRUE)


lst <- split(mtcars$mpg, mtcars$cyl)
lst2 <- lapply(lst, FUN = function(x){x - mean(x)})
range(unlist(lst2))


fac_num <- 4

#graphics.off()

num.plots <- (fac_num^2)

# Note that the layout/lcm combo can generate an invalid graphics state
# when the plot window is resized (as expected). But when rerunning the
# code chunk that extracts the new plot window size and regenerates a layout
# R will continue complaining that the plot device is in an invalid state.
# The workaround seems to be to generate a bnak plot before recreating the
# layout
#
.pardef <- par(no.readonly = TRUE)
on.exit(par(.pardef))
tryCatch({
  layout1(fac_num, num.plots)
}, error = function(e) {
  cat("Error:", e$message, "\n")
  # Handle the error, e.g., provide alternative visualization or log the issue
  par(mfrow = c(1,1))
  plot(0, type='n',axes=FALSE,ann=FALSE)
  layout1(fac_num, num.plots)
})

# Set the margins to zero
par(mar = c(0,0,0,0), pty = "s")

# Create the plots within the grid
for (i in 1:num.plots) {
  plot(1:10, main = paste("Plot", i))
}
#par(mfrow = c(1,1))
par(.pardef)

##
par("mfrow")
##

layout1 <- function(fac_num, num.plots){
  lmin <- min(dev.size("cm")) # Get smallest window side in cm
  dim.cm <- lmin / fac_num * 1.3
  layout(matrix(1:num.plots, nrow = fac_num),
         widths = rep(lcm(dim.cm), num.plots),
         heights = rep(lcm(dim.cm), num.plots))
}

layout2 <- function(fac_num, num.plots){
  lmin <- min(dev.size("cm")) # Get smallest window side in cm
  dim.cm <- lmin / fac_num * 1.3
  layout(matrix(1:num.plots, nrow = fac_num),
         widths = 1,
         heights = 1)
}

