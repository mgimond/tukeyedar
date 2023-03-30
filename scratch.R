# Infant mortality
edu <- c("ed8", "ed9to11", "ed12", "ed13to15", "ed16")
df1 <- data.frame(region =  rep( c("NE", "NC", "S", "W"), each = 5),
                 edu = rep( factor(edu, levels = edu), 4),
                 perc = c(25.3, 25.3, 18.2, 18.3, 16.3, 32.1, 29, 18.8,
                          24.3, 19, 38.8, 31, 19.3, 15.7, 16.8, 25.4, 21.1, 20.3, 24, 17.5))

df2 <- data.frame(grp = rep(paste0("G",1:6)),
                 r1 = c(6.39, 8.95, 5.61, 813.70, 4411.5, 32848.4),
                 r2 = c(8.1, 7.48, 8.58, 686.5, 3778.9, 28866),
                 r3 = c(6.08, 6.57,5.72,691.2, 4565.3, 46984.4))
df2 <- tidyr::pivot_longer(df2, names_to = "rep", values_to = "val", -grp)

df3 <- data.frame(year = rep(2016:2021, each=6),
                  type = rep(c("Car, Truck, or Van - Drove Alone",
                           "Car, Truck, or Van - Carpooled", "Public Transportation (Excluding Taxicab)",
                           "Walked", "Taxicab, Motorcycle, Bicycle, or Other Means", "Worked At Home"), 6),
                  perc = c(76.41, 9.32, 5.13, 2.76, 1.82, 4.57, 76.44, 9.15, 5.13, 2.73,
                           1.81, 4.73, 76.44, 9.08, 5.05, 2.69, 1.81, 4.93, 76.33, 9.01,
                           5, 2.67, 1.82, 5.17, 74.92, 8.85, 4.58, 2.57, 1.81, 7.26, 73.24,
                           8.59, 4.17, 2.48, 1.83, 9.7) )


## test IQRoQ on gene expression data
f1 <- function(x){
  out <- eda_pol(df2, row = "grp", col = "rep", val = "val",
                 p = x, plot=FALSE, tukey = FALSE)
  c(p=out$power, IQrow = out$IQ_row, IQcol = out$IQ_col)
}

IQ <- t(sapply(0:25/10,  FUN = f1 ))

OP <- par(mfrow = c(1,2))
plot(IQrow ~ p, IQ)
grid()
plot(IQcol ~ p, IQ)
grid()
par(OP)

## test infant mortality data
f1 <- function(x){
  out <- eda_pol(df1, row = "region", col = "edu", val = "perc",
                 p = x, plot=FALSE, tukey = FALSE)
  c(p=out$power, IQrow = out$IQ_row, IQcol = out$IQ_col)
}

IQ <- t(sapply(-25:25/10,  FUN = f1 ))

OP <- par(mfrow = c(1,2))
plot(IQrow ~ p, IQ)
grid()
plot(IQcol ~ p, IQ)
grid()
par(OP)

## Side by side plot sort vs unsorted
OP <- par(mfrow = c(1,2))
 eda_pol(df1, row = "region", col = "edu", val = "perc", sort = FALSE)
 eda_pol(df1, row = "region", col = "edu", val = "perc", sort = TRUE)
par(OP)

## test commute data
f1 <- function(x){
  out <- eda_pol(df3, row = "type", col = "year", val = "perc",
                 p = x, plot=FALSE, tukey = FALSE)
  c(p=out$power, IQrow = out$IQ_row, IQcol = out$IQ_col)
}

IQ <- t(sapply(-25:25/10,  FUN = f1 ))

OP <- par(mfrow = c(1,2))
plot(IQrow ~ p, IQ)
grid()
plot(IQcol ~ p, IQ)
grid()
par(OP)

## Side by side plot raw vs log
OP <- par(mfrow = c(1,2))
 eda_pol(df3, row = "type", col = "year", val = "perc", p = 0.25, adj.mar = T, row.size = 0.4)
 eda_pol(df3, row = "type", col = "year", val = "perc", p = 0, adj.mar = T, row.size = 0.4)
par(OP)

out <- eda_pol(df3, row = "type", col = "year", val = "perc", p = 0, adj.mar = TRUE, row.size = 0.8, sort = TRUE)
plot(out, row.size = 1, adj.mar = TRUE)
