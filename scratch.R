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

df4 <- data.frame(Education = c("Less than High School Graduate",
                                "High School Graduate (Includes Equivalency)",
                                "Some College or Associate's Degree",
                                "Bachelor's Degree", "Graduate or Professional Degree",
                                "Less than High School Graduate",
                                "High School Graduate (Includes Equivalency)",
                                "Some College or Associate's Degree",
                                "Bachelor's Degree",
                                "Graduate or Professional Degree"),
                  Sex = c(rep("Male", 5), rep("Female",5)),
                  Earnings_2021 = c(31722, 40514, 49288, 73128,98840,20448,
                                    26967, 33430, 50554, 67202))

# Tukey's p.354 data
df5 <- data.frame(loc = rep( c("Laredo", "Washington", "Caribou"), 7),
                  month = rep(month.abb[1:7], each = 3),
                  val = c(57.6, 36.2, 8.7,61.9,37.1,9.8,68.4, 45.3, 21.7,
                          75.9, 54.4,34.7, 81.2,64.7,48.5,85.8, 73.4, 58.4,
                          87.7, 77.3, 64))


## test IQRoQ on gene expression data
f1 <- function(x){
  out <- eda_pol(df2, row = grp, col = rep, val = val,
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
  out <- eda_pol(df1, row = region, col = edu, val = perc,
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
 eda_pol(df1, row = region, col = edu, val = perc, sort = FALSE)
 eda_pol(df1, row = region, col = edu, val = perc, sort = TRUE)
par(OP)

## test commute data
f1 <- function(x){
  out <- eda_pol(df3, row = type, col = year, val = perc,
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
 eda_pol(df3, row = type, col = year, val = perc, p = 0.25, adj.mar = T, row.size = 0.4)
 eda_pol(df3, row = type, col = year, val = perc, p = 0, adj.mar = T, row.size = 0.4)
par(OP)

out <- eda_pol(df3, row = type, col = year, val = perc, p = 0, adj.mar = TRUE, row.size = 0.8, sort = TRUE)
plot(out, adj.mar = TRUE,  row.size = 0.5)


## Earnings by education and sex (2021 data for US)
out <- eda_pol(df4, row = Education, col = Sex,
               val = Earnings_2021, adj.mar = TRUE,
               sort = TRUE, row.size = 0.6)
plot(out, type = "diagnostic")
out2 <- eda_pol(df4, row = Education, col = Sex,
                val = Earnings_2021, adj.mar = TRUE,
                sort = TRUE, row.size = 0.6, p = 0.11)
plot(out2, type = "diagnostic")
plot(out2,  col.eff = FALSE, colpal = "Blue-Red")
plot(out,  col.eff = FALSE, colpal = "Blue-Red")

f4 <- function(x){
  out <- eda_pol(df4, row = Education, col = Sex, val = Earnings_2021,
                 p = x, plot=FALSE, tukey = FALSE)
  c(p=out$power, IQrow = out$IQ_row, IQcol = out$IQ_col)
}

IQ <- t(sapply(0:25/10,  FUN = f4 ))

OP <- par(mfrow = c(1,2))
plot(IQrow ~ p, IQ)
grid()
plot(IQcol ~ p, IQ)
grid()
par(OP)

# Tukey's p 354 data
out <- eda_pol(df5, row = month, col = loc, val = val, sort = TRUE)
plot(out)
plot(out, "diagnostic")
plot(out, "cv")
plot(out, add.cv = TRUE, k = -1)


## lm ----
eda_lm(mtcars, x = wt, y = mpg, grey = 0.8)
Mr <- eda_rline(mtcars, x=wt, y=mpg)
eda_add(Mr)

eda_lm(dat=cars, x=log(dist), y=log(speed), q = T)
Mr <- eda_rline(cars, x=dist, y=speed )
eda_add(Mr)

qy <- quantile(cars$speed, c(0.34,0.84))
qx <- quantile(cars$dist, c(0.34,0.84))
rect(xleft = qx[1], xright = qx[2], ybottom = qy[1], ytop = qy[2], col = rgb(0,0,0,0.2), border = "grey")

## Random bivariate data
n = 1000
r = 0.5
set.seed(321)
x1 = rnorm(n)
x2 = rnorm(n)
y1 = scale(x2) * r  +  scale(residuals(lm(x1~x2))) * sqrt(1-r*r) # https://stats.stackexchange.com/a/112160
df <- data.frame(x=x2, y = y1)
eda_lm(dat=df, x=x, y=y, q = T)
#eda_lm(dat=df, x=x, y=y, q = T, q.val = c(0.25,0.75))
Mr <- eda_rline(dat=df, x=x, y=y)
eda_add(Mr)
plot(Mr, equal = TRUE)

# Neoplasms
eda_lm(dat=neoplasms, x=Temp, y=Mortality, q = T, q.type = 9)
Mr <- eda_rline(dat=neoplasms, x=Temp, y=Mortality)
eda_add(Mr)

# Age Height
eda_lm(dat=age_height, x=Months, y=Height, q = T, q.type = 9)
Mr <- eda_rline(dat=age_height, x=Months, y=Height)
eda_add(Mr)
