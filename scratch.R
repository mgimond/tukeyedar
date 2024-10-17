M1 <- eda_lm(mtcars, hp, mpg, poly=0)
eda_rfs(M1, q=T)

M1 <- eda_lm(mtcars, hp, mpg, poly=1)
eda_rfs(M1, q=T)

M1 <- lm(mpg~ hp, mtcars)
eda_rfs(M1, q=T)

M1 <- eda_rline(mtcars, hp, mpg)
eda_rfs(M1, q=T)

eda_rfs(mtcars, mpg, am, q = T)
eda_rfs(mtcars, mpg, am, q = T, bar = FALSE)

library(lattice)
rfs(oneway(mpg ~ am, mtcars, spread = 1))

# Create a data frame with the two percentages
data <- data.frame(
  category = c("Category 1", "Category 2"),
  percentage1 = c(60, 40),
  percentage2 = c(30, 70)
)

# Calculate the cumulative percentages
data$cumulative_percentage1 <- cumsum(data$percentage1)
data$cumulative_percentage2 <- cumsum(data$percentage2)

# Create the stacked bar chart

def.par <- par(no.readonly = TRUE)
par(mar = c(0.2,0.2,0,0))
barplot( rbind(0.8, 0.4), col = c("#FAC4C3", "#EEFAE1"),border = FALSE,
  axes = TRUE, width = 2, omd = c(4, 4, 1,0.5))
par(def.par)


def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,2),1,2, byrow = TRUE))
par(mai = c(0.6,0,0.2,0))
plot(mtcars$hp, mtcars$mpg)
barplot( rbind(0.6, 0.5), border = FALSE,
         axes = FALSE, ylim = range(mtcars$mpg))
par(def.par)


for (i in 1:length(par1)) {
  if (!identical(par1[[i]], par2[[i]])) {
    print(paste("Element", i, "is different. \n",
                par1[[i]], par2[[i]]))
  }
}


