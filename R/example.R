set.seed(500)

source('./hivePlotWrapper.R')

group <- sample(3, 100, replace = TRUE)
# Real data correlated with group
x <- group + rnorm(100,0,0.5)
y <- x + rnorm(100,0,1)

df <- data.frame(group,x,y)
plotHiveDF(df,df$group)