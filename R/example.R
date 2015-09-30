set.seed(500)

source('./hivePlotWrapper.R')

sample_size <- 300

group <- sample(3, sample_size, replace = TRUE)
# Real data correlated with group
x <- group + rnorm(sample_size,0,0.5)
y <- x + rnorm(sample_size,0,1)

df <- data.frame(group,x,y)
plotHiveDF(df,df$group)