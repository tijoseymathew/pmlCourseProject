# Principal Components
# Visualization aid that shows the first two principal components of the data
# with color coded class points.
require(ggplot2)
pcPlot <- function(x, t, title=""){
  x <- as.matrix(x)
  pc <- prcomp(x, scale=T)
  xPC <- x %*% pc$rotation[,1:2]
  df <- data.frame(xPC, t)
  ggplot(df) + 
    geom_point(aes(x=PC1, y=PC2, colour=t, shape=t), size=5) + 
    ggtitle(title)  
}