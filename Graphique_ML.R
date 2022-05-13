## EXERCICE 13-05

install.packages("ggplot2")
library(ggplot2)

names(iris)
iris

graphML <- ggplot(data = iris) +
  aes(x = species, y = Sepal.Length) +
  geom_histogram()

print(graphML)
