mtcars
cs = chisq.test(mtcars)
cs
cs$mpg
cs2 = chisq.test(mtcars$mpg, mtcars$cyl)
cs2

flower <- iris
flower
nochar <- flower[, !sapply(flower, is.character)]
nochar
cs = chisq.test(iris)
cs
