x <- c(23,15,46,NA)
z <- c(5,6,NA,8)

mean(x)
mean(x, na.rm = TRUE)
mean(z, na.rm = TRUE)

sum(x)
sum(x, na.rm = TRUE)

d <- data.frame(rost = x, ves = z)
