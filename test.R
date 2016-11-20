x <- c(5, 5, 8, 10, 10, 10, 18, 19, 20, 20, 20, 30, 40, 42, 50, 50)

boot_np <- function(data, NBoot = 5000) {
  boots <- numeric(NBoot)
  for (i in 1:NBoot) {
    boots[i] <- mean(sample(data, replace = T))
  }
  CI <- quantile(boots, prob = c(0.025, 0.975))
  return (c(m = mean(data), CI))
}
boot_np(x, 50000)

param_CI <- function(data) {
  n = length(data)
  m = mean(data)
  SE = sd(data)/sqrt(n)
  E = qt(0.975, df = n - 1) * SE
  CI <- m + c(-E, E)
  return ((c(m, CI)))
}
param_CI(x)