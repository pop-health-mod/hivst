library(wpp2024)
library(rstan)

# we take Kenya as a test countries
data(popB1)
wpp_b <- popB1[popB1$name == "Kenya", !(colnames(popB1) %in% as.character(c(1949:2010))) ]
data(popM1)
wpp_m <- popM1[popM1$name == "Kenya", !(colnames(popM1) %in% as.character(c(1949:2010))) ]
data(popF1)
wpp_f <- popF1[popF1$name == "Kenya", !(colnames(popF1) %in% as.character(c(1949:2010))) ]

b15 <- wpp_b[wpp_b$age == 15, ]
m15 <- wpp_m[wpp_m$age == 15, ]
f15 <- wpp_f[wpp_f$age == 15, ]

data(mxB1)
wpp_dr <- mxB1[mxB1$name == "Kenya", !(colnames(mxB1) %in% as.character(c(1949:2010, 2023:2100)))]
plot(wpp_dr$`2011` ~ wpp_dr$age, type = 'l', lwd= 2, col = "dodgerblue4")
data(mxM1)
wpp_dr_m <- mxB1[mxM1$name == "Kenya", !(colnames(mxM1) %in% as.character(c(1949:2010, 2023:2100)))]
data(mxF1)
wpp_dr_f <- mxF1[mxF1$name == "Kenya", !(colnames(mxM1) %in% as.character(c(1949:2010, 2023:2100)))]
plot(wpp_dr_m$`2011` ~ wpp_dr_m$age, type = 'l', lwd= 2, col = "dodgerblue4")
lines(wpp_dr_f$`2011` ~ wpp_dr_f$age, type = 'l', lwd= 2, col = "pink4")

dt <- 1
start <- 2011
end <- 2023
niter <- (end - start) / dt + 1
entrants <- unlist(b15[colnames(b15) %in% as.character(start:end)])
death <- t(wpp_dr[wpp_dr$age >= 15, colnames(wpp_dr) %in% as.character(start:end)])

pop <- matrix(data = 0, nrow = niter, ncol = (100 - 15 + 1))
pop[1, ] <- wpp_b$`2011`[wpp_b$age >= 15]
rownames(pop) <- start:end

for (i in 2:niter) {
  pop_i <- pop[i - 1, ]
  pop[i, 1] <- entrants[i]
  for (a in 2:(100 - 15 + 1)) {
    pop[i, a] <- pop_i[a - 1] + dt * (- death[i - 1, a - 1] * pop_i[a - 1])
  }
}

mod <- pop[niter, ]
wpp <- wpp_b$`2022`[wpp_b$age >= 15]
summary(mod / wpp)

library(dplyr)
df <- data.frame(
  age = rep(15:100, 2),
  comp = rep(c("mod", "wpp"), each = 86),
  pop = c(mod, wpp))

# Create a new column for plotting
df <- df %>%
  mutate(pop = ifelse(comp == "mod", -pop, pop))

# Plot using ggplot2
library(ggplot2)
ggplot(df, aes(x = age, y = pop, fill = comp)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = abs) +  # Show positive values on y-axis
  labs(title = "Population Pyramid", x = "Age", y = "Population") +
  theme_minimal()



