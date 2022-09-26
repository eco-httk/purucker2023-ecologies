library(ggplot2)

set.seed(1234)

df <- data.frame(
  Data=factor(rep(c("D1", "D2"), each=200)),
  weight=round(c(rnorm(200, mean=47, sd=5),
                 rnorm(200, mean=55, sd=5)))
)

d1dens <- with(df, density(weight[Data == "D1"], 
                           from = min(weight), 
                           to = max(weight)))
d2dens <- with(df, density(weight[Data == "D2"], 
                           from = min(weight),
                           to = max(weight)))
joint <- pmin(d1dens$y, d2dens$y)

df2 <- data.frame(x = rep(d1dens$x, 3), 
                  y = c(d1dens$y, d2dens$y, joint),
                  Data = rep(c("D1", "D2", "overlap"), each = length(d1dens$x)))

ggplot(df2, aes(x, y, fill = Data)) + 
  geom_area(position = position_identity(), color = "black") +
  scale_fill_brewer(palette = "OrRd") +
  theme_bw() +
  xlab("") + ylab("") +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
