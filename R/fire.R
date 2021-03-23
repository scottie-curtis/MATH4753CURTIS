# Read in the fire data

fire = read.csv("../Labs/Lab7/FIREDAM.csv")

# Check
head(fire,20)

library(ggplot2)
#install.packages("ggplot2")


# ggplot uses layers

g = ggplot(fire, aes(x = DISTANCE, y = DAMAGE ))

print(g)

# add some clusters

gt = g +  stat_ellipse(type = "t", level=0.95)
gt

gtp = g + stat_ellipse(geom="polygon", alpha = 0.6,level=0.95)

gtp

# need some more packages
library(ggpubr)
library(ggpmisc)

formula <- y~x + I(x^2) + I(x^3)
g  = g + geom_smooth(method = "lm", formula = formula)
g

lbyr <- c(12,13,14)*100
g = g+ stat_cor(label.y = lbyr)+
  stat_poly_eq(
    aes(label = ..eq.label..),
    formula = formula, label.y = c(21,22,23)*100, parse = TRUE) + xlim(0,60)

# or enter `g`

g

# add some labels

g = g + labs(subtitle="Distance Vs Damage",
             y="Damage",
             x="Distance",
             title="Scatterplot",
             caption = "Source: FIREDAM.csv")
g

# Use facets

#g  = g + facet_wrap(~SPECIES)
#g


# Use themes to control other aspects of the plot

g = g + theme(legend.position = "bottom")
g

g = g+theme(axis.text.x = element_text(angle=65, vjust=0.6))
g


## We can make fancy boxplots

b = ggplot(fire, aes(x = DISTANCE, y = DAMAGE))
b = b + geom_boxplot(aes(fill= SPECIES)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Box plot",
       subtitle="Distance Vs Damage",
       caption="Source: FIREDAM.csv",
       x="Distance",
       y="Damage")

b

# add facets

#b = b + facet_wrap(~SPECIES)
#b
