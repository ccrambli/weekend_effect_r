cardata <- mtcars

regression <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
confint(regression)
anova(regression)