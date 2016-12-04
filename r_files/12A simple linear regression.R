library("UsingR")
data(father.son)
sheight <- father.son$sheight
fheight <- father.son$fheight 

plot(sheight ~ fheight,bty="l",pch=20)
regression<-lm(sheight ~ fheight)
abline(regression, lty=1, lwd=2)

anovaRegression <- anova(regression)
sumSquares <- anovaRegression$`Sum Sq`

rho <- cov(fheight, sheight)/ (sd(fheight)*sd(sheight))

plot(resid(regression))

qqnorm(resid(regression))
qqline(resid(regression))

plot(fitted(regression),resid(regression))
plot(fitted(regression),rstandard(regression))

