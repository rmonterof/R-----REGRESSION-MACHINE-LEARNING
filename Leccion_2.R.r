edad=c(1,2,3,5,7,9,11,13)
altura=c(76.11,86.45,95.27,109.18,122.03,133.73,143.73,156.41)
edad
altura
datos1=data.frame(edad,altura)
datos1
plot(datos1)
lm(altura~edad, data=datos1)
abline(lm(altura~edad, data=datos1))
summary(lm(altura~edad, data=datos1))
summary(lm(altura~edad, data=datos1))$r.squared
datos1$edad
#
df_pearson=read.table("http://aprender.uib.es/Rdir/pearson.txt", header=TRUE)
str(df_pearson)
head(df_pearson)
lm(Hijos~Padres, data=df_pearson)
summary(lm(Hijos~Padres, data=df_pearson))$r.squared
plot(df_pearson)
abline(lm(Hijos~Padres, data=df_pearson))
#
inh=c(19,36,60,84)
ser=c(1.2,3.6,12,33)
plot(inh,ser)
plot(inh, ser, log="y")
log10(ser)
lm(log10(ser)~inh)
summary(lm(log10(ser)~inh))$r.squared
plot(inh, ser)
curve(0.52*1.052^x, add=TRUE)
#
tiempo=1:12
SIDA_acum=c(97,709,2698,6928,15242,29944,52902,83903,120612,161711,206247,257085)
df_SIDA=data.frame(tiempo, SIDA_acum)
plot(df_SIDA)
plot(df_SIDA, log="y")
plot(df_SIDA, log="xy")
lm(log10(SIDA_acum)~log10(tiempo), data=df_SIDA)
summary(lm(log10(SIDA_acum)~log10(tiempo),data=df_SIDA))$r.squared
plot(df_SIDA)
curve(82.79422*x^3.274, add=TRUE)
