### creating a dataframe

day = c('Mon','Tue','Wed','Thur','Fri', 'Sat','Sun')
temp = c(28, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed =c('T','F','F', 'F','T', 'F','T')
RPI_Weather_Week = data.frame(day, temp, snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)
RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'day']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5, c('day', 'temp')]
RPI_Weather_Week$temp
subset(RPI_Weather_Week, subset = snowed==TRUE)
help("subset")
sorted.snowed = order(RPI_Weather_Week['snowed'])
sorted.snowed
help("order")
RPI_Weather_Week[sorted.snowed,]

### RPI_Weather_Week[descending_snowed,]
dec.snow = order(RPI_Weather_Week$temp)
dec.snow
RPI_Weather_Week[dec.snow,]

### Creating an empty dataframe
empty.DataFrame = data.frame()
v1 = 1:10
v1 
letters
v2 = letters[1:10]
df = data.frame(col.name.1 = v1, col.name.2 = v2)
df
help("data.frame")

### importing data and expoorting data
write.csv(df, file = 'saved_df1.csv')
df2 = read.csv('saved_df1.csv')
df2

GPW3 = read.csv(file.choose(), header = T)
GPW3
View(GPW3)
EPI_2010 = read.csv(file.choose(), header = T)
View(EPI_2010)
EPI_2010 = read_excel(file.choose())
View(EPI_2010)

### Exercise 1. Exploring the distribution 
attach(EPI_Data)
fix(EPI_Data)
help("fix")
EPI
tf = is.na(EPI)
E = EPI[!tf]
help('is.na')
summary(EPI)
fivenum(EPI, na.rm = TRUE)
help('na.rm')
stem(EPI)
hist(EPI)
hist(EPI,seq(30., 95., 1.0), prob = TRUE)
help("seq")
help("hist")
lines(density(EPI, na.rm = TRUE, bw = 1.))
rug(EPI)
help("lines")
help('rug')
help("density")

### Exercise 1.Cumulative density function, Quantile-Quantile plot
plot(ecdf(EPI), do.points = FALSE, verticals = TRUE)
par(pty = 's')
qqnorm(EPI)
qqline(EPI)
x = seq(30, 95, 1)
qqplot(qt(ppoints(250), df = 5), x, xlab = 'Q-Q plot for t dsn')
qqline(x)
help("qqplot")
help('qqline')

### Exercise 1. fitting a distribution, DALY 
attach(EPI_Data)
fix(EPI_Data)
DALY
tf = is.na(DALY)
E = DALY[!tf]
summary(DALY)
fivenum(DALY)
stem(DALY)
hist(DALY)
lines(density(DALY, na.rm = TRUE, bw = 'SJ'))
rug(DALY)
plot(ecdf(DALY), do.points = FALSE, verticals = TRUE)
par(pty ='s')
qqnorm(DALY)
qqline(DALY)

## Comparing distributions 
boxplot(EPI,DALY)
qqplot(EPI,DALY)
help(qqplot)

# Exerice 2. Filtering (populations)
EPILand = EPI[!Landlock]
ELand = EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0))

help('lm')

### Lab 1 Part 2 
multivariate = read.csv(file.choose(),header = TRUE)
multivariate 
attach(multivariate)
mm = lm(Homeowners ~ Immigrant)
mm
head(multivariate)
summary(mm)
summary(mm)$coef # Pr(>|t|)<0.05, the p-value for that t-test, statistically significant
# F-statistic, the ratio of two variances 
plot(Homeowners ~ Immigrant)
help(abline)
abline(mm)
abline(mm, col=2, lwd=3)
newImmigrantdata = data.frame(Immigrant = c(0, 20))
mm = predict.lm(newImmigrantdata) ##?? no applicable method for 'predict'applied
help("predict")

abline(mm)
abline(mm, col = 3, lwd =3)
attributes(mm)
mm$coefficients

### ggplot examples
# Chapter 2 -- R Graphics Cookbook
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg) # quick plot 
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(wt,mpg))+ geom_point()
help(ggplot)
help(qplot)
help("geom_point")
plot(pressure$temperature, pressure$pressure, type = 'l')
points(pressure$tempeature, pressure$pressure)

lines(pressure$temperature, pressure$temperature/2, col = 'red')
points(pressure$temperature, pressure$pressure/2, col ='blue')
qplot(pressure$temperature, pressure$pressure, geom = 'line')
ggplot(pressure, aes(temperature, pressure)) + geom_line() + geom_point()

# Creating Bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()
help("factor")

qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(factor(cyl))) +geom_bar()


# Creating Histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) # specify number of bins with break 
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg,data = mtcars, binwidth = 4)
ggplot(mtcars,aes(mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars,aes(mpg)) + geom_histogram(binwidth = 5)

# Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = 'boxplot')
qplot(supp, len, data = ToothGrowth, geom = 'boxplot')
ggplot(ToothGrowth, aes(supp, len)) + geom_boxplot()
#using three seperate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len,geom = 'boxplot')
ggplot(ToothGrowth, aes(interaction(supp, dose), len)) + geom_boxplot()
