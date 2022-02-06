####################################

# Tworzymy losow¹ zmienn¹ x i przypisujemy jej wartoœæ y z rozk³adu normalnego; 

library(ggplot2)
x <- sample(1:100, size = 2500, replace = TRUE)
y <- sapply(x, function(x) rnorm(1,0,x))
dane <- data.frame(x,y)

# tworzymy model regresji

model <- lm(y~x+I(x^2), data = dane)
smooth.spline(x, y, df = 100)
predict(smooth.spline(x,y, df = 100), 100)$y
z = c(-5,110)
q <- predict(model, data.frame(x = z))
linia <- data.frame(z,q)
ggplot()+geom_point(data = dane, aes(x=x, y=y))+
  geom_line(data = linia, aes(x=z,y=q), color='red', size = 2)
MSE	<- mean((dane$y - predict(model, data.frame(1)))^2)
MSE

#################################################
# Funkcje w³asne

MojaFunkcja <- function(x, y) {x*x+y*y}
MojaFunkcja(2,3)

MyFunction <- function(t,z) {
  value = z*3
  value = value *t
  return(value)}
MyFunction(2,3)


#################################################
# Pobieranie danych z wybieranego Ÿród³a

mydata <- read.csv(file.choose())


#################################################
# Pobieranie danych z Excela

install.packages('readxl')
library(readxl)

# wczytanie ca³ego pliku
excel_sheets('C:/Users/CP24/Downloads/Dane.xlsx')

# wczytanie arkusza Arkusz1
DaneNowe <- read_excel('C:/Users/CP24/Downloads/Dane.xlsx', sheet = 'Arkusz1')


