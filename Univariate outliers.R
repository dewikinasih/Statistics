library(readxl)
dtuts <- read_excel('dtuts.xlsx')

#Eksplorasi
str(dtuts)
summary(dtuts)
dtuts$NAMAKAB <- as.factor(dtuts$NAMAKAB)
dtuts$NAMAKEC <- as.factor(dtuts$NAMAKEC)
par(mfrow=c(1,3))

#Identifikasi dengan boxplot
boxplot(dtuts$FOOD)
boxplot(dtuts$NONFOOD)
boxplot(dtuts$EXP_CAP)

#Metode IQR untuk satu variabel
periksa = dtuts$EXP_CAP

# Ukuran statistik ketika masih ada outlier
length(periksa)
summary(periksa)
a= mean(periksa)
b= median(periksa)
c= sd(periksa)
d= mad(periksa) #median absolute deviation
sebelum = data.frame(a, b, c, d)
sebelum

batas_atas = quantile(periksa, probs = 0.75) + 1.5*IQR(periksa)
periksa[periksa > batas_atas]
periksa1 = periksa[periksa<batas_atas]

batas_atas2 = quantile(periksa1, probs = 0.75) + 1.5*IQR(periksa1)
periksa1[periksa1 > batas_atas2]
periksa1 = periksa1[periksa1<batas_atas2]

batas_atas3 = quantile(periksa1, probs = 0.75) + 1.5*IQR(periksa1)
periksa1[periksa1 > batas_atas3]
periksa1 = periksa1[periksa1<batas_atas3]

batas_bawah = quantile(periksa1, probs = 0.25) - 1.5*IQR(periksa3)
periksa3[periksa3 < batas_bawah]

periksa_tanpa_outlier = periksa3[periksa3>batas_bawah]

a1=mean(periksa_tanpa_outlier)
b1=median(periksa_tanpa_outlier)
c1=sd(periksa_tanpa_outlier)
d1=mad(periksa_tanpa_outlier)
sesudah = data.frame(a1, b1, c1, d1)

#fungsi
remove_outliers <- function(s){
  Q1 <- quantile(s, probs = 0.25)
  Q3 <- quantile(s, probs = 0.75)
  Limit <- 1.5 * IQR(s)
  no_outliers <- subset(s, s>(Q1-Limit) & s<(Q3+Limit))
  boxplot(no_outliers)
  return(no_outliers)
}
