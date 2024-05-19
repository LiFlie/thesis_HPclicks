library(readxl)

Ref_DE_Aschau <- read_excel("AS_78 2023 02 28 FPOD_6889_train_details_filtered.xlsx")
View(Ref_DE_Aschau)

Ref_DE_BoEck <- read_excel("BoEck 2023 02 07 FPOD_6877_train_details.xlsx")
View(Ref_DE_BoEck)

Ref_DE_Langholz <- read_excel("LA_77 2023 02 28 FPOD_6888_train_details_filtered.xlsx")
View(Ref_DE_Langholz)

#bind all dataframes from german ref stations
Ref_DE <- rbind(Ref_DE_Aschau, Ref_DE_BoEck, Ref_DE_Langholz)
View(Ref_DE)
     
Ref_DK_6621 <- read_excel("2023 02 21 FPOD_6621 file0_train_details.xlsx")
View(Ref_DK_6621)

Ref_DK_6623 <- read_excel("2023 02 21  2023 02 21 FPOD_6623 file0 train details.xlsx")
View(Ref_DK_6623)

Ref_DK_6625 <- read_excel("20230221 2023 02 21 FPOD_6625 file0 train details.xlsx")
View(Ref_DK_6625)

#bind all dataframes from danish ref stations
Ref_DK <- rbind(Ref_DK_6621, Ref_DK_6623, Ref_DK_6625)
View(Ref_DK)

##GERMANY 

#number of ICIS under 67s (1.5ms): 35486 --> ~76%
Aschau_ICI <- Ref_DE_Aschau[which (Ref_DE_Aschau$AvPRF <= 67), ]
hist(Ref_DE_Aschau$AvPRF, xlab = "reciprocal of mean ICI in s", breaks = 100)
hist(Ref_DE_Aschau$AvPRF, xlab = "reciprocal of mean ICI in s", xlim = c(0,200), breaks = 500)
axis(1, at = seq(0, 50, by = 5), labels = seq(0, 50, by = 5))
hist(1/(Ref_DE_Aschau$AvPRF), xlab = "mean ICI in s", xlim = c(0,0.2), breaks = 500)
axis(1, at = seq(0.0, 0.05, by = 0.01), labels = seq(0.0, 0.05 , by = 0.01))
hist((1/(Ref_DE_Aschau$AvPRF))*100, xlab = "mean ICI in ms", xlim = c(0,20), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

#number of ICIS under 67s (1.5ms): 24427 --> ~84%
BoEck_ICI <- Ref_DE_BoEck[which (Ref_DE_BoEck$AvPRF <= 67), ]
hist(Ref_DE_BoEck$AvPRF, xlab = "reciprocal of mean ICI in s", breaks = 100)
hist(Ref_DE_BoEck$AvPRF, xlab = "reciprocal of mean ICI in s", xlim = c(0,200), breaks = 500)
axis(1, at = seq(0, 50, by = 5), labels = seq(0, 50, by = 5))
hist(1/(Ref_DE_BoEck$AvPRF), xlab = "mean ICI in s", xlim = c(0,0.2), breaks = 500)
axis(1, at = seq(0.0, 0.05, by = 0.01), labels = seq(0.0, 0.05, by = 0.01))
hist((1/(Ref_DE_BoEck$AvPRF))*100, xlab = "mean ICI in ms", xlim = c(0,20), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

#number of ICIS under 67s (1.5ms): 57830 --> ~80,7%
Langholz_ICI <- Ref_DE_Langholz[which (Ref_DE_Langholz$AvPRF <= 67), ]
hist(Ref_DE_Langholz$AvPRF, xlab = "reciprocal of mean ICI in s", breaks = 100)
hist(Ref_DE_Langholz$AvPRF, xlab = "reciprocal of mean ICI in s", xlim = c(0,200), breaks = 500)
axis(1, at = seq(0, 50, by = 5), labels = seq(0, 50, by = 5))
hist(1/(Ref_DE_Langholz$AvPRF), xlab = "mean ICI in s", xlim = c(0,0.2), breaks = 500)
axis(1, at = seq(0.0, 0.05, by = 0.01), labels = seq(0.0, 0.05, by = 0.01))
hist((1/(Ref_DE_Langholz$AvPRF))*100, xlab = "mean ICI in ms", xlim = c(0,20), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

par(mfrow = c(1, 3))

hist((1/(Ref_DE_Aschau$AvPRF))*100, xlab = "mean ICI in ms", main = "Aschau", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DE_BoEck$AvPRF))*100, xlab = "mean ICI in ms", main = "BoEck", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DE_Langholz$AvPRF))*100, xlab = "mean ICI in ms", main = "Langholz", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

##DENMARK

#number of ICIS under 67s (1.5ms): 62731 --> ~84,3%
ICI_6621 <- Ref_DK_6621[which (Ref_DK_6621$AvPRF <= 67), ]
hist(Ref_DK_6621$AvPRF, xlab = "reciprocal of mean ICI in s", breaks = 100)
hist(Ref_DK_6621$AvPRF, xlab = "reciprocal of mean ICI in s", xlim = c(0,200), breaks = 500)
axis(1, at = seq(0, 50, by = 5), labels = seq(0, 50, by = 5))
hist(1/(Ref_DK_6621$AvPRF), xlab = "mean ICI in s", xlim = c(0,0.2), breaks = 500)
axis(1, at = seq(0.0, 0.05, by = 0.01), labels = seq(0.0, 0.05 , by = 0.01))
hist((1/(Ref_DK_6621$AvPRF))*100, xlab = "mean ICI in ms", xlim = c(0,20), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

#number of ICIS under 67s (1.5ms): 26197 --> ~83,3%
ICI_6623 <- Ref_DK_6623[which (Ref_DK_6623$AvPRF <= 67), ]
hist(Ref_DK_6623$AvPRF, xlab = "reciprocal of mean ICI in s", breaks = 100)
hist(Ref_DK_6623$AvPRF, xlab = "reciprocal of mean ICI in s", xlim = c(0,200), breaks = 500)
axis(1, at = seq(0, 50, by = 5), labels = seq(0, 50, by = 5))
hist(1/(Ref_DK_6623$AvPRF), xlab = "mean ICI in s", xlim = c(0,0.2), breaks = 500)
axis(1, at = seq(0.0, 0.05, by = 0.01), labels = seq(0.0, 0.05, by = 0.01))
hist((1/(Ref_DK_6623$AvPRF))*100, xlab = "mean ICI in ms", xlim = c(0,20), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

#number of ICIS under 67s (1.5ms): 36832 --> ~81,3%
ICI_6625 <- Ref_DK_6625[which (Ref_DK_6625$AvPRF <= 67), ]
hist(Ref_DK_6625$AvPRF, xlab = "reciprocal of mean ICI in s", breaks = 100)
hist(Ref_DK_6625$AvPRF, xlab = "reciprocal of mean ICI in s", xlim = c(0,200), breaks = 500)
axis(1, at = seq(0, 50, by = 5), labels = seq(0, 50, by = 5))
hist(1/(Ref_DK_6625$AvPRF), xlab = "mean ICI in s", xlim = c(0,0.2), breaks = 500)
axis(1, at = seq(0.0, 0.05, by = 0.01), labels = seq(0.0, 0.05, by = 0.01))
hist((1/(Ref_DK_6625$AvPRF))*100, xlab = "mean ICI in ms", xlim = c(0,20), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

par(mfrow = c(1, 3))

hist((1/(Ref_DK_6621$AvPRF))*100, xlab = "mean ICI in ms", main = "DK1", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DK_6623$AvPRF))*100, xlab = "mean ICI in ms", main = "DK2", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DK_6625$AvPRF))*100, xlab = "mean ICI in ms", main = "DK3", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))


par(mfrow = c(2, 3))

hist((1/(Ref_DE_Aschau$AvPRF))*100, xlab = "mean ICI in ms", main = "Aschau", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DE_BoEck$AvPRF))*100, xlab = "mean ICI in ms", main = "BoEck", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DE_Langholz$AvPRF))*100, xlab = "mean ICI in ms", main = "Langholz", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DK_6621$AvPRF))*100, xlab = "mean ICI in ms", main = "DK1", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DK_6623$AvPRF))*100, xlab = "mean ICI in ms", main = "DK2", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

hist((1/(Ref_DK_6625$AvPRF))*100, xlab = "mean ICI in ms", main = "DK3", xlim = c(0,20), ylim = c(0,4000), breaks = 500)
axis(1, at = seq(0,5, by = 0.5), labels = seq(0,5, by = 0.5))

