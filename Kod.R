library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(kableExtra)
library(corrplot)
library(writexl)
library(ggplot2)

dane <- read_excel("dane_gus.xlsx")
powiaty <- dane[[1]]
dane <- dane[,-1]
ostatnia_kolumna <- dane$Nazwy
dane <-dane[,-ncol(dane)]
  

cv <- apply(dane, 2, function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))) 
tabela_statystyk <- data.frame(
  Średnia = round(apply(dane, 2, function(x) mean(x, na.rm = TRUE)), 3),
  Odchylenie_standardowe = round(apply(dane, 2, function(x) sd(x, na.rm = TRUE)), 3),
  Minimum = round(apply(dane, 2, function(x) min(x, na.rm = TRUE)), 3),
  Maksimum = round(apply(dane, 2, function(x) max(x, na.rm = TRUE)), 3),
  Skośność = round(apply(dane, 2, function(x) skew(x, na.rm = TRUE)), 3),
  Kurtoza = round(apply(dane, 2, function(x) kurtosi(x, na.rm = TRUE)), 3),
  Wsp_zmiennosci = round(cv, 3)
)


macierz_korelacji <- cor(dane)
corrplot(macierz_korelacji, method = 'number', number.cex = 1.2)

variables <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12")


par(mfrow = c(4, 3))  

for (variable in variables) {
  boxplot(dane[[variable]], main = paste("Wykres pudełkowy", variable),
          xlab = variable, ylab = "Values")
}


dane1 <- read_excel("dane.xlsx")
dane1 <- dane1[,-1]
dane_zestandaryzowane <- scale(dane1)

for (i in 1:ncol(dane_zestandaryzowane)) {
  Q1 <- quantile(dane_zestandaryzowane[, i], 0.25)
  Q3 <- quantile(dane_zestandaryzowane[, i], 0.75)
  IQR <- Q3 - Q1
  
  dolny_was <- Q1 - 1.5 * IQR
  gorny_was <- Q3 + 1.5 * IQR
  
  dane_zestandaryzowane[dane_zestandaryzowane[, i] < dolny_was, i] <- dolny_was
  dane_zestandaryzowane[dane_zestandaryzowane[, i] > gorny_was, i] <- gorny_was
}

# 1. Metoda Standaryzowanych Sum
suma_zestandaryzowana <- rowSums(dane_zestandaryzowane)

suma_min <- min(suma_zestandaryzowana)
suma_max <- max(suma_zestandaryzowana)
znormalizowana_suma <- (suma_zestandaryzowana - suma_min) / (suma_max - suma_min)

wynik <- data.frame(Powiaty = powiaty, Metoda_Standaryzowanych_Sum = znormalizowana_suma)

# 2. Metoda Sumy Rang bez wag (rangi malejąco)

rangi <- apply(dane_zestandaryzowane, 2, function(x) rank(x, ties.method = "average"))
suma_rang <- rowSums(rangi)
wynik$Metoda_Sumy_Rang <- suma_rang

# 3. Metoda Hellwiga

wzorzec <- apply(dane_zestandaryzowane, 2, max)
odleglosci_od_wzorca <- apply(dane_zestandaryzowane, 1, function(x) sqrt(sum((x - wzorzec)^2)))
odleglosc_maksymalna <- max(odleglosci_od_wzorca)
wynik_hellwig <- 1 - (odleglosci_od_wzorca / odleglosc_maksymalna)
wynik$Metoda_Hellwiga <- wynik_hellwig



rankingi <- data.frame(
  Powiaty = powiaty,  
  Ranga_Hellwig = rank(-wynik_hellwig),  
  Ranga_SumaRang = rank(-suma_rang), 
  Ranga_StandaryzowaneSumy = rank(-znormalizowana_suma)

)

cor(rankingi[,-1], method = "kendall")

# Podział na grupy


srednie <- colMeans(wynik[,-1], na.rm = TRUE)
odchylenia <- sapply(wynik[,-1], sd)


przypisz_grupe <- function(ranga, srednia, odchylenie) {
  if (ranga >= (srednia + odchylenie)) {
    return("Grupa I (najwyższy poziom)")
  } else if (ranga >= srednia && ranga < (srednia + odchylenie)) {
    return("Grupa II (poziom ponadprzeciętny)")
  } else if (ranga >= (srednia - odchylenie) && ranga < srednia) {
    return("Grupa III (poziom poniżej przeciętnej)")
  } else {
    return("Grupa IV (najniższy poziom)")
  }
}


for (metoda in colnames(wynik)[-1]) {
  wynik[[paste0("Grupa_", metoda)]] <- sapply(wynik[[metoda]], przypisz_grupe, srednie[metoda], odchylenia[metoda])
}


write_xlsx(wynik, "wyniki_rankingow.xlsx")
write_xlsx(rankingi, "RANKINGI.xlsx")



#  Grupowanie 

pca <- prcomp(dane_zestandaryzowane, center = TRUE, scale. = TRUE)

pca_data <- as.data.frame(pca$x[, 1:2])
colnames(pca_data) <- c("PC1", "PC2")
k_klaster_kmeans <- kmeans(dane_zestandaryzowane, centers = 3)

pca_data$klaster_kmeans <- as.factor(k_klaster_kmeans$cluster)


pca_data$powiat <- ostatnia_kolumna 


ggplot(pca_data, aes(x = PC1, y = PC2, color = klaster_kmeans)) +
  geom_point(size = 5) + 
  geom_text(aes(label = powiat), vjust = -1, size = 4) +  
  labs(title = "Klasteryzacja k-średnich",
       x = "Pierwsza Główna Składowa (PC1)", 
       y = "Druga Główna Składowa (PC2)", 
       color = "Klaster") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


dane_kmeans <- as.data.frame(dane)
dane_kmeans$Cluster_KMeans <- k_klaster_kmeans$cluster

kmeans_stats <- dane_kmeans %>%
  group_by(Cluster_KMeans) %>%
  summarise_all(list(mean = ~mean(.), sd = ~sd(.)))

print(kmeans_stats)

dev.off()

dystans <- dist(dane_zestandaryzowane, method = "euclidean")


hc <- hclust(dystans, method = "ward.D2")

plot(hc, labels = powiaty, main = "Dendrogram dla metody Warda", 
     xlab = "Powiaty", ylab = "Odległość", sub = "", cex = 1)

rect.hclust(hc, k = 3, border = "#B691D2")

dane_hierarchical <- as.data.frame(dane)
dane_hierarchical$Cluster_Hierarchical <- cutree(hc, k = 3)

hierarchical_stats <- dane_hierarchical %>%
  group_by(Cluster_Hierarchical) %>%
  summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.)), .names = "{.col}_{.fn}"))

print(hierarchical_stats)

write_xlsx(hierarchical_stats, "statystyki_hierarchiczne.xlsx")
