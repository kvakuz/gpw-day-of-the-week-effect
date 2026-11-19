library(quantmod)
library(PerformanceAnalytics)
library(tidyr)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)
library(rugarch)
library(FinTS)

pliki_csv <- c("WIG20" = "wig20_d-2.csv", "MWIG40" = "mwig40_d.csv", "SWIG80" = "swig80_d.csv")
data_poczatkowa <- "2022-03-01"
data_koncowa <- "2026-02-28"
dzienne_stopy_indeksow <- list()

for(nazwa in names(pliki_csv)) {
  dane_csv <- read.csv(pliki_csv[[nazwa]])
  kol_zamkniecie <- if("Zamkniecie" %in% colnames(dane_csv)) dane_csv$Zamkniecie else dane_csv$Close
  kol_data <- if("Data" %in% colnames(dane_csv)) dane_csv$Data else dane_csv$Date
  
  ceny_zamkniecia <- xts(kol_zamkniecie, order.by = as.Date(kol_data))
  ceny_zamkniecia <- ceny_zamkniecia[paste0(data_poczatkowa, "/", data_koncowa)]
  
  stopy_log <- na.omit(CalculateReturns(ceny_zamkniecia, method = "log"))
  colnames(stopy_log) <- paste0(nazwa, "_Zwrot")
  dzienne_stopy_indeksow[[nazwa]] <- stopy_log
}

wszystkie_indeksy <- do.call(merge, dzienne_stopy_indeksow)

for(indeks in names(pliki_csv)) {
  model_mean <- lm(wszystkie_indeksy[, paste0(indeks, "_Zwrot")] ~ 1)
  arch_test <- ArchTest(residuals(model_mean), lags = 5)
  print(paste("Test ARCH-LM dla", indeks))
  print(arch_test)
}

wyniki_garch <- list()
daty <- index(wszystkie_indeksy)
dni_tygodnia <- format(daty, "%u")

regresory_wariancja <- cbind(
  D_wt  = ifelse(dni_tygodnia == "2", 1, 0),
  D_sr  = ifelse(dni_tygodnia == "3", 1, 0),
  D_czw = ifelse(dni_tygodnia == "4", 1, 0),
  D_pt  = ifelse(dni_tygodnia == "5", 1, 0)
)

for(indeks in names(pliki_csv)) {
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = regresory_wariancja),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std" 
  )
  
  fit <- ugarchfit(spec = spec, data = wszystkie_indeksy[, paste0(indeks, "_Zwrot")], solver = "hybrid")
  wyniki_garch[[indeks]] <- fit@fit$robust.matcoef
  
  reszty_std <- residuals(fit, standardize = TRUE)
  arch_test_res <- ArchTest(as.numeric(reszty_std), lags = 5)
  
  print(paste("Test ARCH-LM (reszty) dla", indeks))
  print(arch_test_res)
}

for(nazwa in names(wyniki_garch)) {
  parametry <- wyniki_garch[[nazwa]]
  szukane <- c("omega", "alpha1", "beta1", "vxreg1", "vxreg2", "vxreg3", "vxreg4", "shape")
  dostepne <- intersect(szukane, rownames(parametry))
  
  parametry_wariancji <- parametry[dostepne, ]
  rownames(parametry_wariancji) <- gsub("vxreg1", "D_wtorek", rownames(parametry_wariancji))
  rownames(parametry_wariancji) <- gsub("vxreg2", "D_sroda", rownames(parametry_wariancji))
  rownames(parametry_wariancji) <- gsub("vxreg3", "D_czwartek", rownames(parametry_wariancji))
  rownames(parametry_wariancji) <- gsub("vxreg4", "D_piatek", rownames(parametry_wariancji))
  rownames(parametry_wariancji) <- gsub("shape", "Stopnie_swobody_(std)", rownames(parametry_wariancji))
  
  print(paste("Parametry modelu GARCH dla", nazwa))
  print(round(parametry_wariancji, 5))
}