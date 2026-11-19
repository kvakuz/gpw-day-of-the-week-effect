library(quantmod)
library(PerformanceAnalytics)
library(tidyr)
library(dplyr)
library(lmtest)
library(car)

pliki_csv <- c("WIG20" = "wig20_d-2.csv", "MWIG40" = "mwig40_d.csv", "SWIG80" = "swig80_d.csv")
data_poczatkowa <- "2022-03-01"
data_koncowa <- "2026-02-28"
dzienne_stopy_indeksow <- list()

for(nazwa in names(pliki_csv)) {
  dane_csv <- read.csv(pliki_csv[[nazwa]])
  kol_zamkniecie <- if("Zamkniecie" %in% colnames(dane_csv)) dane_csv$Zamkniecie else dane_csv$Close
  kol_data <- if("Data" %in% colnames(dane_csv)) dane_csv$Data else dane_csv$Date
  
  ceny_zamkniecia <- xts(kol_zamkniecie, order.by = as.Date(kol_data))
  stopy_log <- na.omit(CalculateReturns(ceny_zamkniecia[paste0(data_poczatkowa, "/", data_koncowa)], method = "log"))
  colnames(stopy_log) <- paste0(nazwa, "_Zwrot")
  dzienne_stopy_indeksow[[nazwa]] <- stopy_log
}

wszystkie_indeksy <- do.call(merge, dzienne_stopy_indeksow)

df_long_ind <- pivot_longer(data.frame(Data = index(wszystkie_indeksy), coredata(wszystkie_indeksy)), 
                            cols = -Data, names_to = "Indeks", values_to = "Zwrot")
df_long_ind$Indeks <- gsub("_Zwrot", "", df_long_ind$Indeks)
df_long_ind$NumerDnia <- format(df_long_ind$Data, "%u")
df_long_ind <- df_long_ind %>% filter(NumerDnia %in% 1:5)
df_long_ind$Dzien <- factor(df_long_ind$NumerDnia, levels = 1:5, labels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek"))

tabela_kmnk <- data.frame()

for(wybrany_indeks in unique(df_long_ind$Indeks)) {
  model <- lm(Zwrot ~ Dzien - 1, data = subset(df_long_ind, Indeks == wybrany_indeks))
  model_summary <- summary(model)$coefficients
  
  hipotezy <- c("Dzienponiedziałek = Dzienwtorek", "Dzienwtorek = Dzienśroda", "Dzienśroda = Dzienczwartek", "Dzienczwartek = Dzienpiątek")
  test_walda <- linearHypothesis(model, hipotezy)
  
  for(i in 1:5) {
    p_value <- model_summary[i, "Pr(>|t|)"]
    tabela_kmnk <- rbind(tabela_kmnk, data.frame(
      Indeks = wybrany_indeks,
      Dzien = gsub("Dzien", "", rownames(model_summary)[i]),
      Wspolczynnik_Beta = round(model_summary[i, "Estimate"], 6),
      P_value_Beta = round(p_value, 4),
      Istotnosc = ifelse(p_value < 0.01, "***", ifelse(p_value < 0.05, "**", ifelse(p_value < 0.1, "*", ""))),
      Wald_P_value = ifelse(i == 1, round(test_walda$`Pr(>F)`[2], 4), NA)
    ))
  }
}

print(tabela_kmnk)