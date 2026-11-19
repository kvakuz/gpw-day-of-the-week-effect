library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(moments)
library(ggplot2)
library(dplyr)
library(tidyr)

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

tabela_wynikow_eda <- data.frame(
  Indeks = character(), Srednia = numeric(), Odchylenie_Std = numeric(),
  Minimum = numeric(), Maksimum = numeric(), Skosnosc = numeric(),
  Kurtoza = numeric(), JB_p_value = numeric(), ADF_p_value = numeric(),
  Ljung_Box_p_value = numeric(), stringsAsFactors = FALSE
)

for(indeks in names(pliki_csv)) {
  dane <- as.numeric(dzienne_stopy_indeksow[[indeks]])
  test_jb <- jarque.bera.test(dane)
  test_adf <- adf.test(dane, alternative = "stationary")
  test_lb <- Box.test(dane, lag = 5, type = "Ljung-Box")
  
  tabela_wynikow_eda <- rbind(tabela_wynikow_eda, data.frame(
    Indeks = indeks,
    Srednia = round(mean(dane), 6), Odchylenie_Std = round(sd(dane), 4),
    Minimum = round(min(dane), 4), Maksimum = round(max(dane), 4),
    Skosnosc = round(skewness(dane), 4), Kurtoza = round(kurtosis(dane), 4),
    JB_p_value = round(test_jb$p.value, 4), ADF_p_value = round(test_adf$p.value, 4),
    Ljung_Box_p_value = round(test_lb$p.value, 4)
  ))
}

print(tabela_wynikow_eda)

df_long_ind <- pivot_longer(data.frame(Data = index(wszystkie_indeksy), coredata(wszystkie_indeksy)), 
                            cols = -Data, names_to = "Indeks", values_to = "Zwrot")
df_long_ind$Indeks <- gsub("_Zwrot", "", df_long_ind$Indeks)
df_long_ind$NumerDnia <- format(df_long_ind$Data, "%u")
df_long_ind <- df_long_ind %>% filter(NumerDnia %in% 1:5)
df_long_ind$Dzien <- factor(df_long_ind$NumerDnia, levels = 1:5, 
                            labels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek"))

wykres_pudelkowy_ind <- ggplot(df_long_ind, aes(x = Dzien, y = Zwrot, fill = Dzien)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.2, alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  facet_wrap(~ Indeks) +
  labs(title = "Rozkład logarytmicznych stóp zwrotu", x = "Dzień tygodnia", y = "Log-stopa zwrotu") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(wykres_pudelkowy_ind)

df_kumulatywny_ind <- df_long_ind %>%
  group_by(Indeks, Dzien) %>% arrange(Data) %>%
  mutate(Skumulowany_Zwrot = cumsum(Zwrot)) %>% ungroup()

wykres_liniowy_ind <- ggplot(df_kumulatywny_ind, aes(x = Data, y = Skumulowany_Zwrot, color = Dzien)) +
  geom_line(linewidth = 0.8) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  facet_wrap(~ Indeks, scales = "free_y") + 
  labs(title = "Skumulowane stopy zwrotu indeksów wg dnia tygodnia",
       x = "Data", y = "Skumulowana log-stopa zwrotu", color = "Dzień tygodnia") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 20))

print(wykres_liniowy_ind)
ggsave("wykres_pudelkowy_ind.png", plot = wykres_pudelkowy_ind, width = 25, height = 15, units = "cm", dpi = 300)