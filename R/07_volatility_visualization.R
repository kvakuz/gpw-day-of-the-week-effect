library(xts)
library(ggplot2)
library(dplyr)
library(tidyr)

pliki_csv <- c("WIG20" = "wig20_d-2.csv", "MWIG40" = "mwig40_d.csv", "SWIG80" = "swig80_d.csv")
data_poczatkowa <- "2022-03-01"
data_koncowa <- "2026-02-28"
dzienna_zmiennosc_indeksow <- list()

for(nazwa in names(pliki_csv)) {
  dane_csv <- read.csv(pliki_csv[[nazwa]])
  kol_high <- if("Najwyzszy" %in% colnames(dane_csv)) dane_csv$Najwyzszy else dane_csv$High
  kol_low <- if("Najnizszy" %in% colnames(dane_csv)) dane_csv$Najnizszy else dane_csv$Low
  kol_data <- if("Data" %in% colnames(dane_csv)) dane_csv$Data else dane_csv$Date
  
  amplituda <- (kol_high - kol_low) / kol_low * 100
  poprawne_indeksy <- !is.na(kol_data) & kol_data != "" & !is.na(amplituda)
  
  amplituda_xts <- na.omit(xts(amplituda[poprawne_indeksy], order.by = as.Date(kol_data[poprawne_indeksy]))[paste0(data_poczatkowa, "/", data_koncowa)])
  colnames(amplituda_xts) <- nazwa
  dzienna_zmiennosc_indeksow[[nazwa]] <- amplituda_xts
}

df_long_zmiennosc <- pivot_longer(data.frame(Data = index(do.call(merge, dzienna_zmiennosc_indeksow)), 
                                             coredata(do.call(merge, dzienna_zmiennosc_indeksow))), 
                                  cols = -Data, names_to = "Indeks", values_to = "Amplituda")

df_long_zmiennosc$NumerDnia <- format(df_long_zmiennosc$Data, "%u")
df_long_zmiennosc <- df_long_zmiennosc %>% 
  filter(NumerDnia %in% 1:5) %>% 
  mutate(Dzien = factor(NumerDnia, levels = 1:5, labels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek")))

wykres_zmiennosc <- ggplot(df_long_zmiennosc, aes(x = Dzien, y = Amplituda, fill = Dzien)) +
  geom_boxplot(outlier.color = "darkred", outlier.size = 1.2, alpha = 0.8) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  facet_wrap(~ Indeks) +
  labs(title = "Amplituda wahań cen według dnia tygodnia", x = "Dzień tygodnia", y = "Amplituda wahań (%)") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(wykres_zmiennosc)
ggsave("wykres_zmiennosc.png", plot = wykres_zmiennosc, width = 25, height = 15, units = "cm", dpi = 300)