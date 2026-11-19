library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)

pliki_csv <- c("WIG20" = "wig20_d-2.csv", "MWIG40" = "mwig40_d.csv", "SWIG80" = "swig80_d.csv")
data_poczatkowa <- "2022-03-01"
data_koncowa <- "2026-02-28"
dzienny_wolumen_indeksow <- list()

for(nazwa in names(pliki_csv)) {
  dane_csv <- read.csv(pliki_csv[[nazwa]])
  kol_wolumen <- if("Wolumen" %in% colnames(dane_csv)) dane_csv$Wolumen else dane_csv$Volume
  kol_data <- if("Data" %in% colnames(dane_csv)) dane_csv$Data else dane_csv$Date
  
  wolumen_xts <- xts(as.numeric(as.character(kol_wolumen)) / 1000000, order.by = as.Date(kol_data))
  wolumen_xts <- na.omit(wolumen_xts[paste0(data_poczatkowa, "/", data_koncowa)])
  colnames(wolumen_xts) <- nazwa
  
  dzienny_wolumen_indeksow[[nazwa]] <- wolumen_xts
}

wszystkie_wolumeny <- do.call(merge, dzienny_wolumen_indeksow)
df_long_vol <- pivot_longer(data.frame(Data = index(wszystkie_wolumeny), coredata(wszystkie_wolumeny)), 
                            cols = -Data, names_to = "Indeks", values_to = "Wolumen")

df_long_vol$NumerDnia <- format(df_long_vol$Data, "%u")
df_long_vol <- df_long_vol %>% filter(NumerDnia %in% 1:5)
df_long_vol$Dzien <- factor(df_long_vol$NumerDnia, levels = 1:5,
                            labels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek"))

wykres_pudelkowy_vol <- ggplot(df_long_vol, aes(x = Dzien, y = Wolumen, fill = Dzien)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.2, alpha = 0.7) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  facet_wrap(~ Indeks, scales = "free_y") +
  labs(title = "Rozkład dziennego wolumenu obrotu w podziale na dni tygodnia",
       x = "Dzień tygodnia", y = "Wolumen (w milionach sztuk)") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(wykres_pudelkowy_vol)

df_srednia_vol <- df_long_vol %>%
  group_by(Indeks, Dzien) %>%
  summarise(Sredni_Wolumen = mean(Wolumen, na.rm = TRUE), .groups = 'drop')

wykres_slupkowy_vol <- ggplot(df_srednia_vol, aes(x = Dzien, y = Sredni_Wolumen, fill = Dzien)) +
  geom_col(alpha = 0.85, color = "black", linewidth = 0.3) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  facet_wrap(~ Indeks, scales = "free_y") +
  labs(title = "Średni wolumen obrotu na sesję według dnia tygodnia",
       x = "Dzień tygodnia", y = "Średni wolumen (w milionach sztuk)") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

print(wykres_slupkowy_vol)

df_srednia_globalna <- df_long_vol %>%
  group_by(Indeks) %>%
  summarise(Srednia_Indeksu = mean(Wolumen, na.rm = TRUE))

df_odchylenie_vol <- df_long_vol %>%
  group_by(Indeks, Dzien) %>%
  summarise(Sredni_Wol_Dnia = mean(Wolumen, na.rm = TRUE), .groups = 'drop') %>%
  left_join(df_srednia_globalna, by = "Indeks") %>%
  mutate(Odchylenie_Pct = (Sredni_Wol_Dnia - Srednia_Indeksu) / Srednia_Indeksu * 100)

wykres_odchylenie_vol <- ggplot(df_odchylenie_vol, aes(x = Dzien, y = Odchylenie_Pct, fill = Odchylenie_Pct > 0)) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(Odchylenie_Pct, 1), "%")), 
            vjust = ifelse(df_odchylenie_vol$Odchylenie_Pct > 0, -0.5, 1.5), size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
  theme_minimal() +
  facet_wrap(~ Indeks) +
  expand_limits(y = c(min(df_odchylenie_vol$Odchylenie_Pct) - 10, max(df_odchylenie_vol$Odchylenie_Pct) + 10)) +
  labs(title = "Odchylenie wolumenu od średniej tygodniowej",
       x = "Dzień tygodnia", y = "Odchylenie od średniej (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.minor = element_blank())

print(wykres_odchylenie_vol)
ggsave("wykres_vol1.png", plot = wykres_odchylenie_vol, width = 25, height = 15, units = "cm", dpi = 300)