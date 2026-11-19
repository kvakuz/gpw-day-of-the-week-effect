library(dplyr)
library(ggplot2)
library(tidyr)

pliki_5m <- c("WIG20" = "wig20dane.txt", "MWIG40" = "mwig40dane.txt", "SWIG80" = "swig80dane.txt")

for(indeks in names(pliki_5m)) {
  dane <- read.csv(pliki_5m[[indeks]])
  colnames(dane) <- c("TICKER", "PER", "DATE", "TIME", "OPEN", "HIGH", "LOW", "CLOSE", "VOL", "OPENINT")
  
  dane <- dane %>%
    group_by(DATE) %>%
    arrange(TIME) %>%
    mutate(Return = log(CLOSE) - log(lag(CLOSE))) %>%
    ungroup() %>%
    filter(!is.na(Return))
  
  dane$Datetime <- as.POSIXct(paste(dane$DATE, sprintf("%06d", as.numeric(dane$TIME))), format="%Y%m%d %H%M%S")
  dane <- dane[!is.na(dane$Datetime), ]
  
  df <- data.frame(Datetime = dane$Datetime, Return = dane$Return, Date = as.Date(dane$Datetime), 
                   Time = format(dane$Datetime, "%H:%M:%S"), DayOfWeek = format(as.Date(dane$Datetime), "%u")) %>% 
    filter(DayOfWeek %in% 1:5)
  
  unikalne_czasy <- sort(unique(df$Time))
  df$i <- match(df$Time, unikalne_czasy)
  df$N <- length(unikalne_czasy)
  
  df <- df %>% group_by(Time) %>% mutate(R_bar_i = mean(Return, na.rm = TRUE)) %>% ungroup() %>%
    group_by(Date) %>% mutate(sigma_t = sqrt(sum(Return^2, na.rm = TRUE))) %>% ungroup()
  
  df$y <- 2 * log((abs(df$Return - df$R_bar_i) / (df$sigma_t / sqrt(df$N))) + 1e-8)
  df <- df %>% mutate(N1 = (N + 1) / 2, N2 = (N + 1) * (N + 2) / 6, trend1 = i / N1, trend2 = (i^2) / N2)
  
  df$D_Pon <- ifelse(df$DayOfWeek == "1", 1, 0)
  df$D_Wto <- ifelse(df$DayOfWeek == "2", 1, 0)
  df$D_Sro <- ifelse(df$DayOfWeek == "3", 1, 0)
  df$D_Czw <- ifelse(df$DayOfWeek == "4", 1, 0)
  
  for(k in 1:4) {
    df[[paste0("cos", k)]] <- cos(2 * pi * k * df$i / df$N)
    df[[paste0("sin", k)]] <- sin(2 * pi * k * df$i / df$N)
  }
  
  f_base <- "y ~ trend1 + trend2 + D_Pon + D_Wto + D_Sro + D_Czw"
  modele <- list(lm(as.formula(paste(f_base, "+ cos1 + sin1")), data = df),
                 lm(as.formula(paste(f_base, "+ cos1 + sin1 + cos2 + sin2")), data = df),
                 lm(as.formula(paste(f_base, "+ cos1 + sin1 + cos2 + sin2 + cos3 + sin3")), data = df),
                 lm(as.formula(paste(f_base, "+ cos1 + sin1 + cos2 + sin2 + cos3 + sin3 + cos4 + sin4")), data = df))
  
  model_optymalny <- modele[[which.min(c(AIC(modele[[1]]), AIC(modele[[2]]), AIC(modele[[3]]), AIC(modele[[4]])))]]
  print(paste("Wartości i parametry modelu FFF dla indeksu:", indeks))
  print(summary(model_optymalny))
  
  df$f_ti <- predict(model_optymalny)
  df$s_ti_norm <- exp(df$f_ti) / mean(exp(df$f_ti), na.rm = TRUE)
  df$Filtered_Return <- df$Return / sqrt(df$s_ti_norm)
  
  profil <- df %>% group_by(Time) %>% summarise(Surowe = mean(abs(Return), na.rm = TRUE), Odsezonowane = mean(abs(Filtered_Return), na.rm = TRUE)) %>%
    pivot_longer(cols = c(Surowe, Odsezonowane), names_to = "Typ", values_to = "Sredni_Modul_Zwrotu")
  
  p <- ggplot(profil, aes(x = Time, y = Sredni_Modul_Zwrotu, color = Typ, group = Typ)) + 
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Surowe" = "#e74c3c", "Odsezonowane" = "#2980b9")) +
    scale_x_discrete(breaks = unique(profil$Time)[seq(1, length(unique(profil$Time)), by = 12)]) +
    theme_minimal() + 
    labs(title = paste("Profil wewnątrzsesyjnej zmienności -", indeks), x = "Czas trwania sesji", y = "Przekrojowa średnia absolutnych stóp zwrotu") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.title = element_blank())
  
  ggsave(paste0("Wykres_FFF_", indeks, ".png"), plot = p, width = 9, height = 5, bg = "white")
  
  sezonowosc_dni <- df %>% group_by(Time, DayOfWeek) %>% summarise(Srednia_Sezonowosc = mean(s_ti_norm, na.rm = TRUE), .groups = "drop") %>%
    mutate(Dzien_Nazwa = factor(DayOfWeek, levels = 1:5, labels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek")))
  
  p_sezonowosc <- ggplot(sezonowosc_dni, aes(x = Time, y = Srednia_Sezonowosc, color = Dzien_Nazwa, group = Dzien_Nazwa)) +
    geom_line(linewidth = 0.8) + geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
    scale_color_manual(values = c("poniedziałek" = "#e74c3c", "wtorek" = "#2980b9", "środa" = "#27ae60", "czwartek" = "#95a5a6", "piątek" = "#8e44ad")) +
    scale_x_discrete(breaks = unique(sezonowosc_dni$Time)[seq(1, length(unique(sezonowosc_dni$Time)), by = 12)]) +
    theme_minimal() + 
    labs(title = paste("Przeciętne wartości składowej sezonowej -", indeks), x = "Czas trwania sesji", y = "Średnia składowa sezonowa") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right", legend.title = element_blank(), panel.grid.minor = element_blank())
  
  print(p_sezonowosc)
  ggsave(paste0("Wykres_Sezonowosc_Dni_INTRADAY_", indeks, ".png"), plot = p_sezonowosc, width = 9, height = 5, bg = "white")
}