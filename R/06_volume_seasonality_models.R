library(lmtest)
library(sandwich)
library(car)

dane_wig20 <- read.csv("mwig40_d.csv")
kol_data <- if("Data" %in% colnames(dane_wig20)) dane_wig20$Data else dane_wig20$Date
kol_vol <- if("Wolumen" %in% colnames(dane_wig20)) dane_wig20$Wolumen else dane_wig20$Volume

df_vol <- data.frame(Data = as.Date(kol_data), Wolumen = as.numeric(kol_vol))
df_vol <- na.omit(df_vol[df_vol$Data >= as.Date("2022-03-01") & df_vol$Data <= as.Date("2026-02-28"), ])
df_vol <- df_vol[order(df_vol$Data), ]

sredni_vol <- mean(df_vol$Wolumen)
df_vol$DV <- ((df_vol$Wolumen - sredni_vol) / sredni_vol)

df_vol$Dzien <- format(df_vol$Data, "%u")
df_vol$D_pon <- ifelse(df_vol$Dzien == "1", 1, 0)
df_vol$D_wt  <- ifelse(df_vol$Dzien == "2", 1, 0)
df_vol$D_sr  <- ifelse(df_vol$Dzien == "3", 1, 0)
df_vol$D_czw <- ifelse(df_vol$Dzien == "4", 1, 0)
df_vol$D_pt  <- ifelse(df_vol$Dzien == "5", 1, 0)

miesiace <- as.numeric(format(df_vol$Data, "%m"))
dni_miesiaca <- as.numeric(format(df_vol$Data, "%d"))
df_vol$D_wiedzmy <- ifelse(miesiace %in% c(3, 6, 9, 12) & df_vol$Dzien == "5" & dni_miesiaca >= 15 & dni_miesiaca <= 21, 1, 0)

model_vol <- lm(DV ~ D_pon + D_wt + D_sr + D_czw + D_pt + D_wiedzmy - 1, data = df_vol)

test_bp <- bptest(model_vol)
test_bg <- bgtest(model_vol, order = 5)

if(test_bp$p.value < 0.05 | test_bg$p.value < 0.05) {
  wynik_koncowy <- coeftest(model_vol, vcov = NeweyWest(model_vol, prewhite = FALSE))
  test_walda <- linearHypothesis(model_vol, c("D_pon = D_wt", "D_wt = D_sr", "D_sr = D_czw", "D_czw = D_pt"), vcov = NeweyWest(model_vol, prewhite = FALSE))
} else {
  wynik_koncowy <- summary(model_vol)$coefficients
  test_walda <- linearHypothesis(model_vol, c("D_pon = D_wt", "D_wt = D_sr", "D_sr = D_czw", "D_czw = D_pt"))
}

print(round(wynik_koncowy, 5))
print(test_walda)