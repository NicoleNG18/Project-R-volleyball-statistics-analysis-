# ==============================================================================
# 1. ЗАРЕЖДАНЕ И ПОДГОТОВКА НА ДАННИТЕ
# ==============================================================================

extract_team_stats <- function(data, team_prefix, opponent_prefix, is_team_1 = TRUE) {
  raw_rec   <- data[[paste0(team_prefix, "_Rec_Perf")]]
  clean_rec <- as.numeric(gsub("%", "", raw_rec))
  opp_aces  <- as.numeric(data[[paste0(opponent_prefix, "_Srv_Ace")]])
  venue_value         <- ifelse(is_team_1, "Home", "Away")
  target_winner_value <- ifelse(is_team_1, 0, 1)
  is_winner <- ifelse(data$Winner == target_winner_value, "Yes", "No")
  return(data.frame(
    Rec_Perf      = clean_rec,
    Opponent_Aces = opp_aces,
    Match_Venue   = as.factor(venue_value),
    Is_Winner     = as.factor(is_winner),
    stringsAsFactors = FALSE
  ))
}

df_raw     <- read.csv("matches.csv")
t1_dataset <- extract_team_stats(df_raw, team_prefix = "T1", opponent_prefix = "T2", is_team_1 = TRUE)
t2_dataset <- extract_team_stats(df_raw, team_prefix = "T2", opponent_prefix = "T1", is_team_1 = FALSE)
df         <- rbind(t1_dataset, t2_dataset)
rownames(df) <- NULL

# Преглед на структурата и обобщените данни
print(str(df))
print(summary(df))


# ==============================================================================
# 2. ЕДНОМЕРЕН АНАЛИЗ — ЧИСЛОВИ ПРОМЕНЛИВИ
# ==============================================================================

# --- 2.1 Rec_Perf ---

par(mfrow = c(1, 2))

boxplot(df$Rec_Perf,
        main   = "Boxplot на Rec_Perf",
        ylab   = "Перфектно посрещане (%)",
        col    = "lightblue",
        border = "black")

hist(df$Rec_Perf,
     main   = "Хистограма на Rec_Perf",
     xlab   = "Перфектно посрещане (%)",
     ylab   = "Честота",
     col    = "lightgreen",
     breaks = 20)

par(mfrow = c(1, 1))

qqnorm(df$Rec_Perf, main = "Q-Q Plot на Rec_Perf", col = "darkblue")
qqline(df$Rec_Perf, col = "red", lwd = 2)

shapiro.test(sample(df$Rec_Perf, 5000))


# --- 2.2 Opponent_Aces ---

par(mfrow = c(1, 2))

boxplot(df$Opponent_Aces,
        main   = "Boxplot на Opponent_Aces",
        ylab   = "Брой сервиси на противника",
        col    = "lightblue",
        border = "black")

hist(df$Opponent_Aces,
     main   = "Хистограма на Opponent_Aces",
     xlab   = "Брой сервиси на противника",
     ylab   = "Честота",
     col    = "lightgreen",
     breaks = 20)

par(mfrow = c(1, 1))

qqnorm(df$Opponent_Aces, main = "Q-Q Plot на Opponent_Aces", col = "darkblue")
qqline(df$Opponent_Aces, col = "red", lwd = 2)

shapiro.test(sample(df$Opponent_Aces, 5000))


# ==============================================================================
# 3. ЕДНОМЕРЕН АНАЛИЗ — КАТЕГОРИЙНИ ПРОМЕНЛИВИ
# ==============================================================================

# --- 3.1 Is_Winner ---

table(df$Is_Winner)

barplot(table(df$Is_Winner),
        main = "Честотно разпределение на Is_Winner",
        xlab = "Изход от мача (Победител)",
        ylab = "Брой мачове",
        col  = c("tomato", "lightgreen"))


# --- 3.2 Match_Venue ---

table(df$Match_Venue)

barplot(table(df$Match_Venue),
        main = "Честотно разпределение на Match_Venue",
        xlab = "Място на провеждане (Домакинство)",
        ylab = "Брой мачове",
        col  = c("tomato", "lightgreen"))


# ==============================================================================
# 4. МНОГОМЕРЕН АНАЛИЗ
# ==============================================================================

# --- 4.1 Rec_Perf & Is_Winner ---

boxplot(Rec_Perf ~ Is_Winner, data = df,
        col  = c("tomato", "lightgreen"),
        main = "Посрещане спрямо Изхода от мача",
        xlab = "Победа (Is_Winner)",
        ylab = "Перфектно посрещане (%)")

wilcox.test(Rec_Perf ~ Is_Winner, data = df)
# p-value = 0.2053 → НЕ е значима разлика


# --- 4.2 Opponent_Aces & Is_Winner ---

boxplot(Opponent_Aces ~ Is_Winner, data = df,
        col  = c("tomato", "lightgreen"),
        main = "Асове на противника спрямо Изхода",
        xlab = "Победа (Is_Winner)",
        ylab = "Асове на противника (брой)")

wilcox.test(Opponent_Aces ~ Is_Winner, data = df)
# p-value < 2.2e-16 → Значима разлика (парадокс: повече асове при победа)


# --- 4.3 Is_Winner & Match_Venue ---

table_venue_winner <- table(df$Match_Venue, df$Is_Winner)
print(table_venue_winner)
print(prop.table(table_venue_winner, margin = 1) * 100)

chisq.test(table_venue_winner)
# p-value = 9.869e-10 → Домакинското предимство е значимо (54.2% победи у дома)

barplot(table_venue_winner,
        beside      = TRUE,
        legend.text = TRUE,
        args.legend = list(x = "topright"),
        main = "Влияние на домакинството върху изхода от мача",
        xlab = "Победител (Is_Winner)",
        ylab = "Брой мачове",
        col  = c("coral", "skyblue"))


# --- 4.4 Match_Venue & Rec_Perf ---

boxplot(Rec_Perf ~ Match_Venue, data = df,
        col  = c("gold", "lightblue"),
        main = "Посрещане: Домакин vs Гост",
        xlab = "Място (Match_Venue)",
        ylab = "Rec_Perf (%)")

wilcox.test(Rec_Perf ~ Match_Venue, data = df)
# p-value = 2.684e-05 → Домакините посрещат по-добре (медиана 19% vs 18%)


# --- 4.5 Opponent_Aces & Match_Venue ---

boxplot(Opponent_Aces ~ Match_Venue, data = df,
        col  = c("gold", "lightblue"),
        main = "Асове на противника: Домакин vs Гост",
        xlab = "Място (Match_Venue)",
        ylab = "Opponent_Aces (брой)")

wilcox.test(Opponent_Aces ~ Match_Venue, data = df)
# p-value = 0.099 → НЕ е значима разлика (натискът от сервиз е константен)


# --- 4.6 Opponent_Aces & Rec_Perf (корелация) ---

plot(df$Opponent_Aces, df$Rec_Perf,
     main = "Корелация: Асове на противника спрямо Посрещане",
     xlab = "Асове на противника (брой)",
     ylab = "Перфектно посрещане (%)",
     col  = "darkgreen",
     pch  = 16)
abline(lm(Rec_Perf ~ Opponent_Aces, data = df), col = "red", lwd = 2)

cor.test(df$Opponent_Aces, df$Rec_Perf, method = "spearman")
# rho = 0.145 → Слаба положителна корелация (p < 0.05)


# ==============================================================================
# 5. ИЗГРАЖДАНЕ НА МОДЕЛИ
# ==============================================================================

# --- 5.1 Модел 1 — Прост (само Opponent_Aces) ---

model1 <- glm(Is_Winner ~ Opponent_Aces, data = df, family = "binomial")
summary(model1)
# AIC: 7254.8 | beta(Opponent_Aces) = +0.0072 (***) — положителен ефект


# --- 5.2 Модел 2 — Сложен (всички три предиктора) ---

model2 <- glm(Is_Winner ~ Opponent_Aces + Rec_Perf + Match_Venue, data = df, family = "binomial")
summary(model2)
# AIC: 7218.6 | Match_VenueAway = -0.3405 (***) — гостуването намалява шанса за победа


# ==============================================================================
# 6. СРАВНЕНИЕ НА МОДЕЛИТЕ И ВЕРОЯТНОСТНИ КРИВИ
# ==============================================================================

# Изчисляване на Z-стойности и вероятности

coeffs1 <- model1$coefficients
Z1      <- round(coeffs1[[1]], 4) + round(coeffs1[[2]], 4) * df[, "Opponent_Aces"]
Probab1 <- round(1 / (1 + exp(-Z1)), 4)
result_DF1 <- data.frame(Z = Z1, Probab = Probab1, Is_Winner = as.character(df[, "Is_Winner"]))
result_DF1 <- result_DF1[order(result_DF1[, "Z"], decreasing = FALSE), ]

coeffs2      <- model2$coefficients
venue_numeric <- ifelse(df[, "Match_Venue"] == "Away", 1, 0)
Z2 <- round(coeffs2[[1]], 4) +
      round(coeffs2[[2]], 4) * df[, "Opponent_Aces"] +
      round(coeffs2[[3]], 4) * df[, "Rec_Perf"] +
      round(coeffs2[[4]], 4) * venue_numeric
Probab2 <- round(1 / (1 + exp(-Z2)), 4)
result_DF2 <- data.frame(Z = Z2, Probab = Probab2, Is_Winner = as.character(df[, "Is_Winner"]))
result_DF2 <- result_DF2[order(result_DF2[, "Z"], decreasing = FALSE), ]

# Nagelkerke R²

n      <- length(model1$y)
L_null <- as.numeric(logLik(glm(Is_Winner ~ 1, data = df, family = "binomial")))

L_mod1       <- as.numeric(logLik(model1))
r2_cox1      <- 1 - exp((2/n) * (L_null - L_mod1))
nagelkerke_r2_model1 <- round(r2_cox1 / (1 - exp((2/n) * L_null)), 4)

L_mod2       <- as.numeric(logLik(model2))
r2_cox2      <- 1 - exp((2/n) * (L_null - L_mod2))
nagelkerke_r2_model2 <- round(r2_cox2 / (1 - exp((2/n) * L_null)), 4)

cat("Nagelkerke R² — Модел 1:", nagelkerke_r2_model1, "\n")
cat("Nagelkerke R² — Модел 2:", nagelkerke_r2_model2, "\n")

# Вероятностни криви

xlable1 <- paste0("Z = ", round(coeffs1[[1]], 4), " + ", round(coeffs1[[2]], 4), "*Opponent_Aces")
xlable2 <- paste0("Z = ", round(coeffs2[[1]], 4), " + ", round(coeffs2[[2]], 4), "*Aces + ",
                  round(coeffs2[[3]], 4), "*Rec + ", round(coeffs2[[4]], 4), "*Venue")

par(mfrow = c(1, 2))

y_numeric1 <- ifelse(result_DF1$Is_Winner == "Yes", 1, 0)
plot(result_DF1[, "Z"], y_numeric1,
     xlab = xlable1, ylab = "Is Winner",
     main = "Probability curve (Model 1)",
     pch = 16, col = "darkgreen", yaxt = "n")
axis(2, at = c(0, 1), labels = c("No", "Yes"))
text(mean(Z1), 0.5, paste("Nagelkerke R2 =", nagelkerke_r2_model1), col = "darkred", font = 2)
lines(result_DF1[, "Z"], result_DF1[, "Probab"], col = "red", lwd = 2)

y_numeric2 <- ifelse(result_DF2$Is_Winner == "Yes", 1, 0)
plot(result_DF2[, "Z"], y_numeric2,
     xlab = xlable2, ylab = "Is Winner",
     main = "Probability curve (Model 2)",
     pch = 16, col = "blue", yaxt = "n")
axis(2, at = c(0, 1), labels = c("No", "Yes"))
text(mean(Z2), 0.5, paste("Nagelkerke R2 =", nagelkerke_r2_model2), col = "darkred", font = 2)
lines(result_DF2[, "Z"], result_DF2[, "Probab"], col = "red", lwd = 2)

par(mfrow = c(1, 1))
