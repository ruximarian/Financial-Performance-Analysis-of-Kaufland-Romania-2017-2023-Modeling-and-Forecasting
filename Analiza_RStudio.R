#Regresie liniara

date<-data.frame(Rezultate_financiare2)

chelt_totale<-date$Cheltuieli_totale
profit_brut<-date$Cheltuieli_totale

install.packages("readxl")
library(readxl)

model<-lm(profit_brut~chelt_totale)
summary(model)

#predictii pe baza modelului de regresie liniara
predictii<-predict(model)

date$predictii_profit<-predictii


# Reprezentarea grafică a variabilelor și a dreptei de regresie
plot(chelt_totale, profit_brut, 
     main = "Regresia liniară: Profit Brut vs. Cheltuieli Totale",
     xlab = "Cheltuieli Totale",
     ylab = "Profit Brut",
     pch = 19, # Tipul punctelor
     col = "blue") # Culoarea punctelor

# Adăugarea dreptei de regresie
abline(model, col = "red", lwd = 2) # Dreapta de regresie (linia)

# Adăugarea unei legende
legend("topleft", legend = c("Date observate", "Dreapta de regresie"), 
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

# Reprezentarea grafică a valorilor observate și previzionate pentru profit brut

# Setarea graficului principal
plot(chelt_totale, profit_brut, 
     main = "Valori Observate și Previzionate pentru Profitul Brut",
     xlab = "Cheltuieli Totale",
     ylab = "Profit Brut",
     pch = 19, # Puncte pentru valorile observate
     col = "blue", # Culoarea punctelor observate
     cex = 1.2) # Dimensiunea punctelor

# Adăugarea valorilor previzionate
points(chelt_totale, profit_brut, 
       pch = 17, # Triunghiuri pentru valorile previzionate
       col = "red", # Culoarea punctelor previzionate
       cex = 1.2) # Dimensiunea punctelor

# Adăugarea dreptei de regresie
abline(model, col = "darkgreen", lwd = 2) # Dreapta de regresie

# Adăugarea unei legende
legend("bottomright", 
       legend = c("Valori Observate", "Valori Previzionate", "Dreapta de regresie"), 
       col = c("blue", "red", "darkgreen"), 
       pch = c(19, 17, NA), 
       lty = c(NA, NA, 1), 
       lwd = c(NA, NA, 2))

# Încărcarea bibliotecilor necesare
install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)

# Crearea seriei temporale din variabila "Venituri_totale"
venituri_ts <- ts(date$Venituri_totale, start = min(date$An), end = max(date$An), frequency = 1)

# Vizualizarea seriei temporale
plot(venituri_ts, main = "Evoluția Veniturilor Totale", xlab = "An", ylab = "Venituri Totale", col = "blue", lwd = 2)

# Testarea staționarității seriei (Dickey-Fuller Test)
adf_test <- adf.test(venituri_ts, alternative = "stationary")

# Dacă seria nu este staționară, diferențiem seria
if (adf_test$p.value > 0.05) {
  venituri_diff <- diff(venituri_ts)
  plot(venituri_diff, main = "Seria diferențiată a Veniturilor", xlab = "An", ylab = "Diferența Venituri", col = "red", lwd = 2)
}

# Determinarea automată a parametrilor ARIMA (p, d, q) folosind auto.arima
model_arima <- auto.arima(venituri_ts, seasonal = FALSE, stepwise = TRUE, approximation = FALSE)

# Rezumatul modelului ARIMA
summary(model_arima)

# Validarea modelului prin diagramele reziduurilor
checkresiduals(model_arima)

# Predicții pentru următorii 5 ani
forecast_arima <- forecast(model_arima, h = 5)

# Vizualizarea predicțiilor
plot(forecast_arima, main = "Predicțiile pentru Venituri Totale", xlab = "An", ylab = "Venituri Totale")


# Încărcarea bibliotecilor necesare
install.packages("tidyverse")
library(tidyverse)

# Crearea unui nou dataset cu scenarii
data <- date %>%
  mutate(
    Optimist = Venituri_totale * 1.10,  # Creștere cu 10%
    Pesimist = Venituri_totale * 0.90   # Scădere cu 10%
  )

# Transformarea datelor într-un format lung pentru grafic
data_long <- data %>%
  pivot_longer(
    cols = c(Venituri_totale, Optimist, Pesimist),  # Coloană reală și scenarii
    names_to = "Scenario",                         # Etichete pentru scenarii
    values_to = "Value"                            # Valoarea pentru fiecare scenariu
  )

# Graficul analizelor de scenarii
ggplot(data_long, aes(x = factor(An), y = Value, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c("Venituri_totale" = "blue", "Optimist" = "red", "Pesimist" = "green"),
    labels = c("Venituri reale", "Scenariu optimist", "Scenariu pesimist")
  ) +
  labs(
    title = "Analiza de scenarii pentru Venituri Totale",
    x = "An",
    y = "Valoare Venituri",
    color = "Scenariu"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


