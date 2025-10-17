population_old_idx <- function(population) {
  population %>%
    summarise(
      totale = sum(Totale),
      under_14 = sum(Totale[Età <= 14]),
      under_14_perc = under_14/totale*100,
      over_65  = sum(Totale[Età >= 65]),
      over_65_perc = over_65/totale*100
    )
}

calculate_quantiles <- function(x, freq, probs = c(0, 0.25, 0.5, 0.75, 1)) {
  # Controllo di base
  if (length(x) != length(freq)) {
    stop("Le lunghezze di x e freq non coincidono.")
  }
  
  # Rimuove valori con frequenza nulla (non servono per il calcolo)
  valid <- freq > 0
  x <- x[valid]
  freq <- freq[valid]
  
  # Ordina per età
  ord <- order(x)
  x <- x[ord]
  freq <- freq[ord]
  
  # Cumulative e totale
  cumfreq <- cumsum(freq)
  N = sum(freq)
  if (N == 0) stop("Tutte le frequenze sono zero!")
  
  # Calcolo dei quantili con interpolazione
  results = sapply(probs, function(p) {
    target = p * N
    i = which(cumfreq >= target)[1]
    if (is.na(i)) {
      stop(paste("Nessun indice trovato per il quantile", p))
    }
    if (i == 1) return(x[1])
    x0 = x[i - 1]
    x1 = x[i]
    f0 = cumfreq[i - 1]
    f1 = cumfreq[i]
    return(x0 + (x1 - x0) * (target - f0) / (f1 - f0))
  })
  
  return(results)
}

preliminary_checks <- function(residents, title) {
  # ---- Stampo il boxplot ----
  par(mfrow = c(1, 3), oma = c(0, 0, 3, 0))
  
  males = boxplot(residents$maschi, main = "Maschi", ylab = "Numero di Residenti")
  females = boxplot(residents$femmine, main = "Femmine", ylab = "")
  total = boxplot(residents$Totale, main = "Totale", ylab = "")
  
  # ---- Quartili ----
  print("Maschi")
  print("Quartili")
  quart_maschi = calculate_quantiles(residents$Età, residents$maschi)
  print(quart_maschi)
  cat("Distanza interqurtile: ", quart_maschi[4] - quart_maschi[2], "\n")
  cat("Range: ", quart_maschi[5] - quart_maschi[1], "\n")
  
  print("Femmine")
  print("Quartili")
  quart_femmine = calculate_quantiles(residents$Età, residents$femmine)
  print(quart_femmine)
  cat("Distanza interqurtile: ", quart_femmine[4] - quart_femmine[2], "\n")
  cat("Range: ", quart_femmine[5] - quart_femmine[1], "\n")
  
  print("Totale")
  print("Quartili")
  quart_tot = calculate_quantiles(residents$Età, residents$Totale)
  print(quart_tot)
  cat("Distanza interqurtile: ", quart_tot[4] - quart_tot[2], "\n")
  cat("Range: ", quart_tot[5] - quart_tot[1], "\n")
  
  mtext(title, outer = TRUE, cex = 1.2, font = 2, line = 0.5)
  
  par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))
  
  # ---- Calcolo le medie ----
  
  n_maschi = sum(residents$maschi)
  n_femmine = sum(residents$femmine)
  n_totale = sum(residents$Totale)
  
  media_maschi = sum(residents$Età * residents$maschi)/n_maschi
  media_femmine = sum(residents$Età * residents$femmine)/n_femmine
  media_totale = sum(residents$Età * residents$Totale)/n_totale
  
  # ---- Varianze ----
  var_maschi  = sum(residents$maschi  * (residents$Età - media_maschi)^2) / n_maschi
  var_femmine = sum(residents$femmine * (residents$Età - media_femmine)^2) / n_femmine
  var_totale  = sum(residents$Totale  * (residents$Età - media_totale)^2) / n_totale
  
  # ---- Calcolo gli outliers ----
  
  risultati_df <- data.frame(
    Statistica = c("Media Età", "Varianza", "Deviazione standard", "N° Outlier"),
    Maschi  = c(media_maschi, var_maschi, sqrt(var_maschi), length(males$out)),
    Femmine = c(media_femmine, var_femmine, sqrt(var_femmine), length(females$out)),
    Totale  = c(media_totale, var_totale, sqrt(var_femmine), length(total$out))
  )
  
  # ---- Tabella formattata ----
  kable(risultati_df, digits = 2, caption = paste("Statistiche per", title))
}