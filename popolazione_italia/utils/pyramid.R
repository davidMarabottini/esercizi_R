data_preparation_for_pyramid <- function(ds, maschi, femmine, group_years = 1, min_age=0, max_age=101) {
  
  ds_pivot = ds %>%
    select(Età, maschi, femmine) %>%
    filter(Età >= min_age & Età <= max_age) %>%
    pivot_longer(cols = c(maschi, femmine), names_to="Sesso", values_to="Popolazione" ) %>%
    
    mutate(Popolazione = ifelse(Sesso == maschi, -Popolazione, Popolazione)) %>%
    
    mutate(classe_eta = floor(Età/group_years)*group_years) %>%
    group_by(classe_eta, Sesso) %>%
    
    summarise(Popolazione_aggregata = sum(Popolazione)) %>%
    mutate(gruppo_eta = paste0(classe_eta, " - ", classe_eta + group_years - 1))
  
  return(ds_pivot)
}

generate_pyramid_chart <- function(pivot, title) {
  pop_max <- max(abs(pivot$Popolazione_aggregata), na.rm = TRUE)
  
  ggplot(pivot, aes(x = classe_eta, y = Popolazione_aggregata, fill = Sesso)) +
    
    geom_col() +
    
    coord_flip() +
    xlab("Maschi") +
    ylab("Femmine") +
    
    labs(
      title = title,
      x = "Età",
      y = "Popolazione",
      fill = "Sesso"
    ) +
    theme_minimal()
}
