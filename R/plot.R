get_name <- function(values, Q_ID, percents, demo_answers){
  sapply(1:length(values), function(ind){
    value <- values[ind]
    name <- if(is.na(value)){
      "NA"
    } else {
      names(demo_answers[[Q_ID]])[demo_answers[[Q_ID]] == value]
    }
    if(length(name) == 0) name <- as.character(value)
    if(percents[ind] < nchar(name)/2 || percents[ind] < 3) name <- ""
    
    return(name)
  })
}

plot_distribution <- function(demo_data, demo_names, demo_answers, var_name){
  var <- as.name(var_name)
  plot_data <- demo_data %>%
    select(!!var, weight) %>%
    filter(!!var >= 0) %>%
    group_by(!!var) %>%
    summarise(mass = sum(weight)) %>%
    mutate(percent = mass/sum(mass)*100) %>%
    mutate(category = get_name(!!var, var_name, percent, demo_answers),
           index = factor(!!var))
  
  plot <- ggplot(plot_data, aes(x = 1, y = percent, fill = index)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(category), y = percent), size = 2.5,
              position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set3") +
    coord_flip() +
    theme_void() +
    theme(legend.position = "none",
          axis.title.y = element_text(size = ifelse(nchar(demo_names[var_name][var_name]) > 20, 7, 8))) +
    labs(x = demo_names[var_name])
  
  return(plot)
}