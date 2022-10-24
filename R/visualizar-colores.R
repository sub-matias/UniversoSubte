colores_barchart <- function(filename){
  my_data <- analizar_foto(filename)
  
  my_data$n <- my_data$n/sum(my_data$n)
  my_data %>% 
    ggplot(aes(x=color, y=n, fill=color ))+
    geom_col()+
    scale_fill_manual(values = c("B"="blue", "G" = "green", "R" = "tomato"))+
    scale_y_continuous(labels = scales::percent)
}

colores_density <- function(filename){
  my_data <- foto_a_dataframe(filename)
  my_data %>% 
    pivot_longer(cols = c(R,G,B)) %>% 
    ggplot(aes(value,
               #fill=name, 
               color = name))+
    geom_density(alpha = .5)+
    scale_fill_manual(values = c("B"="blue", "G" = "green", "R" = "tomato"))+
    scale_color_manual(values = c("B"="blue", "G" = "green", "R" = "tomato"))
}