visualizar_colores <- function(filename){
  my_image <- image_read(filename)
  #Estoy perdido en este punto, no sé cuál es el paso a seguir en este punto. No toma subtecolores.
  subtecolores <- my_image %>%
    as.raster() %>% 
    as.matrix() %>% 
    as.vector() %>% 
    subtecolores_df <- data.frame(color = subtecolores)
  my_data <- data.frame(
    R = str_sub(subtecolores,2,3),
    G = str_sub(subtecolores,4,5),
    B = str_sub(subtecolores,6,7),
  )
  my_data %>% 
    mutate(
      Red = stringr::str_sub(R,1,1) %>% stringr::str_c("0"),
      Green = stringr::str_sub(G,1,1) %>% stringr::str_c("0"),
      Blue = stringr::str_sub(B,1,1) %>% stringr::str_c("0")
    ) -> my_data
  my_data <- my_data %>% 
    mutate(color_nuevo = paste0("#",Red,Green,Blue))
  my_data %>% 
    group_by(color_nuevo) %>% 
    tally(sort=TRUE) %>% 
    head(30) -> tmp
  tmp %>% 
    ggplot(aes(color_nuevo,n))+
    geom_col(fill = tmp$color_nuevo)
}