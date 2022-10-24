foto_a_dataframe <- function(filename){
  my_image <- image_read(filename)
  mis_colores <- my_image %>% 
    as.raster() %>% 
    as.matrix() %>% 
    as.vector() 
  colores_df <- data.frame(
    Red = str_sub(mis_colores,2,3),
    Green = str_sub(mis_colores,4,5),
    Blue = str_sub(mis_colores,6,7)
  )
  colores_df %>%
    mutate(
      R=hex_to_num(Red),
      G=hex_to_num(Green),
      B=hex_to_num(Blue)
    ) -> tmp
  tmp
}