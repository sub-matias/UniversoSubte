analizar_foto <- function(filename){
  my_image <- image_read(filename)
  mis_colores <- my_image %>% 
    as.raster() %>% 
    as.matrix() %>% 
    as.vector() 
  colores_df <- data.frame(
    R = str_sub(mis_colores,2,3),
    G = str_sub(mis_colores,4,5),
    B = str_sub(mis_colores,6,7)
  )
  colores_df %>%
    mutate(
      R=hex_to_num(R),
      G=hex_to_num(G),
      B=hex_to_num(B)
    ) -> tmp
  df_analysis <- data.frame(color = colnames(tmp)[max.col(tmp)] )
  df_analysis %>%
    group_by(color) %>%
    count()
  
}
hex_to_num <- function( hex ){
  paste0("0x", hex) %>% 
    as.numeric()
}