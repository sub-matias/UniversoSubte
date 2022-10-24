hex_to_num <- function( hex ){
  # Aca falta check de validez del input
  paste0("0x", hex) %>% 
    as.numeric()
}

to_RGB_df <- function(my_image){
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
    )
}

convert_to_RGB_df <- function(filename){
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
    )
}