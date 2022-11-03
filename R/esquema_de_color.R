#Redondeo
round_to_any <- function (x, accuracy, f = round) 
{
  f(x/accuracy) * accuracy
}
round_RGB_df <- function(df, accuracy, f = round, max_val = 255){
  df %>% 
    mutate(
      R = round_to_any(R, accuracy, f),
      G = round_to_any(G, accuracy, f),
      B = round_to_any(B, accuracy, f)
    ) %>% 
    mutate(
      R = ifelse(R > max_val, max_val, R ),
      G = ifelse(G > max_val, max_val, G ),
      B = ifelse(B > max_val, max_val, B ),
    )
}

esquema_de_color <- function(imagen, accuracy = 190){
  tmp_df <- to_RGB_df(imagen) %>% 
    round_RGB_df (accuracy = accuracy) %>% 
    mutate(RGB = rgb(R,G,B, maxColorValue = 255))
  #Escala
  my_color_codes <- tmp_df$RGB %>% unique
  names(my_color_codes) <- my_color_codes
  tmp_df %>% 
    group_by(RGB) %>% 
    count() %>% 
    ggplot(aes(x=RGB,y=n,fill=RGB))+
    geom_col()+
    scale_fill_manual(values = my_color_codes)+
    theme(legend.position = "none")
}
