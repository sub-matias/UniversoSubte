
analizar_foto <- function(filename){
  tmp <- foto_a_dataframe(filename = filename) %>% 
    select(R, B, G)
  df_analysis <- data.frame(color = colnames(tmp)[max.col(tmp)] )
  df_analysis %>%
    group_by(color) %>%
    count()
}

hex_to_num <- function( hex ){
  paste0("0x", hex) %>% 
    as.numeric()
}

