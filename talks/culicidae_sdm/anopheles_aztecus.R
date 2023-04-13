
ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = pred |>
                                   terra::rast()) +
    #ggplot2::geom_point(data = eval@occs, 
    #                    ggplot2::aes(x=longitude, 
    #                                 y=latitude), 
    #                    col='red', cex = 0.05) +
    ggplot2::scale_fill_viridis_c("Probability",
                                  na.value = "white",
                                  breaks = seq(from = 0, 
                                               to = 1, 
                                               by = .2)) +
    ggplot2::ggtitle(paste("SDM", "Anopheles aztecus", 
                           paste("n =",  
                                 nrow(eval@occs)),
                           sep = " ")) +
    cowplot::theme_map() +
    ggplot2::theme(legend.position = c(0.8, 0.7))
