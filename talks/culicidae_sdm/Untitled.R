
# Step 1. load the culicidae_mx dataset ####
if(as.vector(Sys.info()["sysname"]) == "Darwin"){
    load("~/Library/CloudStorage/Dropbox/projects/culicidae_mx/1.datasets/1.5.output_datasets/culicidae_mx_dataset.RData")
} else if(as.vector(Sys.info()["sysname"]) == "Windows"){
    load("C:/Users/HOME/Dropbox/projects/culicidae_mx/1.datasets/1.5.output_datasets/culicidae_mx_dataset.RData")
} else if(as.vector(Sys.info()["sysname"]) == "linux"){
    
}

y <- culicidae_mx_dataset |> 
    dplyr::filter(!is.na(specie)) |>
    dplyr::filter(!is.na(long)) |>
    dplyr::filter(!specie %in% c("Wyeomyia sp", "Uranotaenia sp",     
                                "Psorophora sp2","Psorophora sp1",      
                                "Psorophora sp", "Culex sp2",           
                                "Culex sp1","Culex sp",            
                                "Culex Melanoconion sp", "Anopheles sp",        
                                "Aedes sp3", "Aedes sp2",           
                                "Aedes sp1","Aedes sp",             
                                "Aedes Aedes sp")) |>
    dplyr::mutate(genera_specie = specie) |>
    tidyr::separate(col = "genera_specie",
                    into = c("genero")) |>
    dplyr::filter(!is.na(genero)) |>
    dplyr::group_by(dataset,genero, specie) |>
    dplyr::summarise("occ" = dplyr::n(),
                     "unique_occ" = dplyr::n_distinct(long, lat)) |>
    dplyr::filter(genero != "NA")

sort(unique(stringr::str_subset(string = y$specie,
                                pattern = " sp")),
     decreasing = TRUE)


ggplot2::ggplot(data = y |>
                    dplyr::filter(specie %in% c("Aedes aegypti",
                                                "Aedes albopictus",
                                                "Aedes vexans",
                                                "Aedes epactius",
                                                "Aedes scapularis",
                                                "Aedes sollicitans",
                                                "Aedes taeniorhynchus",
                                                "Aedes triseriatus",
                                                "Aedes trivittatus")) |>
                    dplyr::mutate(specie = stringr::str_replace_all(specie,
                                                                    pattern = "Aedes ",
                                                                    replacement = "Ae. ")),
                ggplot2::aes(y = occ,
                             x = specie,
                             fill = dataset)) + 
    ggplot2::geom_bar(stat = "identity",
                      orientation = "x") +
    ggplot2::theme(legend.position = c(.9, .72),
                   legend.background = ggplot2::element_blank()) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Registros") +
    ggplot2::xlab("Especie")

ggplot2::ggsave(filename = "talks/culicidae_sdm/occ_aedes.jpg")

ggplot2::ggplot(data = y |>
                    dplyr::filter(specie %in% c("Aedes aegypti",
                                                "Aedes albopictus",
                                                "Aedes vexans",
                                                "Aedes epactius",
                                                "Aedes scapularis",
                                                "Aedes sollicitans",
                                                "Aedes taeniorhynchus",
                                                "Aedes triseriatus",
                                                "Aedes trivittatus")) |>
                    dplyr::mutate(specie = stringr::str_replace_all(specie,
                                                                    pattern = "Aedes ",
                                                                    replacement = "Ae. ")),
                ggplot2::aes(y = unique_occ,
                             x = specie,
                             fill = dataset)) + 
    ggplot2::geom_bar(stat = "identity",
                      orientation = "x") +
    ggplot2::theme(legend.position = c(.9, .72),
                   legend.background = ggplot2::element_blank()) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Registros") +
    ggplot2::xlab("Especie")

ggplot2::ggsave(filename = "talks/culicidae_sdm/unique_occ_aedes.jpg")

#  aedes
culicidae_mx_dataset |> 
    dplyr::filter(!is.na(specie)) |>
    dplyr::filter(!is.na(long)) |>
    dplyr::filter(!specie %in% c("Wyeomyia sp", "Uranotaenia sp",     
                                 "Psorophora sp2","Psorophora sp1",      
                                 "Psorophora sp", "Culex sp2",           
                                 "Culex sp1","Culex sp",            
                                 "Culex Melanoconion sp", "Anopheles sp",        
                                 "Aedes sp3", "Aedes sp2",           
                                 "Aedes sp1","Aedes sp",             
                                 "Aedes Aedes sp")) |>
    dplyr::mutate(genera_specie = specie) |>
    tidyr::separate(col = "genera_specie",
                    into = c("genero")) |>
    dplyr::filter(!is.na(genero)) |>
    dplyr::group_by(dataset,genero, specie) |>
    dplyr::summarise("occ" = dplyr::n(),
                     "unique_occ" = dplyr::n_distinct(long, lat)) |>
    dplyr::filter(genero != "NA") |>
    dplyr::filter(specie %in% c("Aedes aegypti",
                                "Aedes albopictus",
                                "Aedes vexans",
                                "Aedes epactius",
                                "Aedes scapularis",
                                "Aedes sollicitans",
                                "Aedes taeniorhynchus",
                                "Aedes triseriatus",
                                "Aedes trivittatus")) |>
    dplyr::mutate(specie = stringr::str_replace_all(specie,
                                                    pattern = "Aedes ",
                                                    replacement = "Ae. ")) |>
    dplyr::select(-occ, -genero) |>
    tidyr::pivot_wider(id_cols = specie,
                       names_from = dataset,
                       values_from = unique_occ,
                       values_fill = 0) |>
    dplyr::mutate(registros = rowSums(dplyr::across(cdmx:veim))) |>
    dplyr::mutate(Porcentahe = round((registros/sum(registros)) *100, 1)) |>
    kableExtra::kable() |>
    kableExtra::kable_classic()
                  
                  
#  anopheles
culicidae_mx_dataset |> 
    dplyr::filter(!is.na(specie)) |>
    dplyr::filter(!is.na(long)) |>
    dplyr::filter(!specie %in% c("Wyeomyia sp", "Uranotaenia sp",     
                                 "Psorophora sp2","Psorophora sp1",      
                                 "Psorophora sp", "Culex sp2",           
                                 "Culex sp1","Culex sp",            
                                 "Culex Melanoconion sp", "Anopheles sp",        
                                 "Aedes sp3", "Aedes sp2",           
                                 "Aedes sp1","Aedes sp",             
                                 "Aedes Aedes sp")) |>
    dplyr::mutate(genera_specie = specie) |>
    tidyr::separate(col = "genera_specie",
                    into = c("genero")) |>
    dplyr::filter(!is.na(genero)) |>
    dplyr::group_by(dataset,genero, specie) |>
    dplyr::summarise("occ" = dplyr::n(),
                     "unique_occ" = dplyr::n_distinct(long, lat)) |>
    dplyr::filter(genero != "NA") |>
    dplyr::filter(specie %in% c("Anopheles aztecus",
                                "Anopheles freeborni",
                                "Anopheles pseudopunctipennis",
                                "Anopheles punctipennis",
                                "Anopheles punctimacula",
                                "Anopheles quadrimaculatus",
                                "Anopheles neivai",
                                "Anopheles albimanus",
                                "Anopheles vestitipennis",
                                "Anopheles darlingi")) |>
    dplyr::mutate(specie = stringr::str_replace_all(specie,
                                                    pattern = "Aedes ",
                                                    replacement = "Ae. ")) |>
    dplyr::select(-occ, -genero) |>
    tidyr::pivot_wider(id_cols = specie,
                       names_from = dataset,
                       values_from = unique_occ,
                       values_fill = 0) |>
    dplyr::mutate(registros = rowSums(dplyr::across(cdmx:veim))) |>
    dplyr::mutate(Porcentahe = round((registros/sum(registros)) *100, 1)) |>
    kableExtra::kable() |>
    kableExtra::kable_classic()    


#  culec
culicidae_mx_dataset |> 
    dplyr::filter(!is.na(specie)) |>
    dplyr::filter(!is.na(long)) |>
    dplyr::filter(!specie %in% c("Wyeomyia sp", "Uranotaenia sp",     
                                 "Psorophora sp2","Psorophora sp1",      
                                 "Psorophora sp", "Culex sp2",           
                                 "Culex sp1","Culex sp",            
                                 "Culex Melanoconion sp", "Anopheles sp",        
                                 "Aedes sp3", "Aedes sp2",           
                                 "Aedes sp1","Aedes sp",             
                                 "Aedes Aedes sp")) |>
    dplyr::mutate(genera_specie = specie) |>
    tidyr::separate(col = "genera_specie",
                    into = c("genero")) |>
    dplyr::filter(!is.na(genero)) |>
    dplyr::group_by(dataset,genero, specie) |>
    dplyr::summarise("occ" = dplyr::n(),
                     "unique_occ" = dplyr::n_distinct(long, lat)) |>
    dplyr::filter(genero != "NA") |>
    dplyr::filter(specie %in% c("Culex coronator",
                                "Culex erithrothorax",
                                "Culex nigripalpus",
                                "Culex coronator",
                                "Culex pipiens",
                                "Culex quinquefasciatus",
                                "Culex restuans",
                                "Culex salinarius",
                                "Culex stigmatosoma",
                                "Culex tarsalis",
                                "Culex thriambus")) |>
    dplyr::mutate(specie = stringr::str_replace_all(specie,
                                                    pattern = "Aedes ",
                                                    replacement = "Ae. ")) |>
    dplyr::select(-occ, -genero) |>
    tidyr::pivot_wider(id_cols = specie,
                       names_from = dataset,
                       values_from = unique_occ,
                       values_fill = 0) |>
    dplyr::mutate(registros = rowSums(dplyr::across(cdmx:veim))) |>
    dplyr::mutate("%" = round((registros/sum(registros)) *100, 1)) |>
    dplyr::arrange(dplyr::desc(`%`)) |>
    kableExtra::kable() |>
    kableExtra::kable_classic()  


#  otros
culicidae_mx_dataset |> 
    dplyr::filter(!is.na(specie)) |>
    dplyr::filter(!is.na(long)) |>
    dplyr::mutate(genera_specie = specie) |>
    tidyr::separate(col = "genera_specie",
                    into = c("genero")) |>
    dplyr::filter(!is.na(genero)) |>
    dplyr::group_by(dataset, genero, specie) |>
    dplyr::summarise("occ" = dplyr::n(),
                     "unique_occ" = dplyr::n_distinct(long, lat),
                     .groups = "drop") |>
    dplyr::filter(genero != "NA") |>
    dplyr::filter(specie %in% c(
                                "Haemagogus equinus",
                                "Haemagogus mesodentatus",
                                "Haemagogus anastasionis",
                                "Trichoprosopon digitatum",
                                "Psorophora ciliata",
                                "Psorophora ferox",
                                "Psorophora confinnis")) |>
    dplyr::select(-occ) |>
    tidyr::pivot_wider(id_cols = c(genero, specie),
                       names_from = dataset,
                       values_from = unique_occ,
                       values_fill = 0) |>
    dplyr::mutate(registros = rowSums(dplyr::across(conabio:ro))) |>
    dplyr::mutate("%" = round((registros/sum(registros)) *100, 1)) |>
    dplyr::arrange(genero) |>
    kableExtra::kable() |>
    kableExtra::kable_classic() 

########


