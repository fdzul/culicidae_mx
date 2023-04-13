# Step 1. load the culicidae_mx dataset ####
if(as.vector(Sys.info()["sysname"]) == "Darwin"){
    load("~/Library/CloudStorage/Dropbox/projects/culicidae_mx/1.datasets/1.5.output_datasets/culicidae_mx_dataset.RData")
} else if(as.vector(Sys.info()["sysname"]) == "Windows"){
    load("C:/Users/HOME/Dropbox/projects/culicidae_mx/1.datasets/1.5.output_datasets/culicidae_mx_dataset.RData")
} else if(as.vector(Sys.info()["sysname"]) == "linux"){
    
}



x <- culicidae_mx_dataset |> 
    dplyr::filter(!is.na(specie)) |>
    dplyr::filter(!is.na(long)) |>
    dplyr::mutate(genera_specie = specie) |>
    tidyr::separate(col = "genera_specie",
                    into = c("genero")) |>
    dplyr::filter(!is.na(genero)) |>
    dplyr::filter(genero %in% c("Aedes",
                                "Anopheles",
                                "Coquillettidia",
                                "Culex",
                                "Culiseta",
                                "Haemagogus",
                                "Mansonia",
                                "Psorophora",
                                "Sabethes",
                                "Trichoprosopon")) |>
    dplyr::group_by(genero) |>
    dplyr::summarise(x1 = length(unique(specie)),
                     "occ" = dplyr::n(),
                     "unique_occ" = dplyr::n_distinct(specie, long, lat))  

##

#
y <- culicidae_mx_dataset |> 
    dplyr::filter(!is.na(specie)) |>
    dplyr::filter(!is.na(long)) |>
    dplyr::mutate(genera_specie = specie) |>
    tidyr::separate(col = "genera_specie",
                    into = c("genero")) |>
    dplyr::filter(!is.na(genero)) |>
    dplyr::filter(genero %in% c("Aedes",
                                "Anopheles",
                                "Coquillettidia",
                                "Culex",
                                "Culiseta",
                                "Haemagogus",
                                "Mansonia",
                                "Psorophora",
                                "Sabethes",
                                "Trichoprosopon")) |>
    dplyr::filter(specie %in% c("Aedes aegypti",
                                "Aedes albopictus",
                                "Aedes vexans",
                                "Aedes epactius",
                                "Aedes scapularis",
                                "Aedes sollicitans",
                                "Aedes taeniorhynchus",
                                "Aedes triseriatus",
                                "Aedes trivittatus",
                                # Anopheles
                                "Anopheles aztecus",
                                "Anopheles freeborni",
                                "Anopheles pseudopunctipennis",
                                "Anopheles punctipennis",
                                "Anopheles punctimacula",
                                "Anopheles quadrimaculatus",
                                "Anopheles neivai",
                                "Anopheles albimanus",
                                "Anopheles vestitipennis",
                                "Anopheles darlingi",
                                # Coquilletidia
                                "Coquillettidia perturbans",
                                "Coquillettidia venezuelensis",
                                "Coquillettidia nigricans",
                                # Culex
                                "Culex coronator",
                                "Culex erithrothorax",
                                "Culex nigripalpus",
                                "Culex coronator",
                                "Culex pipiens",
                                "Culex quinquefasciatus",
                                "Culex restuans",
                                "Culex salinarius",
                                "Culex stigmatosoma",
                                "Culex tarsalis",
                                "Culex thriambus",
                                # Culiseta
                                "Culiseta inornata",
                                "Culiseta particeps",
                                "Culiseta melanura",
                                # Haemagogus
                                "Haemagogus equinus",
                                "Haemagogus mesodentatus",
                                "Haemagogus anastasionis",
                                # Mansonia 
                                "Mansonia titillans",
                                # Psorophora
                                "Psorophora ciliata",
                                "Psorophora ferox",
                                "Psorophora confinnis",
                                # Sabethes
                                "Sabethes chloropterus",
                                # Trichoprosopon
                                "Trichoprosopon digitatum")) |>
    dplyr::group_by(genero) |>
    dplyr::summarise(x1 = length(unique(specie)),
                     "occ" = dplyr::n(),
                     "unique_occ" = dplyr::n_distinct(specie, long, lat))

    
tibble::tibble(Genero = x$genero,
               Especies = x$x1,
               "Especies IM" = y$x1,
               "Registros" = x$occ,
               "Registros IM" = y$occ,
               "Registros Unicos" = x$unique_occ,
               "Registros Unicos IM" = y$unique_occ)

