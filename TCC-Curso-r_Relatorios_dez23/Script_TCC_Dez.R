library(geobr)
geobr::list_geobr()
dados_geobr<-list_geobr()


escolas<-read_schools()
escolasSP <- escolas %>%  filter(name_muni == "São Paulo")

ensino_medio_sao_paulo <- escolasSP |>
  dplyr::filter(stringr::str_detect(education_level, "Ensino Médio")) |>
  dplyr::group_by(government_level) |> 
  dplyr::mutate(
    numero_linhas = dplyr::n(),
    nome_label = paste0(government_level, " (N=", numero_linhas, ")")
  ) |> 
  dplyr::ungroup()

escolasPV <- escolas %>% 
  filter(name_muni == "Porto Velho")  

ensino_medio_porto_velho <- escolasPV |>
  dplyr::filter(stringr::str_detect(education_level, "Ensino Médio")) |>
  dplyr::group_by(government_level) |> 
  dplyr::mutate(
    numero_linhas = dplyr::n(),
    nome_label = paste0(government_level, " (N=", numero_linhas, ")")
  ) |> 
  dplyr::ungroup()

escolasCB <- escolas %>% 
  filter(name_muni == "Cuiabá")

ensino_medio_cuiaba <- escolasCB |>
  dplyr::filter(stringr::str_detect(education_level, "Ensino Médio")) |>
  dplyr::group_by(government_level) |> 
  dplyr::mutate(
    numero_linhas = dplyr::n(),
    nome_label = paste0(government_level, " (N=", numero_linhas, ")")
  ) |> 
  dplyr::ungroup()

ensino_medio_cuiaba |>
               ggplot() +
               aes(x = government_level, y = size) +
               geom_point(aes(color = government_level)) +
               theme_minimal(12) +
               labs(
                 title = "Escolas de Ensino Medio em Cuiaba",
                 caption = "Fonte: INEP, pacote de dados geobr",
                 x = "Classificaçao da escola: Publica(Estadual ou Federal) ou Privada",
                 y = "No. de Matriculas"
               )

ensino_medio_porto_velho |>
  ggplot() +
  aes(x = government_level, y = size) +
  geom_point(aes(color = government_level)) +
  theme_minimal(12) +
  labs(
    title = "Escolas de Ensino Medio em Porto Velho",
    caption = "Fonte: INEP, pacote de dados geobr",
    x = "Classificaçao da escola: Publica(Estadual ou Federal) ou Privada",
    y = "No. de Matriculas"
  )
