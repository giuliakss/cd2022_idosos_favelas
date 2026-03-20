# ========================================
# Mapas - % Idosos
# ========================================

library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)
library(ggspatial)

source("scripts/00_variaveis.R")

# ------------------------
# 1. Carregar dados
# ------------------------

base_setores <- readRDS("dados/derivados/base_setores_2022_rj_com_infos.rds")
base_bairros <- readRDS("dados/derivados/base_bairros_rj_com_infos.rds")

# ------------------------
# 2. Padronizar tipos
# ------------------------

base_setores$CD_BAIRRO <- as.numeric(base_setores$CD_BAIRRO)
base_bairros$CD_BAIRRO <- as.numeric(base_bairros$CD_BAIRRO)

# ------------------------
# 3. Criar indicador
# ------------------------

base_setores <- base_setores %>%
  mutate(
    pop_idosa = rowSums(across(all_of(vars_total_pop_60_ou_mais)), na.rm = TRUE),
    pop_total = rowSums(across(all_of(vars_total_pop)), na.rm = TRUE)
  )

# ------------------------
# 4. MAPA 1 (bairros)
# ------------------------

# Essa visualização não é tão útil para os propósitos deste trabalho mas é mantida porque pode auxiliar em outros estudos espaciais
# Também pode servir para propósitos de validação

base_bairros <- base_bairros %>%
  mutate(
    pop_idosa = rowSums(across(all_of(vars_total_pop_60_ou_mais)), na.rm = TRUE),
    pop_total = rowSums(across(all_of(vars_total_pop)), na.rm = TRUE),
    perc_idosos = pop_idosa / pop_total * 100
  ) %>%
  st_transform(4326)

pal1 <- colorNumeric("Reds", domain = base_bairros$perc_idosos)

mapa1 <- leaflet(base_bairros) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal1(perc_idosos),
    color = "white",
    weight = 1,
    fillOpacity = 0.7
  ) %>%
  addLegend(pal = pal1, values = ~perc_idosos, title = "% Idosos")

# ------------------------
# 5. Preparar setores
# ------------------------

base_setores <- st_transform(base_setores, 4326)

setores_favela <- base_setores %>%
  filter(CD_TIPO == "1")

# ------------------------
# 6. Cálculo por setores FCU e ex-fCU e cidade como um todo
# ------------------------

# ---- FAVELAS
favelas <- setores_favela %>%
  mutate(id_favela = NM_FCU) %>%
  group_by(id_favela) %>%
  summarise(
    pop_idosa = sum(pop_idosa),
    pop_total = sum(pop_total),
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  mutate(
    perc_idosos = ifelse(pop_total > 0, pop_idosa / pop_total * 100, NA),
    tipo = "favela",
    nome = id_favela
  )

# ---- TOTAL POR BAIRRO (DOS SETORES)
bairros_total <- base_setores %>%
  st_drop_geometry() %>%
  group_by(CD_BAIRRO, NM_BAIRRO) %>%
  summarise(
    pop_idosa_total = sum(pop_idosa),
    pop_total_total = sum(pop_total),
    .groups = "drop"
  )

# ---- FAVELA POR BAIRRO
favela_por_bairro <- setores_favela %>%
  st_drop_geometry() %>%
  group_by(CD_BAIRRO) %>%
  summarise(
    pop_idosa_fav = sum(pop_idosa),
    pop_total_fav = sum(pop_total),
    .groups = "drop"
  )

# ---- EX-FAVELA
bairros_info <- bairros_total %>%
  left_join(favela_por_bairro, by = "CD_BAIRRO") %>%
  mutate(
    pop_idosa_fav = ifelse(is.na(pop_idosa_fav), 0, pop_idosa_fav),
    pop_total_fav = ifelse(is.na(pop_total_fav), 0, pop_total_fav),
    
    pop_idosa = pop_idosa_total - pop_idosa_fav,
    pop_total = pop_total_total - pop_total_fav,
    
    perc_idosos = ifelse(pop_total > 0, pop_idosa / pop_total * 100, NA),
    tipo = "bairro_ex_FCU"
  )

# ---- GEOMETRIA DOS BAIRROS
bairros_sem_favela <- base_bairros %>%
  select(CD_BAIRRO, NM_BAIRRO, geometry) %>%
  left_join(bairros_info %>% select(-NM_BAIRRO), by = "CD_BAIRRO") %>%
  mutate(nome = NM_BAIRRO)

# ------------------------
# 7. MAPA 2
# ------------------------

valores <- c(
  bairros_sem_favela$perc_idosos,
  favelas$perc_idosos
)

pal2 <- colorNumeric("Reds", domain = valores, na.color = "transparent")

mapa2 <- leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    data = bairros_sem_favela,
    fillColor = ~pal2(perc_idosos),
    color = "white",
    weight = 1,
    fillOpacity = 0.6,
    popup = ~paste0(
      "<b>Bairro:</b> ", nome, "<br>",
      "<b>% Idosos (ex-favela):</b> ", round(perc_idosos, 1), "%"
    )
  ) %>%
  
  addPolygons(
    data = favelas,
    fillColor = ~pal2(perc_idosos),
    color = "black",
    weight = 1,
    fillOpacity = 0.9,
    popup = ~paste0(
      "<b>Favela:</b> ", nome, "<br>",
      "<b>% Idosos:</b> ", round(perc_idosos, 1), "%"
    )
  ) %>%
  
  addLegend(pal = pal2, values = valores, title = "% Idosos")

mapa2


# Alguns checks de sanidadde

check_valores <- c(bairros_sem_favela$pop_idosa,favelas$pop_idosa)
sum(check_valores)
sum(bairros_total$pop_idosa_total)

# total original (setores)
total_setores <- base_setores %>%
  st_drop_geometry() %>%
  summarise(total = sum(pop_idosa, na.rm = TRUE)) %>%
  pull(total)

# total reconstruído (mapa)
total_mapa <- sum(
  c(bairros_sem_favela$pop_idosa, favelas$pop_idosa),
  na.rm = TRUE
)

total_setores == total_mapa

total_bairros <- base_bairros %>%
  st_drop_geometry() %>%
  summarise(total = sum(pop_idosa, na.rm = TRUE)) %>%
  pull(total)

total_bairros == total_setores

base_mapa2 <- bind_rows(
  bairros_sem_favela %>%
    st_drop_geometry() %>%
    mutate(tipo = "Bairro (ex-FCU)"),
  
  favelas %>%
    st_drop_geometry() %>%
    mutate(tipo = "Favela")
)

sum(base_setores$pop_total)
sum(favelas$pop_total) + sum(bairros_sem_favela$pop_total)


# ------------------------
# 8. MAPA ESTÁTICO
# ------------------------

mapa2_static <- ggplot() +
  
  geom_sf(
    data = bairros_sem_favela,
    aes(fill = perc_idosos),
    color = "black",
    size = 0.9
  ) +
  
  geom_sf(
    data = favelas,
    aes(fill = perc_idosos),
    color = "black",
    size = 0.2
  ) +
  
  scale_fill_gradient(
    low = "#fde0dd",
    high = "#a50f15",
    name = "% População com\n60 anos ou mais"
  ) +
  
  theme_void()

ggsave(
  "resultados/mapa2_favela.png",
  mapa2_static,
  width = 10,
  height = 8,
  dpi = 300
)

# ------------------------
# 9. LEME
# ------------------------

leme_bbox <- base_bairros %>%
  dplyr::filter(NM_BAIRRO == "Leme") %>%
  st_bbox()

leme_bbox_exp <- leme_bbox + c(-0.001, -0.001, 0.001, 0.001)

favelas_leme <- favelas %>%
  dplyr::filter(id_favela %in% c("Babilônia", "Chapéu Mangueira"))

mapa_leme <- ggplot() +
  
  geom_sf(
    data = bairros_sem_favela,
    aes(fill = perc_idosos),
    color = "grey30",
    size = 0.3
  ) +
  
  geom_sf(
    data = favelas_leme,
    fill = NA,
    color = "yellow",
    size = 0.6
  ) +
  
  geom_label(
    data = favelas_leme,
    aes(
      geometry = geometry,
      label = paste0(id_favela, "\n", round(perc_idosos, 1), "%")
    ),
    stat = "sf_coordinates",
    size = 3,
    fill = "white"
  ) +
  
  coord_sf(
    xlim = c(leme_bbox_exp["xmin"], leme_bbox_exp["xmax"]),
    ylim = c(leme_bbox_exp["ymin"], leme_bbox_exp["ymax"])
  ) +
  
  scale_fill_gradient(
    low = "#c6dbef",
    high = "#08306b",
    name = "% Idosos"
  ) +
  
  theme_void()

mapa_leme

