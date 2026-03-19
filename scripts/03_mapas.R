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

base_bairros <- base_bairros %>%
  mutate(
    pop_idosa = rowSums(across(all_of(vars_total_pop_60_ou_mais)), na.rm = TRUE),
    pop_total = rowSums(across(all_of(vars_total_pop)), na.rm = TRUE),
    perc_idosos = pop_idosa / pop_total * 100
  )

# ------------------------
# 4. MAPA 1 - PERCENTUAL DE IDOSOS POR BAIRROS
# ------------------------

base_bairros <- st_transform(base_bairros, 4326)

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

mapa1

# ------------------------
# 5. Preparar setores
# ------------------------

base_setores <- st_transform(base_setores, 4326)

# setores de favela
setores_favela <- base_setores %>%
  filter(CD_TIPO == "1")

# setores fora de favela e urbanos
setores_nao_favela <- base_setores %>%
  filter(CD_TIPO != "1", CD_SIT %in% c(1, 2, 3))

# ------------------------
# 6. Construir base do mapa 2
# ------------------------

# ---- FAVELAS 
favelas <- setores_favela %>%
  mutate(
    id_favela = NM_FCU
  ) %>%
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

# ---- RESTO DO BAIRRO (sem favela)

# unir todas as favelas (para recorte)
favelas_union <- setores_favela %>%
  summarise(geometry = st_union(geometry))

# calcular indicadores
bairros_info <- setores_nao_favela %>%
  group_by(CD_BAIRRO, NM_BAIRRO) %>%
  summarise(
    pop_idosa = sum(pop_idosa),
    pop_total = sum(pop_total),
    .groups = "drop"
  ) %>%
  mutate(
    perc_idosos = ifelse(pop_total > 0, pop_idosa / pop_total * 100, NA),
    tipo = "bairro_ex_FCU"
  )

bairros_sem_favela <- base_bairros %>%
  select(CD_BAIRRO, NM_BAIRRO, geometry) %>%
  st_difference(favelas_union) %>%
  st_collection_extract("POLYGON") %>%
  left_join(
    st_drop_geometry(bairros_info),
    by = c("CD_BAIRRO", "NM_BAIRRO")
  ) %>%
  filter(pop_total > 0) %>%   
  mutate(nome = NM_BAIRRO)

# ---- BASE FINAL
base_mapa2 <- bind_rows(
  bairros_sem_favela,
  favelas
)

# ------------------------
# 7. MAPA 2 - FINAL
# ------------------------

valores <- base_mapa2$perc_idosos[!is.na(base_mapa2$perc_idosos)]
pal2 <- colorNumeric("Reds", domain = valores)

mapa2 <- leaflet(base_mapa2) %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
  fillColor = ~pal2(perc_idosos),
  color = ~ifelse(tipo == "favela", "black", "white"),
  weight = 1,
  fillOpacity = ~ifelse(tipo == "favela", 0.9, 0.6),

popup = ~paste0(
  "<b>Nome:</b> ", nome, "<br>",
  "<b>% Idosos:</b> ", round(perc_idosos, 1), "%"
)) %>%
  
  addLegend(pal = pal2, values = valores, title = "% Idosos")

mapa2

# salvar versão interativa 
htmlwidgets::saveWidget(mapa2, "resultados/mapa2.html", selfcontained = FALSE)

# ------------------------
# 8. MAPA ESTÁTICO (PNG)
# ------------------------

mapa2_static <- ggplot() +
  
  # Bairros (base)
  geom_sf(
    data = base_mapa2 %>% dplyr::filter(tipo == "bairro_sem_favela"),
    aes(fill = perc_idosos),
    color = "black",
    size = 0.3
  ) +
  
  # Favelas (contorno mais forte)
  geom_sf(
    data = base_mapa2 %>% dplyr::filter(tipo == "favela"),
    aes(fill = perc_idosos),
    color = "black",
    size = 0.4
  ) +
  
  # Paleta mais suave
  scale_fill_gradient(
    low = "#fde0dd",
    high = "#a50f15",
    name = "% População com 60 anos ou mais",
    breaks = seq(5, 35, 5)
  ) +
  
  labs(
    title = "Proporção de população idosa sobre população total por FCUs e bairro (ex-FCU)",
    caption = "Contornos: bairros (linha preta grossa) e favelas (linha preta fina)"
  ) +
  
  # Tema mais limpo (igual paper)
  theme_void() +
  
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    
    # legenda mais "compacta"
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.4, "cm"),
    
    plot.caption = element_text(size = 8)
  )

ggsave(
  "resultados/mapa2_favela.png",
  mapa2_static,
  width = 10,
  height = 8,
  dpi = 300
)

