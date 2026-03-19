# ========================================
# Mapas - % Idosos
# ========================================

library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)

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
# 4. MAPA 1 - BAIRROS
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

# setores fora de favela (AJUSTE AQUI)
setores_nao_favela <- base_setores %>%
  filter(CD_TIPO != "1", CD_SIT %in% c(1, 2, 3))

# ------------------------
# 6. Construir base do mapa 2
# ------------------------

# ---- FAVELAS (AJUSTE AQUI)
favelas <- setores_favela %>%
  mutate(
    id_favela = ifelse(is.na(NM_FCU), paste0("favela_", CD_SETOR), NM_FCU)
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
    tipo = "bairro_sem_favela"
  )

# aplicar recorte espacial
bairros_sem_favela <- base_bairros %>%
  select(CD_BAIRRO, NM_BAIRRO, geometry) %>%
  st_difference(favelas_union) %>%
  left_join(bairros_info, by = c("CD_BAIRRO", "NM_BAIRRO")) %>%
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

# salvar versão interativa (AJUSTE AQUI)
htmlwidgets::saveWidget(mapa2, "resultados/mapa2.html", selfcontained = FALSE)

# ------------------------
# 8. MAPA ESTÁTICO (PNG)
# ------------------------

mapa2_static <- ggplot() +
  
  geom_sf(
    data = base_mapa2 %>% dplyr::filter(tipo == "bairro_sem_favela"),
    aes(fill = perc_idosos),
    color = "white",
    size = 0.1
  ) +
  
  geom_sf(
    data = base_mapa2 %>% dplyr::filter(tipo == "favela"),
    aes(fill = perc_idosos),
    color = "black",
    size = 0.3
  ) +
  
  scale_fill_distiller(
    palette = "Reds",
    name = "% Idosos",
    direction = 1
  ) +
  
  theme_minimal() +
  labs(title = "% de idosos - favelas vs restante do bairro") +
  
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

ggsave(
  "resultados/mapa2_favela.png",
  mapa2_static,
  width = 10,
  height = 8,
  dpi = 300
)