# ========================================
# Construção das bases derivadas
# ========================================

# ------------------------
# Pacotes
# ------------------------
library(dplyr)
library(sf)

# ------------------------
# Funções auxiliares
# ------------------------
source("scripts/01_funcoes_auxiliares.R")
source("scripts/00_variaveis.R")

# ------------------------
# 1. Leitura dos dados brutos
# ------------------------

infos_setores <- read.csv(
  "dados/brutos/Agregados_por_setores_cor_ou_raca_BR.csv",
  sep = ";",
  colClasses = c(CD_SETOR = "character")
)

infos_setores2 <- read.csv(
  "dados/brutos/Agregados_por_setores_alfabetizacao_BR.csv",
  sep = ";",
  colClasses = c(CD_setor = "character")
)

# ------------------------
# 2. Ajuste de nomes (CD_SETOR deveria ser maisculo mas veio como minusculo em 1 único arquivo)
# ------------------------

infos_setores2 <- infos_setores2 |>
  dplyr::rename(CD_SETOR = CD_setor)

# ------------------------
# 3. Filtrar cidade do Rio de Janeiro pelo código do setor
# ------------------------

infos_setores_rj <- infos_setores |>
  dplyr::filter(substr(CD_SETOR, 1, 7) == "3304557")

infos_setores2_rj <- infos_setores2 |>
  dplyr::filter(substr(CD_SETOR, 1, 7) == "3304557")

# ------------------------
# 4. Merge das bases de setores censitários para ter infos de alfabetização e cor/raça
# ------------------------

base_setores <- infos_setores_rj |>
  dplyr::left_join(infos_setores2_rj, by = "CD_SETOR")

# ------------------------
# 5. Malha territorial (setores e bairros)
# ------------------------

setores_rj <- sf::st_read("dados/brutos/RJ_setores_CD2022/RJ_setores_CD2022.shp") |>
  dplyr::filter(CD_MUN == "3304557")

bairros_rj <- sf::st_read("dados/brutos/RJ_bairros_CD2022/RJ_bairros_CD2022.shp") |>
  dplyr::filter(CD_MUN == "3304557")

# ------------------------
# 6. Join malha territorial de setores com informações dos setores
# ------------------------

base_setores_geo <- setores_rj |>
  dplyr::left_join(base_setores, by = "CD_SETOR")

# ------------------------
# 7. Tratamento das colunas numéricas
# ------------------------

colunas_para_tratar <- names(base_setores_geo)[
  grepl("^V|^CD_", names(base_setores_geo))
]

base_setores_geo <- tratar_colunas(base_setores_geo, colunas_para_tratar)

base_setores_geo <- base_setores_geo %>%
  filter(CD_SIT <= 3)

# ------------------------
# 8. Agregação por bairro (via CD_BAIRRO)
# ------------------------

colunas_para_somar <- names(base_setores_geo)[
  grepl("^V", names(base_setores_geo))
]

base_bairros_info <- base_setores_geo |>
  sf::st_drop_geometry() |>
  
  tratar_colunas(colunas_para_somar) |>
  
  dplyr::group_by(CD_BAIRRO) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(colunas_para_somar),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

bairros_geo <- bairros_rj |>
  dplyr::select(CD_BAIRRO, NM_BAIRRO, geometry) |>
  mutate(CD_BAIRRO = as.numeric(CD_BAIRRO))

base_bairros <- bairros_geo |>
  dplyr::left_join(base_bairros_info, by = "CD_BAIRRO")


#checks de sanidade 

sum(base_setores_geo$V01387)

sum(base_bairros$V01387)

sum(base_setores_geo$V01387)==sum(base_bairros$V01387)



base_setores_geo %>%
  st_drop_geometry() %>%
  summarise(
    total = sum(across(all_of(vars_total_pop_60_ou_mais)), na.rm = TRUE)
  ) %>%
  pull(total)



# ------------------------
# 10. Salvar bases
# ------------------------

saveRDS(
  base_setores_geo,
  "dados/derivados/base_setores_2022_rj_com_infos.rds"
)

saveRDS(
  base_bairros,
  "dados/derivados/base_bairros_rj_com_infos.rds"
)

# ------------------------
# Fim
# ------------------------