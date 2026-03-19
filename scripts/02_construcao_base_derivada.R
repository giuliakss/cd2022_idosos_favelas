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
# 3. Filtrar RJ pelo código do setor
# ------------------------

infos_setores_rj <- infos_setores |>
  dplyr::filter(substr(CD_SETOR, 1, 7) == "3304557")

infos_setores2_rj <- infos_setores2 |>
  dplyr::filter(substr(CD_SETOR, 1, 7) == "3304557")

# ------------------------
# 4. Merge das bases de setores censitários
# ------------------------

base_setores <- infos_setores_rj |>
  dplyr::left_join(infos_setores2_rj, by = "CD_SETOR")

# ------------------------
# 5. Malha territorial (setores + bairros)
# ------------------------

setores_rj <- sf::st_read("dados/brutos/RJ_setores_CD2022/RJ_setores_CD2022.shp") |>
  dplyr::filter(CD_MUN == "3304557")

bairros_rj <- sf::st_read("dados/brutos/RJ_bairros_CD2022/RJ_bairros_CD2022.shp") |>
  dplyr::filter(CD_MUN == "3304557")

# ------------------------
# 6. Join malha territorial com informações dos setores
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

# ------------------------
# 8. Join espacial setor → bairro
# ------------------------

setores_com_bairro <- sf::st_join(base_setores_geo, bairros_rj)

setores_com_bairro <- setores_com_bairro |>
  dplyr::rename(
    NM_BAIRRO = NM_BAIRRO.y,
    CD_BAIRRO = CD_BAIRRO.y   # 👈 ADICIONAR ISSO
  )

setores_com_bairro <- setores_com_bairro %>%
  mutate(
    CD_BAIRRO = as.numeric(CD_BAIRRO)
 )

# ------------------------
# 9. Agregação por bairro (somando todas colunas numéricas)
# ------------------------

# remover colunas que não devem ser somadas (ex: códigos)
colunas_para_somar <- names(setores_com_bairro)[
  grepl("^V", names(setores_com_bairro))]

# ---------Separar info e geometria ---------

# agregação (sem geometria)
base_bairros_info <- setores_com_bairro |>
  sf::st_drop_geometry() |>
  dplyr::group_by(NM_BAIRRO, CD_BAIRRO) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(colunas_para_somar),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# pegar geometria dos bairros
bairros_geo <- bairros_rj |>
  dplyr::select(NM_BAIRRO, geometry)

# juntar info + geometria
base_bairros <- bairros_geo |>
  dplyr::left_join(base_bairros_info, by = "NM_BAIRRO")

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
