#Regularizar todo según los datos oficiales para hacerles un join posteriormente

#####################################
######## FASE DE EXPLORACIÓN ########
#####################################

library(readr)
library(tidyverse)
library(data.table)
library(arrow)
############# Fact-Run: tabla de hechos. Una fila es un jugador en un torneo

fact_run_ofi <- read_csv("~/PycharmProjects/REG-G/EXTRACCION/fact_run_ofi.csv")
fact_run_noofi <- read_csv("~/PycharmProjects/REG-G/EXTRACCION/fact_run_noofi.csv")

# ID: tienen que ser únicos

fact_run_ofi$ID %>% unique() %>% length() == nrow(fact_run_ofi) #TRUE
fact_run_noofi$ID %>% unique() %>% length() == nrow(fact_run_noofi) #TRUE

# Torneo: checkear las categorias

fact_run_ofi %>%  group_by(Torneo) %>%  summarise(n_players= n())
fact_run_noofi %>%  group_by(Torneo) %>%  summarise(n_players= n())

# Tipo_Torneo: solo hay 2
fact_run_ofi %>%  group_by(Tipo_Torneo) %>%  summarise(n= n())
fact_run_noofi %>%  group_by(Tipo_Torneo) %>%  summarise(n= n())

# Wins, Losses: número asumible, suma consistente
fact_run_ofi %>% select(Wins, Losses) %>% mutate(partidas = Wins + Losses) %>% arrange (-partidas)
fact_run_noofi %>% select(Wins, Losses) %>% mutate(partidas = Wins + Losses) %>% arrange (-partidas)

############# Rounds: una fila es una ronda asociada a un jugador en un torneo, contra un rival y obteniendo un resultado

round_list_ofi <- read_csv("~/PycharmProjects/REG-G/EXTRACCION/round_list_ofi.csv")
round_list_noofi <- read_csv("~/PycharmProjects/REG-G/EXTRACCION/round_list_noofi.csv")

# ID: tienen que coincidir con los de la fact table

round_list_ofi$ID %>% unique() %>% length() == nrow(fact_run_ofi) #TRUE
round_list_noofi$ID %>% unique() %>% length() == nrow(fact_run_noofi) #TRUE
round_list_noofi %>% filter(ID == 20030) %>% nrow == (fact_run_noofi %>% filter(ID == 20030) %>% mutate(n=Wins+Losses) %>% .$n)
sum(fact_run_noofi$Wins) + sum(fact_run_noofi$Losses)
# ROnda: Verificar si las categorias son iguales

round_list_ofi%>% group_by(Ronda) %>% summarise(n = n())
round_list_noofi%>% group_by(Ronda) %>% summarise(n = n()) # Las categorias de rondas eliminatorias son distintas

round_list_noofi = round_list_noofi %>%
  mutate(Ronda = case_when(
    grepl("^T32-", Ronda) ~ "Top 32",
    grepl("^T16-", Ronda) ~ "Top 16",
    grepl("^T8-", Ronda) ~ "Top 8",
    grepl("^T4-", Ronda) ~ "Top 4",
    grepl("^T2-", Ronda) ~ "Finals",
    TRUE ~ Ronda
  ))

round_list_noofi%>% group_by(Ronda) %>% summarise(n = n()) #ahora si coinciden

# Resultado solo se puede ganar perder o asumir un DROP

round_list_ofi %>% .$Resultado %>% table
round_list_ofi <- round_list_ofi %>%
  mutate(Resultado = ifelse(Resultado == "Ongoing", "DROP", Resultado))
round_list_noofi %>% .$Resultado %>% table

############# Team List: Cada fila es un Pokemon, un jugador en un torneo necesita 6 para participar, mínimo 4

team_list_ofi <- read_csv("~/PycharmProjects/REG-G/EXTRACCION/team_list_ofi.csv")
team_list_noofi <- read_csv("~/PycharmProjects/REG-G/EXTRACCION/team_list_noofi.csv")


# Pokemon (muchos problemas jaja)
team_list_ofi%>% group_by(Pokemon) %>% summarise(n = n()) %>% arrange(n) %>% View
team_list_ofi %>%
  filter(grepl("[^a-zA-Z]", Pokemon)) %>%  
  group_by(Pokemon) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% View

team_list_noofi%>% group_by(Pokemon) %>% summarise(n = n()) %>% arrange(n) %>% View
team_list_noofi %>%
  filter(grepl("[^a-zA-Z]", Pokemon)) %>%  
  group_by(Pokemon) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% View

team_list_noofi %>%
  mutate(
    Pokemon = case_when(
      grepl("Rapid Strike Urshifu", Pokemon) ~ "Urshifu-Rapid-Strike",
      grepl("Shadow Rider Calyrex", Pokemon) ~ "Calyrex-Shadow",
      grepl("Ice Rider Calyrex", Pokemon) ~ "Calyrex-Ice",
      grepl("Black Kyurem", Pokemon) ~ "Kyurem-Black",
      grepl("White Kyurem", Pokemon) ~ "Kyurem-White",
      grepl("Origin Forme Palkia", Pokemon) ~ "Palkia-Origin",
      grepl("Origin Forme Dialga", Pokemon) ~ "Dialga-Origin",
      grepl("Dawn Wings Necrozma", Pokemon) ~ "Necrozma-Dawn-Wings",
      grepl("Dusk Mane Necrozma", Pokemon) ~ "Necrozma-Dusk-Mane",
      grepl("Bloodmoon Ursaluna", Pokemon) ~ "Ursaluna-Bloodmoon",
      grepl("Indeedee ♀", Pokemon) ~ "Indeedee-F",
      grepl("Paldean Tauros (Aqua|Blaze) Breed", Pokemon) ~ gsub("Paldean Tauros (Aqua|Blaze) Breed", "Tauros-Paldea-\\1", Pokemon),
      grepl("^(Wellspring|Hearthflame|Cornerstone) Mask Ogerpon", Pokemon) ~ gsub("^(Wellspring|Hearthflame|Cornerstone) Mask Ogerpon", "Ogerpon-\\1", Pokemon),
      grepl("(Landorus|Thundurus|Tornadus|Enamorus) Therian", Pokemon) ~ gsub("(Landorus|Thundurus|Tornadus|Enamorus) Therian", "\\1-Therian", Pokemon),
      grepl("Tatsugiri (Droopy|Stretchy) Form", Pokemon) ~ gsub("Tatsugiri (Droopy|Stretchy) Form", "Tatsugiri-\\1", Pokemon),
      grepl("(Frost|Wash|Heat|Mow|Fan) Rotom", Pokemon) ~ gsub("(Frost|Wash|Heat|Mow|Fan) Rotom", "Rotom-\\1", Pokemon),
      grepl("(Alolan|Hisuian|Galarian) [A-Z][a-z]+", Pokemon) ~ {
        pokemon <- gsub("(Alolan|Hisuian|Galarian) ([A-Z][a-z]+)", "\\2-\\1", Pokemon)
        pokemon <- gsub("Galarian", "Galar", pokemon)
        pokemon <- gsub("Hisuian", "Hisui", pokemon)
        pokemon <- gsub("Alolan", "Alola", pokemon)
        pokemon
      },
      grepl("Ogerpon", Pokemon) & grepl("Hearthflame", Objeto) ~ "Ogerpon-Hearthflame",
      grepl("Ogerpon", Pokemon) & grepl("Wellspring", Objeto) ~ "Ogerpon-Wellspring",
      grepl("Ogerpon", Pokemon) & grepl("Cornerstone", Objeto) ~ "Ogerpon-Cornerstone",
      TRUE ~ Pokemon
    )
  ) -> team_list_noofi

setdiff(team_list_ofi$Pokemon , team_list_noofi$Pokemon) #Todo solucionado

# Objeto

setdiff(team_list_noofi$Objeto , team_list_ofi$Objeto) 

team_list_noofi %>%
  mutate(
    Objeto = case_when(
      grepl("Booster energy", Objeto) ~ "Booster Energy",
      grepl("choice scarf", Objeto) ~ "Choice Scarf",
      grepl("WellSpring Mask", Objeto) ~ "Wellspring Mask",
      grepl("rocky helmet", Objeto) ~ "Rocky Helmet",
      grepl("assault vest", Objeto) ~ "Assault Vest",
      grepl("Life orb", Objeto) ~ "Life Orb",
      grepl("focus sash", Objeto) ~ "Focus Sash",
      grepl("mago Berry", Objeto) ~ "Mago Berry"
      ,
      TRUE ~ Objeto
    )
  ) ->team_list_noofi # Todo ok

# Habilidad

setdiff(team_list_ofi$Habilidad ,team_list_noofi$Habilidad) 
setdiff(team_list_noofi$Habilidad ,team_list_ofi$Habilidad) 
unique(team_list_noofi$Habilidad)
team_list_noofi %>%
  mutate(
    Habilidad = case_when(
      grepl("Mind's Eye", Habilidad) ~ "Mind’s Eye" ,
      grepl("Snow warning", Habilidad) ~ "Snow Warning",
      grepl("Armor tail", Habilidad) ~ "Armor Tail",
      grepl("regenerator", Habilidad) ~ "Regenerator",
      grepl("DRIZZLE", Habilidad) ~ "Drizzle",
      grepl("imposter", Habilidad) ~ "Imposter",
      grepl("Drizzle Level", Habilidad) ~ "Drizzle",
      grepl("Intimidate Level", Habilidad) ~ "Intimidate",
      grepl("Regenerator Level", Habilidad) ~ " Regenerator",
      grepl("Armor Tail Level", Habilidad) ~ "Armor Tail",
      grepl("Imposter Level", Habilidad) ~ "Imposter",
      grepl("Grassy Surge Level", Habilidad) ~ "Grassy Surge",
      grepl("Tera Shift Level", Habilidad) ~ "Tera Shift",
      grepl("Triage Level", Habilidad) ~ "Triage",
      grepl("Unseen Fist Tera Type", Habilidad) ~ "Unseen Fist",
      grepl("transform", Habilidad) ~ "Transform"
      
      ,
      TRUE ~ Habilidad
    )
  ) -> team_list_noofi

team_list_ofi %>%
  mutate(
    Habilidad = case_when(
      grepl("As One (Glastrier)", Habilidad) ~ "As One" ,
      grepl("As One (Spectrier)", Habilidad) ~ "As One"
      ,
      TRUE ~ Habilidad
    )
  ) -> team_list_ofi


# Teratipo

setdiff(team_list_ofi$Teratipo ,team_list_noofi$Teratipo) 
setdiff(team_list_noofi$Teratipo ,team_list_ofi$Teratipo) 

team_list_noofi %>%
  mutate(
    Teratipo = case_when(
      grepl("grass", Teratipo) ~ "Grass" ,
      grepl("psychic", Teratipo) ~ "Psychic",
      grepl("fire", Teratipo) ~ "Fire",
      grepl("water", Teratipo) ~ "Water",
      grepl("steel", Teratipo) ~ "Steel",
      grepl("fairy", Teratipo) ~ "Fairy",
      grepl("Fariy", Teratipo) ~ "Fairy"
      ,
      TRUE ~ Teratipo
    )
  ) -> team_list_noofi #fixed no ofi , falta ofi que tiene unos strings raros en Stellar

# Mov1
setdiff(team_list_ofi$Mov1 ,team_list_noofi$Mov1) 
setdiff(team_list_noofi$Mov1 ,team_list_ofi$Mov1) 


team_list_ofi %>%
  slice(which(rowSums(is.na(select(., 6:9))) > 0)) 

team_list_ofi %>%
  slice(which(rowSums(is.na(select(., 6:9))) > 0)) %>%
  filter(grepl("^Stellar\n-", Teratipo))

team_list_noofi <- team_list_noofi %>%
  mutate(across(-ID, ~ ifelse(is.na(Mov2) & !(grepl("Transform", Mov1, ignore.case = TRUE) | grepl("transform", Mov1, ignore.case = TRUE)), NA, .)))

team_list_ofi %>%
  mutate(
    Mov4 = ifelse(grepl("Stellar\n-", Teratipo),gsub("^Stellar\n- ", "", Teratipo), Mov4),
    Teratipo = ifelse(grepl("Stellar\n-", Teratipo), "Stellar", Teratipo)
  ) ->team_list_ofi #fixea Tera de ofi y aparte el Mov4

# Movimientos

team_list_ofi %>% select(6:9) %>% pivot_longer(everything() , names_to = "nn", values_to = "Movs") %>% select(Movs)

setdiff(team_list_noofi %>% select(6:9) %>% pivot_longer(everything() , names_to = "nn", values_to = "Movs") %>% select(Movs)
        , team_list_ofi %>% select(6:9) %>% pivot_longer(everything() , names_to = "nn", values_to = "Movs") %>% select(Movs)
) 

team_list_noofi %>%
  mutate_at(vars(c("Mov1", "Mov2", "Mov3", "Mov4")), .funs = function(columna) {
    gsub("follow me", "Follow Me", columna) %>%
      gsub("Icicle crash", "Icicle Crash", .) %>%
      gsub("twin beam", "Twin Beam", .) %>%
      gsub("Sword Dance", "Swords Dance", .) %>%
      gsub("Play rough", "Play Rough", .) %>%
      gsub("Ice spinner", "Ice Spinner", .) %>%
      gsub("shelter", "Shelter", .) %>%
      gsub("U-Turn", "U-turn", .) %>%
      gsub("fake out", "Fake Out", .) %>%
      gsub("tranform", "Transform", .) %>%
      gsub("Horn-Leech", "Horn Leech", .)
  }) ->team_list_noofi #fixed todo

########## Los 3 dfs listos
fact_run = rbind(fact_run_ofi,fact_run_noofi)
team_list = rbind(team_list_ofi,team_list_noofi)
round_list = rbind(round_list_ofi,round_list_noofi)


###########################
######### CLUSTER #########
###########################

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Terapagos")$ID)->terapagos_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Lunala")$ID)->lunala_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Zacian")$ID)->zacian_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Zamazenta")$ID)->zamazenta_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Kyogre")$ID)->kyogre_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Groudon")$ID)->groudon_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Calyrex-Shadow")$ID)->calyshadow_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Calyrex-Ice")$ID)->calyice_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Koraidon")$ID)->koraidon_core

team_list %>%
  filter(ID %in% filter(team_list, Pokemon == "Miraidon")$ID)->miraidon_core

jacc_1D <- function(a, b) {
  a <- unlist(a)
  b <- unlist(b)
  intersection <- length(intersect(a, b))
  #union <- length(union(a, b))
  return(1 - (2*intersection) / 16)
}




jacc_2D <-  function(a, b) {
    # Calcula la distancia Jaccard entre cada fila de 'a' y todas las filas de 'b'
    distancias <- apply(a, 1, function(row_a) {
      apply(b, 1, function(row_b) {
        jacc_1D(row_a, row_b)
      })
    })
    
    # Encuentra la distancia mínima para cada fila de 'a'
    dist_min <- apply(distancias, 2, min)
    
    # Calcula la media de las distancias mínimas
    jaccard <- mean(dist_min)
    
    return(jaccard)
  }


jacc_3D <- function(mat) {
  un = unique(mat$ID)
  dist_mat = matrix(0, nrow = length(un), ncol = length(un))
  for (i in 1:(length(un)-1)) {
    for (j in (i+1):length(un)) {
      dist_mat[i,j] = jacc_2D(mat[mat$ID == un[i], -which(names(mat) == "ID")],mat[mat$ID == un[j], -which(names(mat) == "ID")])
      dist_mat[j,i] = dist_mat[i,j]
    }
  }
  return(dist_mat)
}


# Definimos las matrices de distancias
miraidon_mat = jacc_3D(miraidon_core)
koraidon_mat = jacc_3D(koraidon_core)
calyshadow_mat = jacc_3D(calyshadow_core)
calyice_mat = jacc_3D(calyice_core)
kyogre_mat = jacc_3D(kyogre_core)
groudon_mat = jacc_3D(groudon_core)
terapagos_mat = jacc_3D(terapagos_core)
lunala_mat = jacc_3D(lunala_core)
zacian_mat = jacc_3D(zacian_core)
zamazenta_mat = jacc_3D(zamazenta_core)


# Ahora si, clusterizamos con todos
library(dbscan)
library(cluster)
library(factoextra)

##############################################################################################################################
###### Analisis de Miraidon

fviz_nbclust(miraidon_mat, hcut, method = "gap_stat")
hc_miraidon = agnes(as.dist(miraidon_mat), diss = FALSE, method = "weighted")
cluster_miraidon<-cutree(hc_miraidon, 4)

miraidon_core %>%
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_miraidon) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_miraidon) %>%
  group_by(cluster_miraidon, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_miraidon, desc(repeticiones)) %>% View

miraidon_core %>% 
  filter(Pokemon == "Miraidon") %>%
  select(Objeto) %>% 
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>% 
  arrange(desc(repeticiones))

miraidon_core %>%
  filter(Pokemon == "Miraidon") %>% 
  select(Mov1 , Mov2, Mov3, Mov4) %>% 
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>% 
  group_by(nombre) %>% 
  summarise(repeticiones = n()) %>% 
  arrange(desc(repeticiones))


miraidon_core %>%
  group_by(Pokemon) %>% 
  summarise(repeticiones = n()) %>% 
  arrange(desc(repeticiones)) %>% View

miraidon_core %>%
  filter(Pokemon == "Iron Bundle") %>%
  distinct(ID) %>%
  inner_join(miraidon_core, by = "ID") %>% 
  group_by(Pokemon) %>% 
  summarise(repeticiones = n()) %>% 
  arrange(desc(repeticiones)) %>% View

miraidon_core %>% filter(Pokemon %in% c("Ursaluna-Bloodmoon", "Farigiraf")) %>% 
                  group_by(ID) %>% 
                  filter(all(c("Ursaluna-Bloodmoon", "Farigiraf") %in% Pokemon)) %>% 
                  distinct(ID) %>% inner_join(miraidon_core) %>% 
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

miraidon_core %>% filter(Pokemon %in% c("Ursaluna-Bloodmoon", "Farigiraf")) %>% 
  group_by(ID) %>% 
  filter(all(c("Ursaluna-Bloodmoon", "Farigiraf") %in% Pokemon)) %>% 
  distinct(ID)->miraidon_moonaTR_ID


miraidon_core %>%
  filter(Habilidad == "Quark Drive") %>%
  group_by(ID) %>%
  summarise(repeticiones = n()) %>%
  filter(repeticiones > 1) %>%
  distinct(ID) %>%
  inner_join(miraidon_core, by = "ID") %>% 
  group_by(Pokemon) %>% 
  summarise(repeticiones = n()) %>% 
  arrange(desc(repeticiones)) %>% View

miraidon_core %>%
  filter(Habilidad == "Quark Drive") %>%
  group_by(ID) %>%
  summarise(repeticiones = n()) %>%
  filter(repeticiones > 1) %>%
  distinct(ID)->miraidon_quark_ID

intersect(miraidon_moonaTR_ID$ID, miraidon_quark_ID$ID)
miraidon_core %>% 
  filter(ID == 20999)

miraidon_moonaTR_ID %>% 
  filter(ID != 20999)->miraidon_moonaTR_ID

jacc_3D(miraidon_core %>%
         filter(!ID %in% c(miraidon_moonaTR_ID$ID, miraidon_quark_ID$ID)))

cutree(agnes(x=jacc_3D(miraidon_core %>%
                         filter(!ID %in% c(miraidon_moonaTR_ID$ID, miraidon_quark_ID$ID))), diss=FALSE , method = "ward"),7)

miraidon_core %>%
  filter(!ID %in% c(miraidon_moonaTR_ID$ID, miraidon_quark_ID$ID)) %>% .[49:54,]

miraidon_core %>%
  filter(!ID %in% c(miraidon_moonaTR_ID$ID, miraidon_quark_ID$ID)) %>% 
  group_by(Pokemon) %>% 
  summarise(repeticiones = n()) %>% 
  arrange(desc(repeticiones)) %>% View

miraidon_core %>%
  filter(!ID %in% c(miraidon_moonaTR_ID$ID, miraidon_quark_ID$ID)) %>% 
  filter(Pokemon == "Urshifu-Rapid-Strike") %>%
  distinct(ID) %>%
  inner_join(miraidon_core, by = "ID") %>%
  group_by(Pokemon) %>% 
  summarise(repeticiones = n()) %>% 
  arrange(desc(repeticiones)) %>% View

miraidon_core %>%
  filter(!ID %in% c(miraidon_moonaTR_ID$ID, miraidon_quark_ID$ID)) %>% 
  filter(Pokemon == "Urshifu-Rapid-Strike") %>%
  distinct(ID)->miraidon_urshifu_ID


rbind(data.frame(miraidon_moonaTR_ID, arch = rep("MiraiMoona TR", nrow(miraidon_moonaTR_ID))),
data.frame(miraidon_quark_ID, arch = rep("Miraidon Toolbox", nrow(miraidon_quark_ID))),
data.frame(miraidon_urshifu_ID, arch = rep("Miraidon-Urshi", nrow(miraidon_urshifu_ID))))


miraidon_core %>% 
  full_join(rbind(data.frame(miraidon_moonaTR_ID, arch = rep("MiraiMoona TR", nrow(miraidon_moonaTR_ID))),
                  data.frame(miraidon_quark_ID, arch = rep("Miraidon Toolbox", nrow(miraidon_quark_ID))),
                  data.frame(miraidon_urshifu_ID, arch = rep("Miraidon-Urshi", nrow(miraidon_urshifu_ID)))),
             by = "ID") ->miraidon_core
###########################################################################################################
######### Analisis de Koraidon

fviz_nbclust(koraidon_mat, hcut, method = "silhouette")
hc_koraidon = agnes(as.dist(koraidon_mat), diss = FALSE, method = "weighted")
cluster_koraidon<-cutree(hc_koraidon, 3)

koraidon_core %>%
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_koraidon) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_koraidon) %>%
  group_by(cluster_koraidon, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_koraidon, desc(repeticiones)) %>% View

koraidon_core %>%
  filter(Pokemon == "Koraidon") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

koraidon_core %>%
  filter(Pokemon == "Koraidon") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

koraidon_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

koraidon_core %>%
  filter(Pokemon == "Farigiraf") %>%
  distinct(ID) %>%
  inner_join(koraidon_core, by = "ID") %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

koraidon_core %>% 
  filter(Pokemon == "Farigiraf") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

koraidon_core %>%
  filter(Pokemon %in% c("Incineroar", "Farigiraf")) %>%
  group_by(ID) %>%
  filter(all(c("Incineroar", "Farigiraf") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(koraidon_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

koraidon_core %>%
  filter(Pokemon %in% c("Incineroar", "Farigiraf")) %>%
  group_by(ID) %>%
  filter(all(c("Incineroar", "Farigiraf") %in% Pokemon)) %>%
  distinct(ID)->koraidon_balance_ID


koraidon_core %>%
  filter(Pokemon == "Tornadus") %>%
  distinct(ID) %>%
  inner_join(koraidon_core, by = "ID") %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

koraidon_core %>%
  filter(Pokemon == "Tornadus") %>%
  distinct(ID)->koraidon_offense_ID

intersect(koraidon_balance_ID$ID, koraidon_offense_ID$ID)

koraidon_core %>%
  full_join(rbind(data.frame(koraidon_balance_ID, arch = rep("Koraidon Balance", nrow(koraidon_balance_ID))),
                  data.frame(koraidon_offense_ID, arch = rep("Koraidon Offense", nrow(koraidon_offense_ID)))),
             by = "ID") ->koraidon_core



koraidon_core %>%
  filter(is.na(arch)) %>% 
  filter(Pokemon %in% c("Flutter Mane", "Raging Bolt")) %>%
  group_by(ID) %>%
  filter(all(c("Flutter Mane", "Raging Bolt") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(koraidon_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

koraidon_core %>%
  filter(is.na(arch)) %>% 
  filter(Pokemon %in% c("Flutter Mane", "Raging Bolt")) %>%
  group_by(ID) %>%
  filter(all(c("Flutter Mane", "Raging Bolt") %in% Pokemon)) %>%
  distinct(ID) ->koraidon_toolbox_ID

koraidon_core %>%
  mutate(arch = ifelse(ID %in% koraidon_toolbox_ID$ID, "Koraidon Toolbox", arch))->koraidon_core

###################################################################################################################################
######### Analisis de Calyrex-Shadow
######################################################3

fviz_nbclust(calyshadow_mat, hcut, method = "wss")
hc_calyshadow = agnes(as.dist(calyshadow_mat), diss = FALSE, method = "complete")
cluster_calyshadow<-cutree(hc_calyshadow, 2)

calyshadow_core %>%
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_calyshadow) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_calyshadow) %>%
  group_by(cluster_calyshadow, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_calyshadow, desc(repeticiones)) %>% View

calyshadow_core %>%
  filter(Pokemon == "Calyrex-Shadow") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

calyshadow_core %>%
  filter(Pokemon == "Calyrex-Shadow") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))


calyshadow_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

calyshadow_core %>%
  filter(Pokemon == "Indeedee-F") %>%
  distinct(ID) %>%
  inner_join(calyshadow_core, by = "ID") %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

calyshadow_core %>%
  filter(Pokemon == "Indeedee-F") %>%
  distinct(ID) ->calyshadow_psyspam_ID

calyshadow_core %>%
  filter(Pokemon %in% c("Incineroar", "Urshifu-Rapid-Strike")) %>%
  group_by(ID) %>%
  filter(all(c("Incineroar", "Urshifu-Rapid-Strike") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(calyshadow_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

calyshadow_core %>%
  filter(Pokemon %in% c("Incineroar", "Urshifu-Rapid-Strike")) %>%
  group_by(ID) %>%
  filter(all(c("Incineroar", "Urshifu-Rapid-Strike") %in% Pokemon)) %>%
  distinct(ID)->calyshadow_offense_ID

intersect(calyshadow_psyspam_ID$ID, calyshadow_offense_ID$ID)

ifelse(calyshadow_offense_ID$ID %in% intersect(calyshadow_psyspam_ID$ID, calyshadow_offense_ID$ID) , NA, calyshadow_offense_ID$ID)->calyshadow_offense_ID$ID

intersect(calyshadow_psyspam_ID$ID, calyshadow_offense_ID$ID)

calyshadow_offense_ID %>% na.omit() ->calyshadow_offense_ID

calyshadow_core %>% 
  full_join(rbind(data.frame(calyshadow_psyspam_ID, arch = rep("Calyshadow Psyspam", nrow(calyshadow_psyspam_ID))),
                  data.frame(calyshadow_offense_ID, arch = rep("Calyshadow Offense", nrow(calyshadow_offense_ID)))),
             by = "ID") ->calyshadow_core

calyshadow_core %>% 
  filter(is.na(arch)) %>% View
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

jacc_3D(calyshadow_core %>% 
  filter(is.na(arch))) %>% agnes(diss = FALSE, method = "ward") %>% cutree(4)

calyshadow_core %>%
  filter(is.na(arch)) %>% 
  filter(Pokemon %in% c("Ogerpon-Wellspring" , "Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Ogerpon-Wellspring" , "Incineroar") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(calyshadow_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

calyshadow_core %>%
  filter(is.na(arch)) %>% 
  filter(Pokemon %in% c("Ogerpon-Wellspring" , "Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Ogerpon-Wellspring" , "Incineroar") %in% Pokemon)) %>%
  distinct(ID)->calyshadow_setup_ID

intersect(calyshadow_core %>%
            filter(is.na(arch)) %>%
            filter(Pokemon == "Tornadus" | Pokemon == "Whimsicott") %>% 
            distinct(ID) , calyshadow_core %>%
            filter(is.na(arch)) %>%
            filter(Pokemon == "Urshifu" | Pokemon == "Chi-Yu" | Pokemon == "Urshifu-Rapid-Strike") %>%
            distinct(ID)) %>%
  inner_join(calyshadow_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

intersect(calyshadow_core %>%
            filter(is.na(arch)) %>%
            filter(Pokemon == "Tornadus" | Pokemon == "Whimsicott") %>% 
            distinct(ID) , calyshadow_core %>%
            filter(is.na(arch)) %>%
            filter(Pokemon == "Urshifu" | Pokemon == "Chi-Yu" | Pokemon == "Urshifu-Rapid-Strike") %>%
            distinct(ID)) -> calyshadow_hyperoffense_ID

intersect(calyshadow_hyperoffense_ID, calyshadow_setup_ID)

ifelse(calyshadow_setup_ID$ID %in% intersect(calyshadow_hyperoffense_ID$ID, calyshadow_setup_ID$ID), NA, calyshadow_setup_ID$ID)->calyshadow_setup_ID$ID
intersect(calyshadow_hyperoffense_ID$ID, calyshadow_setup_ID$ID)

calyshadow_setup_ID %>% na.omit() ->calyshadow_setup_ID

calyshadow_core %>% 
  mutate(arch = ifelse(ID %in% calyshadow_setup_ID$ID, "Calyshadow Setup", arch))->calyshadow_core

calyshadow_core %>%
  mutate(arch = ifelse(ID %in% calyshadow_hyperoffense_ID$ID, "Calyshadow Hyperoffense", arch))->calyshadow_core
  
#######################################################################################################
fviz_nbclust(calyice_mat, hcut, method = "silhouette")
hc_calyice = agnes(as.dist(calyice_mat), diss = FALSE, method = "ward")
cluster_calyice<-cutree(hc_calyice, 3)

calyice_core %>%
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_calyice) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_calyice) %>%
  group_by(cluster_calyice, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_calyice, desc(repeticiones)) %>% View

calyice_core %>%
  filter(Pokemon == "Calyrex-Ice") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

calyice_core %>%
  filter(Pokemon == "Calyrex-Ice") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

calyice_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

calyice_core %>%
  filter(Pokemon %in% c("Pelipper", "Urshifu-Rapid-Strike")) %>%
  group_by(ID) %>%
  filter(all(c("Pelipper", "Urshifu-Rapid-Strike") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(calyice_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View


calyice_core %>%
  filter(Pokemon %in% c("Pelipper", "Urshifu-Rapid-Strike")) %>%
  group_by(ID) %>%
  filter(all(c("Pelipper", "Urshifu-Rapid-Strike") %in% Pokemon)) %>%
  distinct(ID) ->calyice_rain_ID

calyice_core %>%
  filter(Pokemon == "Ursaluna-Bloodmoon" | Pokemon == "Torkoal" | Pokemon == "Ursaluna") %>%
  distinct(ID) %>%
  inner_join(calyice_core, by = "ID") %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

calyice_core %>%
  filter(Pokemon == "Ursaluna-Bloodmoon" | Pokemon == "Torkoal" | Pokemon == "Ursaluna") %>%
  distinct(ID)->calyice_slowoffense_ID






calyice_core %>%
  filter(!(ID %in% c(calyice_core %>%
                       filter(Pokemon %in% c("Pelipper", "Urshifu-Rapid-Strike")) %>%
                       group_by(ID) %>%
                       filter(all(c("Pelipper", "Urshifu-Rapid-Strike") %in% Pokemon)) %>%
                       distinct(ID) %>% .$ID))) %>% 
  filter(Pokemon %in% c("Amoonguss", "Incineroar")) %>% 
  group_by(ID) %>%
  filter(all(c("Amoonguss", "Incineroar") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(calyice_core) %>%
  filter(!(ID %in% c(calyice_core %>%
                       filter(Pokemon == "Ursaluna-Bloodmoon" | Pokemon == "Torkoal" | Pokemon == "Ursaluna") %>%
                       distinct(ID)  %>% .$ID))) %>% 
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View


calyice_core %>%
  filter(!(ID %in% c(calyice_core %>%
                       filter(Pokemon %in% c("Pelipper", "Urshifu-Rapid-Strike")) %>%
                       group_by(ID) %>%
                       filter(all(c("Pelipper", "Urshifu-Rapid-Strike") %in% Pokemon)) %>%
                       distinct(ID) %>% .$ID))) %>% 
  filter(Pokemon %in% c("Amoonguss", "Incineroar")) %>% 
  group_by(ID) %>%
  filter(all(c("Amoonguss", "Incineroar") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(calyice_core) %>%
  filter(!(ID %in% c(calyice_core %>%
                       filter(Pokemon == "Ursaluna-Bloodmoon" | Pokemon == "Torkoal" | Pokemon == "Ursaluna") %>%
                       distinct(ID)  %>% .$ID))) %>% distinct(ID)->calyice_balanced_ID
  
intersect(calyice_rain_ID$ID, calyice_balanced_ID$ID)
intersect(calyice_rain_ID$ID, calyice_slowoffense_ID$ID)
intersect(calyice_balanced_ID$ID, calyice_slowoffense_ID$ID)

ifelse(calyice_rain_ID$ID %in% intersect(calyice_rain_ID$ID, calyice_slowoffense_ID$ID), NA, calyice_rain_ID$ID)->calyice_rain_ID$ID
calyice_rain_ID %>% na.omit() ->calyice_rain_ID

calyice_core %>% 
  full_join(rbind(data.frame(calyice_rain_ID, arch = rep("Calyice Rain", nrow(calyice_rain_ID))),
                  data.frame(calyice_slowoffense_ID, arch = rep("Calyice Slow Offense", nrow(calyice_slowoffense_ID))),
                  data.frame(calyice_balanced_ID, arch = rep("Calyice Balanced", nrow(calyice_balanced_ID)))),
             by = "ID") ->calyice_core


#############################################################################################################################
fviz_nbclust(kyogre_mat, hcut, method = "silhouette")
hc_kyogre = agnes(x=kyogre_mat**2, diss=FALSE , method = "ward")
cluster_kyogre<-cutree(hc_kyogre, 3)

kyogre_core %>% 
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_kyogre) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_kyogre) %>%
  group_by(cluster_kyogre, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_kyogre, desc(repeticiones)) %>% View

kyogre_core %>%
  filter(Pokemon == "Kyogre") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))


kyogre_core %>%
  filter(Pokemon == "Kyogre") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

kyogre_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

kyogre_core %>%
  group_by(ID) %>% 
  filter(Pokemon != "Tornadus") %>% 
  group_by(ID) %>%
  summarise(n = n()) %>% 
  filter(n == 6) %>% 
  distinct(ID) %>% 
  inner_join(kyogre_core) %>% 
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

kyogre_core %>%
  filter(Pokemon == "Kyogre" & (Mov1 == "Calm Mind"| Mov2 == "Calm Mind"| Mov3 == "Calm Mind"| Mov4 == "Calm Mind")) 



kyogre_core %>% 
  filter(Pokemon == "Urshifu-Rapid-Strike") %>%
  distinct(ID) %>%
  inner_join(kyogre_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

kyogre_core %>% 
  filter(Pokemon == "Urshifu-Rapid-Strike") %>%
  distinct(ID)->kyogre_rainoffense_ID
  


kyogre_core %>% 
  filter(!ID %in% c(kyogre_core %>%
                      filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                      distinct(ID) %>% .$ID)) %>% 
  filter(Pokemon =="Incineroar") %>% 
  distinct(ID) %>%
  inner_join(kyogre_core) %>% 
  filter(Pokemon == "Rillaboom" | Pokemon == "Amoonguss" | Pokemon == "Tsareena" | Pokemon == "Ogerpon-Wellspring") %>%
  distinct(ID) %>%
  inner_join(kyogre_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

kyogre_core %>% 
  filter(!ID %in% c(kyogre_core %>%
                      filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                      distinct(ID) %>% .$ID)) %>% 
  filter(Pokemon =="Incineroar") %>% 
  distinct(ID) %>%
  inner_join(kyogre_core) %>% 
  filter(Pokemon == "Rillaboom" | Pokemon == "Amoonguss" | Pokemon == "Tsareena" | Pokemon == "Ogerpon-Wellspring") %>%
  distinct(ID)->kyogre_FWG_ID


kyogre_core %>% 
  filter(!ID %in% union(c(kyogre_core %>%
                      filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                      distinct(ID) %>% .$ID), c(kyogre_core %>% 
                                                   filter(!ID %in% c(kyogre_core %>%
                                                                       filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                                                                       distinct(ID) %>% .$ID)) %>% 
                                                   filter(Pokemon =="Incineroar") %>% 
                                                   distinct(ID) %>%
                                                   inner_join(kyogre_core) %>% 
                                                   filter(Pokemon == "Rillaboom" | Pokemon == "Amoonguss" | Pokemon == "Tsareena" | Pokemon == "Ogerpon-Wellspring") %>%
                                                   distinct(ID) %>% .$ID))) %>% 
  filter(Pokemon == "Urshifu") %>% 
  distinct(ID) %>%
  inner_join(kyogre_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

kyogre_core %>% 
  filter(!ID %in% union(c(kyogre_core %>%
                            filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                            distinct(ID) %>% .$ID), c(kyogre_core %>% 
                                                        filter(!ID %in% c(kyogre_core %>%
                                                                            filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                                                                            distinct(ID) %>% .$ID)) %>% 
                                                        filter(Pokemon =="Incineroar") %>% 
                                                        distinct(ID) %>%
                                                        inner_join(kyogre_core) %>% 
                                                        filter(Pokemon == "Rillaboom" | Pokemon == "Amoonguss" | Pokemon == "Tsareena" | Pokemon == "Ogerpon-Wellspring") %>%
                                                        distinct(ID) %>% .$ID))) %>% 
  filter(Pokemon == "Urshifu") %>% 
  distinct(ID)->kyogre_ho_ID

kyogre_core %>% 
  filter(!ID %in% union(c(kyogre_core %>%
                            filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                            distinct(ID) %>% .$ID), c(kyogre_core %>% 
                                                        filter(!ID %in% c(kyogre_core %>%
                                                                            filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                                                                            distinct(ID) %>% .$ID)) %>% 
                                                        filter(Pokemon =="Incineroar") %>% 
                                                        distinct(ID) %>%
                                                        inner_join(kyogre_core) %>% 
                                                        filter(Pokemon == "Rillaboom" | Pokemon == "Amoonguss" | Pokemon == "Tsareena" | Pokemon == "Ogerpon-Wellspring") %>%
                                                        distinct(ID) %>% .$ID))) %>% 
  filter(Pokemon == "Farigiraf") %>% 
  distinct(ID) %>%
  inner_join(kyogre_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

kyogre_core %>% 
  filter(!ID %in% union(c(kyogre_core %>%
                            filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                            distinct(ID) %>% .$ID), c(kyogre_core %>% 
                                                        filter(!ID %in% c(kyogre_core %>%
                                                                            filter(Pokemon == "Urshifu-Rapid-Strike") %>%
                                                                            distinct(ID) %>% .$ID)) %>% 
                                                        filter(Pokemon =="Incineroar") %>% 
                                                        distinct(ID) %>%
                                                        inner_join(kyogre_core) %>% 
                                                        filter(Pokemon == "Rillaboom" | Pokemon == "Amoonguss" | Pokemon == "Tsareena" | Pokemon == "Ogerpon-Wellspring") %>%
                                                        distinct(ID) %>% .$ID))) %>% 
  filter(Pokemon == "Farigiraf") %>% 
  distinct(ID) ->kyogre_dobletempo_ID

intersect(kyogre_ho_ID$ID, kyogre_dobletempo_ID$ID)
intersect(kyogre_ho_ID$ID, kyogre_FWG_ID$ID)
intersect(kyogre_dobletempo_ID$ID, kyogre_FWG_ID$ID)
intersect(kyogre_rainoffense_ID$ID, kyogre_ho_ID$ID)
intersect(kyogre_rainoffense_ID$ID, kyogre_dobletempo_ID$ID)
intersect(kyogre_rainoffense_ID$ID, kyogre_FWG_ID$ID)

ifelse(kyogre_ho_ID$ID %in% intersect(kyogre_ho_ID$ID, kyogre_dobletempo_ID$ID), NA, kyogre_ho_ID$ID)->kyogre_ho_ID$ID
kyogre_ho_ID %>% na.omit() ->kyogre_ho_ID
kyogre_core %>% 
  full_join(rbind(data.frame(kyogre_ho_ID, arch = rep("Kyogre Hyper Offense", nrow(kyogre_ho_ID))),
                  data.frame(kyogre_dobletempo_ID, arch = rep("Kyogre Doble Tempo", nrow(kyogre_dobletempo_ID))),
                  data.frame(kyogre_FWG_ID, arch = rep("Kyogre FWG", nrow(kyogre_FWG_ID))),
                  data.frame(kyogre_rainoffense_ID, arch = rep("Kyogre Rain Offense", nrow(kyogre_rainoffense_ID)))),
             by = "ID") ->kyogre_core

###################################################################################################################################
######### Analisis de Groudon
######################################################3

fviz_nbclust(groudon_mat, hcut, method = "silhouette")
hc_groudon = agnes(as.dist(groudon_mat), diss = FALSE, method = "ward")
cluster_groudon<-cutree(hc_groudon, 2)

groudon_core %>% 
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_groudon) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_groudon) %>%
  group_by(cluster_groudon, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_groudon, desc(repeticiones)) %>% View

groudon_core %>%
  filter(Pokemon == "Groudon") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

groudon_core %>%
  filter(Pokemon == "Groudon") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View


groudon_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

groudon_core %>%
  filter(Pokemon == "Flutter Mane") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

groudon_core %>%
  filter(Pokemon == "Flutter Mane") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

groudon_core %>% 
  filter(Pokemon == "Ogerpon-Hearthflame") %>%
  distinct(ID) %>%
  inner_join(groudon_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View
  

groudon_core %>% 
  filter(Pokemon %in% c("Flutter Mane" ,"Incineroar" )) %>%
  group_by(ID) %>%
  filter(all(c("Flutter Mane" ,"Incineroar" ) %in% Pokemon)) %>%
  distinct(ID) %>% 
  inner_join(groudon_core) %>%
  filter(Pokemon == "Raging Bolt" | Pokemon == "Walking Wake") %>%
  distinct(ID) %>%
  inner_join(groudon_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

groudon_core %>% 
  filter(Pokemon %in% c("Flutter Mane" ,"Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Flutter Mane" ,"Incineroar" ,"Raging Bolt") %in% Pokemon)) %>%
  distinct(ID) %>% 
  inner_join(groudon_core) %>%
  filter(Pokemon == "Raging Bolt" | Pokemon == "Walking Wake") %>%
  distinct(ID) -> groudon_toolbox_ID


groudon_core %>%
  filter(!ID %in% c(groudon_toolbox_ID$ID)) %>%
  filter(Habilidad == "Chlorophyll" | Pokemon == "Chi-Yu" | Pokemon == "Ogerpon-Hearthflame" | Pokemon == "Entei") %>%
  distinct(ID) %>%
  inner_join(groudon_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

groudon_core %>%
  filter(!ID %in% c(groudon_toolbox_ID$ID)) %>%
  filter(Habilidad == "Chlorophyll" | Pokemon == "Chi-Yu" | Pokemon == "Ogerpon-Hearthflame" | Pokemon == "Entei") %>%
  distinct(ID)->groudon_sun_ID

intersect(groudon_sun_ID$ID, groudon_toolbox_ID$ID)

groudon_core %>% 
  full_join(rbind(data.frame(groudon_sun_ID, arch = rep("Groudon Sun", nrow(groudon_sun_ID))),
                  data.frame(groudon_toolbox_ID, arch = rep("Groudon Toolbox", nrow(groudon_toolbox_ID))))) %>% .$arch %>% table/6



#####################################################################################################################################
######### Analisis de Terapagos
######################################################3

fviz_nbclust(terapagos_mat, hcut, method = "gap_stat")
hc_terapagos = agnes(as.dist(terapagos_mat), diss = FALSE, method = "weighted")

cluster_terapagos<-cutree(hc_terapagos, 3)

terapagos_core %>% 
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_terapagos) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_terapagos) %>%
  group_by(cluster_terapagos, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_terapagos, desc(repeticiones)) %>% View

terapagos_core %>%
  filter(Pokemon == "Terapagos") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))


terapagos_core %>%
  filter(Pokemon == "Terapagos") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

terapagos_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

terapagos_core %>%
  filter(Pokemon == "Terapagos" & (Objeto == "Choice Specs" )) %>% 
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

terapagos_core %>%
  filter(Pokemon == "Terapagos" & (Objeto == "Choice Specs" )) %>% 
  distinct(ID) ->terapagos_specs_ID



terapagos_core %>%
  filter(!ID %in% c(terapagos_core %>%
                      filter(Pokemon == "Terapagos" & (Objeto == "Choice Specs" )) %>% 
                      distinct(ID) %>% .$ID)) %>%
  filter(Pokemon == "Terapagos") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))




terapagos_core %>%
  filter(Pokemon == "Terapagos" & (Mov1 == "Calm Mind"| Mov2 == "Calm Mind"| Mov3 == "Calm Mind"| Mov4 == "Calm Mind")) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View



terapagos_core %>%
  filter(Pokemon == "Terapagos" & (Mov1 == "Calm Mind"| Mov2 == "Calm Mind"| Mov3 == "Calm Mind"| Mov4 == "Calm Mind")) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>% 
  filter(Pokemon %in% c("Urshifu-Rapid-Strike" ,"Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Urshifu-Rapid-Strike" ,"Incineroar") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  filter(Pokemon == "Rillaboom" | Pokemon == "Amoonguss") %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

terapagos_core %>%
  filter(Pokemon == "Terapagos" & (Mov1 == "Calm Mind"| Mov2 == "Calm Mind"| Mov3 == "Calm Mind"| Mov4 == "Calm Mind")) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>% 
  filter(Pokemon %in% c("Urshifu-Rapid-Strike" ,"Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Urshifu-Rapid-Strike" ,"Incineroar") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  filter(Pokemon == "Rillaboom" | Pokemon == "Amoonguss") %>%
  distinct(ID)->terapagos_fwg_ID


terapagos_core %>%
  filter(Pokemon == "Terapagos" & (Mov1 == "Calm Mind"| Mov2 == "Calm Mind"| Mov3 == "Calm Mind"| Mov4 == "Calm Mind")) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>% 
  filter(Pokemon %in% c("Incineroar", "Ogerpon-Wellspring")) %>%
  group_by(ID) %>%
  filter(all(c("Incineroar", "Ogerpon-Wellspring") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

terapagos_core %>%
  filter(Pokemon == "Terapagos" & (Mov1 == "Calm Mind"| Mov2 == "Calm Mind"| Mov3 == "Calm Mind"| Mov4 == "Calm Mind")) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>% 
  filter(Pokemon %in% c("Incineroar", "Ogerpon-Wellspring")) %>%
  group_by(ID) %>%
  filter(all(c("Incineroar", "Ogerpon-Wellspring") %in% Pokemon)) %>%
  distinct(ID)->terapagos_ogerpon_ID

union(terapagos_fwg_ID$ID, terapagos_ogerpon_ID$ID) %>% union(terapagos_specs_ID$ID)

intersect(terapagos_fwg_ID$ID, terapagos_ogerpon_ID$ID)
intersect(terapagos_fwg_ID$ID, terapagos_specs_ID$ID)
intersect(terapagos_ogerpon_ID$ID, terapagos_specs_ID$ID)

jacc_3D(terapagos_core %>% 
          filter(!ID %in% c(union(terapagos_fwg_ID$ID, terapagos_ogerpon_ID$ID) %>% union(terapagos_specs_ID$ID)))) %>% 
  as.dist() %>% agnes(diss = TRUE, method = "weighted") %>% cutree(3)

terapagos_core %>% 
  filter(!ID %in% c(union(terapagos_fwg_ID$ID, terapagos_ogerpon_ID$ID) %>% union(terapagos_specs_ID$ID))) %>% 
  filter(Pokemon == "Tornadus") %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  filter(Pokemon == "Urshifu"| Pokemon == "Urshifu-Rapid-Strike"| Pokemon == "Flutter Mane" | Pokemon == "Chi-Yu" | Pokemon == "Chien-Pao") %>%
  distinct(ID) %>% 
  inner_join(terapagos_core) %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View




terapagos_core %>% 
  filter(!ID %in% c(union(terapagos_fwg_ID$ID, terapagos_ogerpon_ID$ID) %>% union(terapagos_specs_ID$ID))) %>% 
  filter(Pokemon == "Tornadus") %>%
  distinct(ID) %>%
  inner_join(terapagos_core) %>%
  filter(Pokemon == "Urshifu"| Pokemon == "Urshifu-Rapid-Strike"| Pokemon == "Flutter Mane" | Pokemon == "Chi-Yu" | Pokemon == "Chien-Pao") %>%
  distinct(ID) %>% 
  inner_join(terapagos_core) %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  distinct(ID) ->terapagos_tornadus_ID

terapagos_ogerpon_ID %>% 
  filter(!ID == 10659)->terapagos_ogerpon_ID

terapagos_core %>%
  full_join(rbind(data.frame(terapagos_fwg_ID, arch = rep("Terapagos FWG", nrow(terapagos_fwg_ID))),
                  data.frame(terapagos_ogerpon_ID, arch = rep("Terapagos Ogerpon", nrow(terapagos_ogerpon_ID))),
                  data.frame(terapagos_specs_ID, arch = rep("Terapagos Specs", nrow(terapagos_specs_ID))),
                  data.frame(terapagos_tornadus_ID, arch = rep("Terapagos TW Offense", nrow(terapagos_tornadus_ID)))),
             by = "ID") ->terapagos_core


terapagos_core %>% .$arch %>% table/6

##############################################################################################3
######### Analisis de Lunala
######################################################3

fviz_nbclust(lunala_mat, hcut, method = "wss")
hc_lunala = agnes(as.dist(lunala_mat), diss = FALSE, method = "complete")
cluster_lunala<-cutree(hc_lunala, 5)

lunala_core %>% 
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_lunala) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_lunala) %>%
  group_by(cluster_lunala, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_lunala, desc(repeticiones)) %>% View

lunala_core %>%
  filter(Pokemon == "Lunala") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

lunala_core %>%
  filter(Pokemon == "Lunala") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

lunala_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

lunala_core %>%
  filter(Pokemon %in% c("Incineroar", "Amoonguss")) %>%
  group_by(ID) %>%
  filter(all(c("Incineroar", "Amoonguss") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(lunala_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

lunala_core %>%
  filter(Pokemon %in% c("Incineroar", "Amoonguss")) %>%
  group_by(ID) %>%
  filter(all(c("Incineroar", "Amoonguss") %in% Pokemon)) %>%
  distinct(ID)->lunala_balance_ID


lunala_core %>%
  filter(Pokemon == "Indeedee-F") %>% 
  distinct(ID) %>%
  inner_join(lunala_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

lunala_core %>%
  filter(Pokemon == "Indeedee-F") %>% 
  distinct(ID)->lunala_psyspam_ID

intersect(lunala_balance_ID$ID, lunala_psyspam_ID$ID)

lunala_core %>%
  filter(!ID %in% c(lunala_balance_ID$ID, lunala_psyspam_ID$ID)) %>% 
  filter(Pokemon == "Kingambit" | Pokemon == "Urshifu" | Pokemon == "Ogerpon-Hearthflame") %>% 
  distinct(ID) %>%
  inner_join(lunala_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

lunala_core %>%
  filter(!ID %in% c(lunala_balance_ID$ID, lunala_psyspam_ID$ID)) %>% 
  filter(Pokemon == "Kingambit" | Pokemon == "Urshifu" | Pokemon == "Ogerpon-Hearthflame") %>% 
  distinct(ID)->lunala_ad_offense_ID

lunala_core %>%
  filter(!ID %in% c(lunala_balance_ID$ID, lunala_psyspam_ID$ID ,lunala_ad_offense_ID$ID)) %>% 
  distinct(ID) %>%
  inner_join(lunala_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

intersect(lunala_balance_ID$ID, lunala_ad_offense_ID$ID)
intersect(lunala_balance_ID$ID, lunala_psyspam_ID$ID)
intersect(lunala_ad_offense_ID$ID, lunala_psyspam_ID$ID)

lunala_balance_ID %>% 
  filter(!ID == 20952)->lunala_balance_ID

lunala_core %>%
  full_join(rbind(data.frame(lunala_balance_ID, arch = rep("Lunala Balance", nrow(lunala_balance_ID))),
                  data.frame(lunala_psyspam_ID, arch = rep("Lunala Psyspam", nrow(lunala_psyspam_ID))),
                  data.frame(lunala_ad_offense_ID, arch = rep("Lunala AD Offense", nrow(lunala_ad_offense_ID)))),
             by = "ID") ->lunala_core

############################################################################
######### Analisis de Zacian
######################################################3

fviz_nbclust(zacian_mat, hcut, method = "gap_stat")
hc_zacian = agnes(as.dist(zacian_mat), diss = FALSE, method = "weighted")
cluster_zacian<-cutree(hc_zacian, 5)

zacian_core %>% 
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_zacian) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_zacian) %>%
  group_by(cluster_zacian, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_zacian, desc(repeticiones)) %>% View

zacian_core %>%
  filter(Pokemon == "Zacian") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

zacian_core %>%
  filter(Pokemon == "Zacian") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

zacian_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

zacian_core %>% 
  filter(Pokemon == "Pelipper") %>% 
  distinct(ID) %>%
  inner_join(zacian_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

zacian_core %>%
  filter(Pokemon %in% c("Pelipper" ,"Urshifu-Rapid-Strike")) %>%
  group_by(ID) %>%
  filter(all(c("Pelipper" ,"Urshifu-Rapid-Strike") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(zacian_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

zacian_core %>%
  filter(Pokemon %in% c("Pelipper" ,"Urshifu-Rapid-Strike")) %>%
  group_by(ID) %>%
  filter(all(c("Pelipper" ,"Urshifu-Rapid-Strike") %in% Pokemon)) %>%
  distinct(ID)->zacian_rain_ID

zacian_core %>% 
  filter(!ID %in% zacian_rain_ID$ID) %>%
  filter(Pokemon == "Chien-Pao") %>%
  distinct(ID) %>%
  inner_join(zacian_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View 

zacian_core %>% 
  filter(!ID %in% zacian_rain_ID$ID) %>%
  filter(Pokemon == "Chien-Pao") %>%
  distinct(ID) ->zacian_pao_ID

zacian_core %>% 
  filter(!ID %in% c(zacian_rain_ID$ID, zacian_pao_ID$ID)) %>%
  filter(Mov1 == "Tailwind" | Mov2 == "Tailwind" | Mov3 == "Tailwind" | Mov4 == "Tailwind") %>%
  distinct(ID) %>%
  inner_join(zacian_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

zacian_core %>% 
  filter(!ID %in% c(zacian_rain_ID$ID, zacian_pao_ID$ID)) %>%
  filter(Mov1 == "Tailwind" | Mov2 == "Tailwind" | Mov3 == "Tailwind" | Mov4 == "Tailwind") %>%
  distinct(ID) ->zacian_tw_ID

zacian_core %>% 
  full_join( rbind(data.frame(zacian_rain_ID, arch = rep("Zacian Rain", nrow(zacian_rain_ID))),
                   data.frame(zacian_pao_ID, arch = rep("Zacian Pao", nrow(zacian_pao_ID))),
                   data.frame(zacian_tw_ID, arch = rep("Zacian TW", nrow(zacian_tw_ID)))
                   ),
             by = "ID") ->zacian_core

#######################################################################################################
######### Analisis de Zamazenta
#######################################################################################

fviz_nbclust(zamazenta_mat, hcut, method = "silhouette")
hc_zamazenta = agnes(as.dist(zamazenta_mat), diss = FALSE, method = "ward")
cluster_zamazenta<-cutree(hc_zamazenta, 2)

zamazenta_core %>% 
  group_by(ID) %>% 
  mutate(rn = row_number()) %>% 
  ungroup() %>% 
  select(ID, rn, everything()) %>% 
  pivot_wider(
    id_cols = ID,
    names_from = rn,
    values_from = c(Pokemon, Objeto, Habilidad, Teratipo, Mov1, Mov2, Mov3, Mov4),
    names_prefix = "",
    values_fill = NA
  ) %>% cbind(cluster_zamazenta) %>% select(2,3,4,5,6,7,50) %>% gather(key = "columna", value = "nombre", -cluster_zamazenta) %>%
  group_by(cluster_zamazenta, nombre) %>%
  summarise(repeticiones = n())%>%
  arrange(cluster_zamazenta, desc(repeticiones)) %>% View

zamazenta_core %>%
  filter(Pokemon == "Zamazenta") %>%
  select(Objeto) %>%
  group_by(Objeto) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

zamazenta_core %>%
  filter(Pokemon == "Zamazenta") %>%
  select(Mov1 , Mov2, Mov3, Mov4) %>%
  pivot_longer(everything(), names_to = "columna", values_to = "nombre") %>%
  group_by(nombre) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones))

zamazenta_core %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>% View

zamazenta_core %>% 
  filter(Pokemon %in% c("Pelipper" ,"Urshifu-Rapid-Strike")) %>%
  group_by(ID) %>%
  filter(all(c("Pelipper" ,"Urshifu-Rapid-Strike") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(zamazenta_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

zamazenta_core %>% 
  filter(Pokemon %in% c("Pelipper" ,"Urshifu-Rapid-Strike")) %>%
  group_by(ID) %>%
  filter(all(c("Pelipper" ,"Urshifu-Rapid-Strike") %in% Pokemon)) %>%
  distinct(ID)->zamazenta_rain_ID


zamazenta_core %>% 
  filter(!ID %in% zamazenta_rain_ID$ID) %>%
  filter(Pokemon %in% c("Amoonguss" ,"Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Amoonguss" ,"Incineroar") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(zamazenta_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

zamazenta_core %>% 
  filter(!ID %in% zamazenta_rain_ID$ID) %>% 
  filter(Pokemon %in% c("Amoonguss" ,"Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Amoonguss" ,"Incineroar") %in% Pokemon)) %>%
  distinct(ID) ->zamazenta_balance_ID

zamazenta_core %>% 
  filter(!ID %in% c(zamazenta_rain_ID$ID , zamazenta_balance_ID$ID)) %>%
  filter(Pokemon %in% c("Rillaboom" ,"Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Rillaboom" ,"Incineroar") %in% Pokemon)) %>%
  distinct(ID) %>%
  inner_join(zamazenta_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View

zamazenta_core %>% 
  filter(!ID %in% c(zamazenta_rain_ID$ID , zamazenta_balance_ID$ID)) %>%
  filter(Pokemon %in% c("Rillaboom" ,"Incineroar")) %>%
  group_by(ID) %>%
  filter(all(c("Rillaboom" ,"Incineroar") %in% Pokemon)) %>%
  distinct(ID) ->zamazenta_fwg_ID

zamazenta_core %>% 
  filter(!ID %in% c(zamazenta_balance_ID$ID , zamazenta_fwg_ID$ID, zamazenta_fwg_ID$ID)) %>% 
  filter(Pokemon == "Ting-Lu") %>% 
  distinct(ID) %>%
  inner_join(zamazenta_core) %>%
  group_by(Pokemon) %>%
  summarise(repeticiones = n()) %>%
  arrange(desc(repeticiones)) %>%
  View
  
zamazenta_core %>% 
  filter(!ID %in% c(zamazenta_balance_ID$ID , zamazenta_fwg_ID$ID, zamazenta_fwg_ID$ID)) %>% 
  filter(Pokemon == "Ting-Lu") %>% 
  distinct(ID) ->zamazenta_tinglu_ID  

intersect(zamazenta_balance_ID$ID, zamazenta_fwg_ID$ID)
intersect(zamazenta_balance_ID$ID, zamazenta_tinglu_ID$ID)
intersect(zamazenta_fwg_ID$ID, zamazenta_tinglu_ID$ID)
intersect(zamazenta_rain_ID$ID, zamazenta_balance_ID$ID)
intersect(zamazenta_rain_ID$ID, zamazenta_fwg_ID$ID)
intersect(zamazenta_rain_ID$ID, zamazenta_tinglu_ID$ID)

zamazenta_rain_ID %>% 
  filter(!ID == 10232)->zamazenta_rain_ID

zamazenta_rain_ID %>% 
  na.omit ->zamazenta_rain_ID

zamazenta_core %>%
  full_join(rbind(data.frame(zamazenta_rain_ID, arch = rep("Zamazenta Rain", nrow(zamazenta_rain_ID))),
                  data.frame(zamazenta_balance_ID, arch = rep("Zamazenta Balance", nrow(zamazenta_balance_ID))),
                  data.frame(zamazenta_fwg_ID, arch = rep("Zamazenta FWG", nrow(zamazenta_fwg_ID))),
                  data.frame(zamazenta_tinglu_ID, arch = rep("Zamazenta Tinglu", nrow(zamazenta_tinglu_ID)))
                  ),
             by = "ID") ->zamazenta_core
#############################################################################################################################  
  
calyshadow_core %>% 
  mutate(Habilidad = ifelse(Habilidad == "As One (Spectrier)", "As One", Habilidad)) %>% 
  mutate(Habilidad = ifelse(Habilidad == "Grim Neigh", "As One", Habilidad)) ->calyshadow_core
  
  
calyice_core %>% 
  mutate(Habilidad = ifelse(Habilidad == "As One (Glastrier)", "As One", Habilidad)) %>% 
  mutate(Habilidad = ifelse(Habilidad == "Unnerve", "As One", Habilidad))->calyice_core

#######################################################################################################

bind_rows(groudon_core, 
          terapagos_core, 
          lunala_core, 
          zacian_core, 
          zamazenta_core, 
          calyshadow_core, 
          calyice_core,
          kyogre_core,
          miraidon_core,
          koraidon_core) %>% 
  arrange(ID) %>% select(ID, arch) %>% distinct() ->archetypes
  
left_join(team_list, archetypes, by = "ID") %>% 
  write_csv("team_archetypes.csv")
