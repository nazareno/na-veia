library(dplyr)
library(RMySQL)
u_gestora <- read.csv("../dados/codigo_ugestora.csv", encoding="UTF-8")

#gerando csv de codigo por municipio
get_nome <- function(cd_uges){
  de_mun <- subset(u_gestora, cd_Ugestora == cd_uges)
  list_de_mun <- de_mun$de_Ugestora
  de_mun <- as.character(list_de_mun[1])
  
  nome_mun_prefeitura <- unlist(strsplit(de_mun, "Prefeitura Municipal de "))[2]
  nome_mun_camara <- unlist(strsplit(de_mun, "Câmara Municipal de "))[2]
  if(is.na(nome_mun_prefeitura)){
    nome_mun <- nome_mun_camara
  }else{
    nome_mun <- nome_mun_prefeitura  
  }
  
  return(nome_mun)
}

nomes <- c()
for(i in 1:nrow(u_gestora)){
  nomes[i] <- get_nome(u_gestora$cd_Ugestora[i])
}

u_gestora$municipio <- nomes
u <- u_gestora %>% select(cd_Ugestora, municipio, de_Ugestora) %>% group_by( municipio) %>% arrange(cd_Ugestora) %>% summarize(cd_Ugestora = last(cd_Ugestora), de_Ugestora = last(de_Ugestora))
write.table(u, "../dados/codigo_municipio.csv", quote = F, row.names = F, sep=";", fileEncoding = "UTF-8")