
#pegando eleitos por partido
ReadDadosEleicoesMunicipais <- function(file, encoding = "latin1") {
  read.csv(file, header = F, encoding = encoding, sep = ";", stringsAsFactors = F,
           col.names = c("data_geracao", "hora_geracao", "ano_eleicao", "num_turno",
                         "descricao_eleicao", "sigla_uf", "sigla_ue", "codigo_municipio",
                         "nome_municipio", "numero_zona", "codigo_cargo", "numero_cand",
                         "sq_candidato", "nome_candidato", "nome_urna_candidato",
                         "descricao_cargo","cod_sit_cand_superior", "desc_sit_cand_superior",
                         "codigo_sit_candidato", "desc_sit_candidato", "codigo_sit_cand_tot",
                         "desc_sit_cand_tot", "numero_partido", "sigla_partido", "nome_partido",
                         "sequencial_legenda", "nome_coligacao", "composicao_legenda",
                         "total_votos"))
}


eleicoes_pb_2008_file <- "../dados/votacao_candidato_munzona_2008_PB.txt"
eleicoes_pb_2012_file <- "../dados/votacao_candidato_munzona_2012_PB.txt"

res_prefeitos_pb <- ReadDadosEleicoesMunicipais(eleicoes_pb_2008_file) %>%
  bind_rows(ReadDadosEleicoesMunicipais(eleicoes_pb_2012_file)) %>%
  filter(descricao_cargo == "PREFEITO", desc_sit_cand_tot == "ELEITO") %>%
  group_by(ano_eleicao, nome_municipio, nome_candidato, sigla_partido, nome_coligacao) %>%
  distinct()

eleitos_por_partido <- res_prefeitos_pb %>%
  group_by(ano_eleicao, sigla_partido, nome_coligacao, nome_candidato, nome_municipio) %>%
  summarise(n_eleitos_partido = n()) %>%
  mutate(prop_eleitos_partido = n_eleitos_partido / sum(n_eleitos_partido)) %>%
  arrange(desc(ano_eleicao), desc(n_eleitos_partido))

eleitos_por_partido <- eleitos_por_partido %>% select(ano_eleicao, sigla_partido, nome_coligacao, nome_candidato, nome_municipio)

write.table(eleitos_por_partido, "../dados/eleitos_partido.csv", quote = F, row.names = F, sep=";", fileEncoding = "UTF-8")
