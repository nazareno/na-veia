ler_empenhos_brutos = function(arquivo_empenhos = "dados/empenhos_por_municipio.csv", 
                               arquivo_municipios = "dados/dados_municipios.csv"){
    require(readr)
    library(dplyr, warn.conflicts = FALSE)
    empenhos_brutos = read_csv(arquivo_empenhos, 
                        col_types = cols(.default = col_character(), 
                                         vl_Empenhos = col_double(), 
                                         ano_eleicao = col_integer(),
                                         qt_Empenhos = col_integer()))
    
    municpios = read_csv(arquivo_municipios, 
                         col_types = cols(.default = col_character()))
    
    empenhos_brutos = empenhos_brutos %>%
       left_join(municpios, by = "COD_MUNICIPIO")
    
    return(empenhos_brutos)
}

completa_anos = function(empenhos_in) {
    require(dplyr)
    require(tidyr)
    empenhos_in %>%
        group_by(COD_MUNICIPIO,
                 NOME_MUNICIPIO) %>% 
        do(
            complete(., 
                nesting(ano_eleicao, 
                        sigla_partido, 
                        COD_MUNICIPIO,
                        NOME_MUNICIPIO),
                nesting(
                    nu_CPFCNPJ,
                    nome_fornecedor,
                    codigo_municipio_fornecedor
                ),
                fill = list(vl_Empenhos = 0, qt_Empenhos = 0)
            )
        ) %>%
        return()
}

filtra_relevantes = function(empenhos_in, minimo_empenhos, aumento_minimo){
    require(dplyr)
    relevantes = empenhos_in %>% 
        group_by(nu_CPFCNPJ, COD_MUNICIPIO) %>% 
        summarise(total_empenhos = sum(vl_Empenhos), 
                  aumento = max(vl_Empenhos) / min(ifelse(vl_Empenhos == 0, 1, vl_Empenhos))) %>% 
        filter(total_empenhos >= minimo_empenhos, aumento >= aumento_minimo)
    
    empenhos_in %>%
        filter(nu_CPFCNPJ %in% relevantes$nu_CPFCNPJ) %>% 
        return()
}

calcula_mudancas = function(empenhos_in){
    partidos_w = empenhos_in %>% 
        select(COD_MUNICIPIO, legislatura, sigla_partido) %>% 
        distinct() %>% 
        spread(legislatura, sigla_partido) 
    names(partidos_w) = c("COD_MUNICIPIO", "partido2009", "partido2013")
    
    empenhos_w = empenhos_in %>% 
        select(nu_CPFCNPJ, 
               nome_fornecedor, NOME_MUNICIPIO, COD_MUNICIPIO, legislatura, vl_Empenhos) %>% 
        spread(legislatura, vl_Empenhos) %>% 
        left_join(partidos_w, by = "COD_MUNICIPIO")
    
    mudancas = empenhos_w %>% 
        group_by(NOME_MUNICIPIO) %>% 
        summarise(correlacao = cor(`2009-2012`, `2013-2016`, method = "kendall"), 
                  partido2009 = first(partido2009), 
                  partido2013 = first(partido2013)) %>% 
        mutate(mudou = (partido2009 != partido2013), 
               txt = paste0(NOME_MUNICIPIO, " (", partido2009, "-", partido2013, ")"))
    
    return(mudancas)
}

