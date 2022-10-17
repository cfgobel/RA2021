# PRAIA_SED ----------------------------------------------------------------
###   SET ARQUIVOS      ####
trn <- "PMBA-A1-RESULTADOS-TRN1-Outubro2021/PMBA-A1-RESULTADOS-TRN1-20211031-PRAIA.xlsx"
# nomes das abas
aba <- excel_sheets(trn)

cname <- read_xlsx(trn,"A1-PRAIA_SED", skip = 2,n_max = 0,
                   .name_repair= ~substr(.x,1,nchar(.x))) %>% names()
analise <- read_xlsx(trn,"A1-PRAIA_SED", skip = 1,n_max = 0,
                     .name_repair= ~substr(.x,1,nchar(.x))) %>% names()
metad <- read_xlsx(trn,"A1-PRAIA_SED",
                   range=cell_limits(c(6,1),c(NA,4)),
                   col_names = c("campanha","ponto","replica","data"),
                   col_types = c("numeric","text","text","date"),
                   na = c("PREENCHER"))

# importar aba para separar METAL DISSOLVIDO FUNDO
met <- read_xlsx(trn,"A1-PRAIA_SED",
                 range=cell_limits(c(6,17),c(NA,NA)),
                 col_names = cname,
                 # col_types = "numeric",
                 na = c("NA","AI","NE","ENC","ER"))
names(met) <- cname


## Matriz CAMP 6 e F
indice <- grep("metal",tolower(analise))
met <- met[,indice]  %>% bind_cols(metad, .) %>% as.data.frame()

# Removendo caracter do LQ e convertendo p numérico
# str(met)
for (i in 6:length(met)) {
  if (class(met[,i])=="character") {
    met[,i] <- gsub("\"|<|>","",met[,i])
    met[,i] <- gsub(",",".",met[,i]) %>% as.numeric(.)
  }
}
# str(met[,sapply(met,class)=="character"])


# média por ponto, e remover ponto não amostrado
met <- met %>% group_by(campanha,ponto) %>%
  summarise_at(vars(-replica,-data),mean,na.rm=TRUE)

met <- arrange(met,ponto)

###   SET ARQUIVOS      ####
ano1 <- "../RSE2021/PMBA-A1-RESULTADOS-ANO1-20210531-v4.xlsx"
# nomes das abas
aba <- excel_sheets(ano1)

cname <- read_xlsx(ano1,"A1-PRAIA_SED", skip = 2,n_max = 0,
                   .name_repair= ~substr(.x,1,nchar(.x))) %>% names()
analise <- read_xlsx(ano1,"A1-PRAIA_SED", skip = 1,n_max = 0,
                     .name_repair= ~substr(.x,1,nchar(.x))) %>% names()
metad <- read_xlsx(ano1,"A1-PRAIA_SED",
                   range=cell_limits(c(6,1),c(NA,4)),
                   col_names = c("campanha","ponto","replica","data"),
                   col_types = c("numeric","text","text","date"),
                   na = c("PREENCHER"))

# importar aba para separar METAL DISSOLVIDO FUNDO
met1 <- read_xlsx(ano1,"A1-PRAIA_SED",
                 range=cell_limits(c(6,17),c(NA,NA)),
                 col_names = cname,
                 # col_types = "numeric",
                 na = c("NA","AI","NE","ENC","ER"))
names(met1) <- cname


indice <- grep("metal",tolower(analise))
met1 <- met1[,indice]  %>% bind_cols(metad, .) %>% as.data.frame()

# Removendo caracter do LQ e convertendo p numérico
# str(met1)
for (i in 6:length(met1)) {
  if (class(met1[,i])=="character") {
    met1[,i] <- gsub("\"|<|>","",met1[,i])
    met1[,i] <- gsub(",",".",met1[,i]) %>% as.numeric(.)
  }
}
# str(met1[,sapply(met1,class)=="character"])

# média por ponto, e remover ponto não amostrado
met1 <- met1 %>% group_by(campanha,ponto) %>%
  summarise_at(vars(-replica,-data),mean,na.rm=TRUE)

met1 <- arrange(met1,ponto)

metais <- bind_rows(met1,met) %>% arrange(ponto)

