library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(vcd)
library(Rmixmod)
library(broom)
library(ellipse)
library(magrittr)
library(purrr)
library(purrrlyr)
library(tibble)
library(rpart)
library(rpart.plot)
library(caret)
library(glmnet)
library(corrplot)
set.seed(76)
#leitura da tabela


dados <-
  read_excel("C:/Users/User/Desktop/projetoamparo-master/dados2.xlsx")
moca <-
  read_excel("C:/Users/User/Desktop/projetoamparo-master/Jogo do Goleiro + Dados Motores   1.xlsx")
#extracao das arvores


v <- unique(dados$`tree`)

#vetor de contextos possiveis

poscont <- c(
  "0",
  "1",
  "2",
  "00",
  "01",
  "02",
  "10",
  "20",
  "11",
  "12",
  "21",
  "22",
  "000",
  "001",
  "002",
  "010",
  "020",
  "100",
  "200",
  "011",
  "021",
  "101",
  "201",
  "012",
  "022",
  "102",
  "202",
  "110",
  "210",
  "120",
  "220",
  "111",
  "211",
  "121",
  "221",
  "112",
  "212",
  "122",
  "222"
)
probs <- matrix(nrow = length(poscont), ncol = 20)
#funcao para pegar os padroes
nums <- grep("[0-9]+:[0-9]+;[0-9]+", v, value = TRUE)
nums2 <- strsplit(nums, ' *\\| *')
l <- 1
for (i in 1:length(nums2)) {
  for (j in 1:length(nums2[[i]])) {
    contx <- str_trim(strsplit(nums2[[i]][j], ":")[[1]][1])
    linha <- which (poscont == contx)
    
    pr <- strsplit(nums2[[i]][j], ":")[[1]][2]
    p1 <- strsplit(pr, ";")[[1]][1]
    p2 <- strsplit(pr, ";")[[1]][2]
    
    probs[linha, l] <- as.numeric(p1)
    
    probs[linha, l + 1] <- as.numeric(p2)
    
    probs[linha, l + 2] <-
      1 - probs[linha, l] - probs[linha, l + 1]
    
    
    
  }
  l <- l + 3
}

## matriz com os placares de cada jogador em cada jogada

x <-
  data.frame(
    as.character(dados$playerAlias) ,
    dados$move,
    dados$`optionChoosen`,
    dados$tree ,
    dados$sequExecuted ,
    dados$`movementTime(s)` ,
    dados$correct,
    dados$game ,
    dados$phase,
    dados$`Escala HY`,
    dados$Escolaridade
  )

names(x) <-
  cbind(
    "player",
    "move",
    "escolhido",
    "tree",
    "seqreal",
    "tempo",
    "acertou",
    "jogo",
    "fase",
    "escalaHY",
    "escolaridade"
  )

player <- unique(x$player)
score <- matrix(ncol = length(player), nrow = 200)
k <- rep(1, length(player))
nums2 <- strsplit(v, ' *\\| *')
vecs <- vector(length = nrow(x))
jog <- 1
cntx <- 1
for (i in 1:nrow(x)) {
  t <- which(v == x[i, "tree"])
  if (length(t) == 0) {
    score[k[jog], jog] <- x[i, "acertou"]
    vecs[i] <- x[i, "acertou"]
    next
  }
  
  tdcont <- array(length(nums2[[t]]))
  
  
  for (j in 1:length(nums2[[t]])) {
    tdcont[j] <- strsplit(nums2[[t]][j], ':')[[1]][1]
  }
  past <- substr(x[i, "seqreal"], 1, x[i, "move"] - 1)
  for (j in 1:length(tdcont)) {
    if (!is.na(tdcont[j]) && endsWith(past, tdcont[j])) {
      cntx <- which(poscont == tdcont[j])
    }
  }
  
  jog <- which(player == x[i, "player"])
  
  if (is.na(probs[cntx, 3 * (t - 2) + x[i, "escolhido"] + 1])) {
    score[k[jog], jog] <- x[i, "acertou"]
    vecs[i] <- x[i, "acertou"]
  }
  else {
    score[k[jog], jog] <- probs[cntx, 3 * (t - 2) + x[i, "escolhido"] + 1]
    vecs[i] <- probs[cntx, 3 * (t - 2) + x[i, "escolhido"] + 1]
  }
  k[jog] <- k[jog] + 1
}
x <- cbind(x, vecs)

#score <- desc(score)



############## Análise - Identificacao do grupo de risco ###################

jogos <- unique (x$jogo)
pdf("graficosamparo.pdf")
grups <- matrix(0, 70, 8)
u <- 1
despjog<-vector(length = 70)
medtmpaq<-vector(length = 70)
matdesemp<-matrix(0,70,8)
#cada jogo
for (i in 1:length(jogos)) {
  fases <- filter(x, jogo == jogos[i])
  fases2 <- unique(fases$fase)
  
  #cada fase
  
  for (j in 1:length(fases2)) {
    if (i == 4 && j == 1) {
      j <- 2
    }
    
    tabela <- filter(fases , fase == fases2[j])
    #olhando para a mediana
    mediascore <- mean(tabela$vecs, na.rm = TRUE)
    mediatempo <- mean(tabela$tempo , na.rm = TRUE)
    fases3 <- unique(tabela$player)
    mat <- matrix(0, length(player), 9)
    mat <- as.data.frame(mat)
    names(mat) <-
      c("medscore",
        "medtempo",
        "escol",
        "hy",
        "grupo",
        "jogador",
        "risco",
        "alfa",
        "beta"
        )
    #cada jogador
    medvecs<-vector(length = nrow(mat))
    medtmpfase<-vector(length = nrow(mat))
    desempenhofase<-vector(length = nrow(mat))
    for (k in 1:length(fases3)) {
      tabjog <-  filter(tabela, player == fases3[k])
      #olhando para a media ponderada
      mej <- mean(tabjog$vecs , na.rm = TRUE)
      
      met <- mean(tabjog$tempo, na.rm = TRUE)
      
      mat[k, 3] <- tabjog$escolaridade[1]
      mat[k, 4] <- tabjog$escalaHY[1]
      
      
      if (mej >= mediascore && is.nan(mej) == FALSE) {
        mat[k, 1] <- 1
        
      }
      else{
        mat[k, 1] <- 0
        
      }
      
      if (met <= mediatempo && is.nan(met) == FALSE) {
        mat[k, 2] <- 1
      }
      else{
        mat[k, 2] <- 0
      }
      
      if (mat[k, 1] == 1 &&
          mat[k, 2] == 1 && is.nan(met) == FALSE && is.nan(mej) == FALSE) {
        mat[k, 5] <- 1
        grups[k, u] <- 1
      }
      if (mat[k, 1] == 0 &&
          mat[k, 2] == 1 && is.nan(met) == FALSE && is.nan(mej) == FALSE) {
        mat[k, 5] <- 2
        grups[k, u] <- 2
      }
      if (mat[k, 1] == 1 &&
          mat[k, 2] == 0 && is.nan(met) == FALSE && is.nan(mej) == FALSE) {
        mat[k, 5] <- 3
        grups[k, u] <- 3
      }
      if (mat[k, 1] == 0 &&
          mat[k, 2] == 0 && is.nan(met) == FALSE && is.nan(mej) == FALSE) {
        mat[k, 5] <- 4
        grups[k, u] <- 4
      }
      mat[k, 6] <- k
      
      
      
      if(nrow(tabjog)>=5){ 
      coefs<-tabjog %>% do(tidy(glm(acertou~move, 
                            data=., 
                            family=binomial(link="logit"))))
        
      #glm_predicted <- predictvals(coefs, "move", "acertou", type="response")
      mat[k, 8] <- coefs[1, 2]
      mat[k, 9] <- coefs[2, 2]}
      else{
        mat[k, 8] <- coefs[1, 2]
        mat[k, 9] <- coefs[2, 2]}
      
      
      #GRAFICO De cada fase;jogo.jogador
      ####colocar aqui o grafico a ser feito
      #grafico  aes=eixos
      g <- ggplot(tabjog, aes(x = as.factor(move), y = tempo))
      #fala pra fazer pontos e colocar cores diferentes de acordo com o valor de acerto
      g <- g + geom_point(aes(color = factor(acertou)), rm.na = TRUE)
      #g<- g+ geom_line(data=glm_predicted, colour="#1177FF", size=1)    
      #curvinha maneira
      
      #matriz de graficos
      #+facet_grid (. ~fase )
      #### nomes
      g <-
        g + labs (
          title = str_c(
            str_c(
              str_c(
                str_c(as.character(fases2[j]), as.character(jogos[i]), sep = ";"),
                as.character(fases3[k]),
                sep = ":"
              ),
              as.character(tabjog$escalaHY[1]),
              sep = ":"
            ),
            as.character(tabjog$escolaridade),
            sep = ":"
          ),
          x = "jogada",
          y = "tempo"
        )
      
      print(g)
    
    
     
      
      dsmp <- 0
      
      dsmpq <- 0
      
      e <- exp(1)
      
      lambda <- 1/20
      
      lambda2 <- sqrt ( e ) 
      
      dgfq <- 20
      
      for ( h in length ( tabjog$vecs )){
        
        dsmp <- dsmp + ( ( e ^ ( -(lambda) * tabjog$tempo[h] ) ) * ( e ^ ( (lambda2) * tabjog$vecs[h] ) ) )
        
        dsmpq <- dsmpq + ( dchisq( tabjog$tempo[h], dgfq )  * ( e ^ ( (lambda2) * tabjog$vecs[h] ) ) )
        
      }
      
      desempenhofase[k] <- dsmp
      
      matdesemp[k,u] <- dsmp
      
      
      
      ## MANEIRA ANTIGA DE MEDIR DESEMPENHO
      medvecs[k]<-mean(tabjog$vecs)      
      medtmpfase[k]<-mean(tabjog$tempo)
      if (i == 1 && j == 1) {
        medtmpaq[k]<-mean(tabjog$tempo)
      }
     
     # desempenhofase[k]<-medvecs[k]/(medtmpaq[k]/medtmpfase[k])
     # matdesemp[k,u]<-medvecs[k]/(medtmpaq[k]/medtmpfase[k])
      print(matdesemp[k,u])
     #  #fecha jogador   
     }
    
    
    
    #fecha fase; brincadeira com mat
    
    u <- u + 1
    
    
    ##FORMACAO DE GRUPOS PARA O FUTURO
    if (i == 1 && j == 1) {
      riscoaq <- filter(mat, grupo == 1)
      plriscoaq <- unique(riscoaq$jogador)
      tam <- nrow(riscoaq)
      desempenhoaq<-desempenhofase
      print(plriscoaq)
    }
    
    
    if (i == 3 && j == 3) {
      riscoimp <- filter(mat, grupo == 1 | grupo == 3)
      plriscoimp <- unique(riscoimp$jogador)
      tam <- nrow(riscoimp)
      desempenhoimp<-desempenhofase
      print(plriscoimp)
    }
    
    
    
    if (i == 4 && j == 2) {
      riscoexp <- filter(mat, grupo == 1)
      plriscoexp <- unique(riscoexp$jogador)
      desempenhoexp<-desempenhofase
      tam <- nrow(riscoexp)
      print(plriscoexp)
      
    }
    
    
##Analise qualitativa da fase
    grup1<-vector(length = nrow(mat))
    grup2<-vector(length = nrow(mat))
    grup3<-vector(length = nrow(mat))
    grup4<-vector(length = nrow(mat))
      for(w in 1:nrow(mat)){
        if(mat$grupo[w]==1){
          grup1[w]<-1
        }
        else{
          grup1[w]<-0
        }
        if(mat$grupo[w]==2){
          grup2[w]<-1
        }
        else{
          grup2[w]<-0
        }
        if(mat$grupo[w]==3){
          grup3[w]<-1
        }
        else{
          grup3[w]<-0
        }
        if(mat$grupo[w]==4){
          grup4[w]<-1
        }
        else{
          grup4[w]<-0
        }
        
      
      }
    mat<-cbind(mat,grup1,grup2,grup3,grup4,medtmpfase,medvecs,moca$MoCA_visuoesp_exec[1:nrow(mat)],moca$MoCA_nomeação[1:nrow(mat)],moca$MoCA_atenção[1:nrow(mat)],moca$MoCA_abstração[1:nrow(mat)],moca$MoCA_linguagem[1:nrow(mat)],moca$MoCA_evoc_tardia[1:nrow(mat)],moca$MoCA_orientação[1:nrow(mat)],moca$MoCA_TOTAL[1:nrow(mat)])

    arv1<-rpart(grup1~mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
    rpart.plot(arv1)%>%print()
    
    arv5<-rpart(grup1~escol+mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
    rpart.plot(arv5)%>%print()
    arv2<-rpart(grup2~mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
    rpart.plot(arv2)%>%print()
    arv6<-rpart(grup2~escol+mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
    rpart.plot(arv6)%>%print() 
    arv3<-rpart(grup3~mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
    rpart.plot(arv3)%>%print()
    
    arv7<-rpart(grup3~escol+mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
    rpart.plot(arv7)%>%print()
    arv4<-rpart(grup4~mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
    rpart.plot(arv4)%>%print()
    arv8<-rpart(grup4~escol+mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
    rpart.plot(arv8)%>%print()
    
    
     grafase <- ggplot(mat, aes(x = medtmpfase, y = medvecs))
     grafase <- grafase + geom_point(rm.na = TRUE)
     grafase <- grafase + facet_grid(. ~ escol)
     grafase2 <- grafase + facet_grid(. ~ hy)
     print(grafase)
     print(grafase2)
# 
#     ##cada fator do moca pelo desempenho nessa fase
#      fit_total<-lm(moca$MoCA_TOTAL[1:length(desempenhofase)]~desempenhofase)
#      fit_visuoesp<-lm(moca$MoCA_visuoesp_exec[1:length(desempenhofase)]~desempenhofase)
#      fit_atencao<-lm(moca$MoCA_atenção[1:length(desempenhofase)]~desempenhofase)
#      fit_abstracao<-lm(moca$MoCA_abstração[1:length(desempenhofase)]~desempenhofase)
#      fit_orientacao<-lm(moca$MoCA_orientação[1:length(desempenhofase)]~desempenhofase)
#      fit_linguagem<-lm(moca$MoCA_linguagem[1:length(desempenhofase)]~desempenhofase)
#      fit_evoc<-lm(moca$MoCA_evoc_tardia[1:length(desempenhofase)]~desempenhofase)
#      fit_nomeacao<-lm(moca$MoCA_nomeação[1:length(desempenhofase)]~desempenhofase)
#    
#      dfit<-cbind(moca[1:length(desempenhofase),],desempenhofase)
#    
#    
#      ##graficos disso , da pra fazer pra todos so copiando
#      grafit_total<-ggplot(dfit , aes(x = desempenhofase, y =MoCA_TOTAL))
#      grafit_total<-grafit_total+geom_point()
#     grafit_total<-grafit_total + geom_smooth(method = lm)
#     print(grafit_total)
#      grafit_visuoesp<-ggplot(dfit , aes(x = desempenhofase, y=MoCA_visuoesp_exec))
#     grafit_visuoesp<-grafit_total+geom_point()
#      grafit_visuoesp<-grafit_total + geom_smooth(method = lm)
#      plot(grafit_visuoesp)
#    
   
   
    # 
    # coeficients_fase <- cbind(fit_total$coefficients,
    # fit_visuoesp$coefficients,
    #  fit_atencao$coefficients,
    #  fit_abstracao$coefficients,
    #  fit_orientacao$coefficients,
    #  fit_linguagem$coefficients,
    #  fit_evoc$coefficients,
    #  fit_nomeacao$coefficients)
    # 
    #  coeficients_fase<-as.data.frame(coeficients_fase)
    #  grafx<-ggplot(coeficients_fase,aes(x))
    #   grafx<-grafx+geom_point()
    #   plot(grafx)
    # 
    #  plot(t(coeficients_fase))
    # 
    #  matgrafico<-cbind(mat$alfa, mat$beta, moca$MoCA_TOTAL, moca$ESCOL_, mat$hy, desempenhofase)
    # matgrafico<-as.data.frame(matgrafico)
    # names(matgrafico)<-c("alfa","beta","mocatotal","escolaridade","hy","desempenho")
    #  
    #   
    #  
    #  
    # mocagraf3<-ggplot(matgrafico, aes(x =desempenho, y =mocatotal))
    # mocagraf3<-mocagraf3 + geom_point(aes(color = factor(escolaridade),shape=factor(hy)))
    # mocagraf3<-mocagraf3 + labs (title="grafico", x="desempenho",y="moca")
    # 
    # print(mocagraf3)
    # 
    gralfa<-ggplot(mat,aes(x=medtmpfase,y=..density..))
    gralfa<-gralfa+geom_density(kernel="gaussian")
    #gralfa<-gralfa+geom_histogram(fill="cornsilk", colour="grey60", size=.2) 
    gralfa<-gralfa + labs (title="Distribuição das médias de tempo na fase", x="tempo",y="densidade")
    
    print(gralfa)
    
    bpalfa<-ggplot(mat, aes(x=factor(escol), y=medtmpfase,fill=escol)) 
    bpalfa<-bpalfa+ geom_boxplot() 
    bpalfa<-bpalfa+ stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    print(bpalfa)
    
    bpalfa2<-ggplot(mat, aes(x=factor(hy), y=medtmpfase,fill=hy)) 
    bpalfa2<-bpalfa+ geom_boxplot() 
    bpalfa2<-bpalfa+ stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    print(bpalfa2)
    
    grbeta<-ggplot(mat,aes(x=medvecs,y=..density..))
    grbeta<-grbeta+ geom_density(kernel="gaussian")
    #grbeta<-grbeta+geom_histogram(fill="cornsilk", colour="grey60", size=.2) 
    grbeta<-grbeta + labs (title="Distribuição das médias de placar na fase", x="placar",y="densidade")
    
    print(grbeta)
     
    bpbeta<-ggplot(mat, aes(x=factor(escol), y=medvecs,fill=escol)) 
    bpbeta<-bpbeta + geom_boxplot() 
    bpbeta<-bpbeta +stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    print(bpbeta)
    
    bpbeta2<-ggplot(mat, aes(x=factor(hy), y=medvecs,fill=hy)) 
    bpbeta2<-bpbeta + geom_boxplot() 
    bpbeta2<-bpbeta +stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
    print(bpbeta2)
    
    
    grconj<-ggplot(mat,aes(x=medtmpfase,y=medvecs))
    grconj<-grconj + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE)
    grconj<-grconj + labs (title="Distribuição conjunta de tempo e placar na fase", x="tempo",y="placar")
    
    print(grconj)
    
  
     
     
     desempenhofase<-as.data.frame(desempenhofase)
     desempenhofase<-c(mat,desempenhofase)
     desempenhofase<-as.data.frame(desempenhofase)
     grdesp<-ggplot(desempenhofase,aes(x=desempenhofase,y=..density..))
     grdesp<-grdesp+geom_density(kernel="gaussian")
     grdesp<-grdesp + labs (title="Distribuição do desempenho na fase", x="desempenho",y="densidade")
     
     print(grdesp)
      
       bpdes<-ggplot(desempenhofase, aes(x=factor(hy), y=desempenhofase))  
       bpdes<-bpdes+ geom_boxplot() 
     bpdes<-bpdes +stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
     
    if (length(mat[, 1]) == length(mat[, 2])) {
      nem <- table(as.factor(mat[, 1]), as.factor(mat[, 2]))
      print(nem)
      print(i)
      print(j)
      #assoc(nem, nem[2,], nem[,2], shade=TRUE , legend=TRUE)
      #as.matrix(nem)
      mos <-
        mosaicplot(
          nem,
          main = "concentracao total",
          xlab = "pontuacao na fase",
          ylab = "tempo medio na fase",
          color = c(5, 7)
        )
      print(mos)
      
      
    }
    
    
    
    
    
              
              
              
              
  
              
              }
  #fecha jogo

}


####pos analise
matdesemp<-cbind(matdesemp,moca$MoCA_visuoesp_exec,moca$MoCA_nomeação,moca$MoCA_atenção,moca$MoCA_linguagem,moca$MoCA_abstração,moca$MoCA_evoc_tardia,moca$MoCA_evoc_tardia)
matdesemp<-as.data.frame(matdesemp)
matcov<-cor(matdesemp,method = "pearson")
covs<-corrplot(matcov, method="shade",addCoef.col = "black", shade.col=NA, tl.col="black", tl.srt=45)
print(covs)

##FORMACAO DOS GRUPOS
novogruporisco1<-which(desempenhoaq>1.5*desempenhoimp)
novogruporisco2<-which(desempenhoexp>1.5*desempenhoimp)
novogruporisco<-intersect(novogruporisco1,novogruporisco2)
grn<-rep(0,length = nrow(mat))
for(i in 1:length(novogruporisco)){
  j<-novogruporisco[i]
  grn[j]<-1
}
mat<-cbind(mat,grn)
arvore43<-rpart(grn~mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
print(rpart.plot(arvore43))
arvore44<-rpart(grn~escol+mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
print(rpart.plot(arvore44))
arvore45<-rpart(grn~escol+MoCA_TOTAL,data=mat)
print(rpart.plot(arvore43))



moca <-as.data.frame(moca)
mocagraf<-ggplot(moca, aes(x = IDAD_, y =MoCA_TOTAL))
mocagraf<-mocagraf + geom_point(aes(color = factor(ESCOL_)))
mocagraf<-mocagraf + xlim(50,90)
mocagraf<-mocagraf + ylim(20,30)
mocagraf<-mocagraf + geom_smooth(method = lm)

print(mocagraf)

mograf<-ggplot(moca, aes(x=IDAD_, y=MoCA_TOTAL, colour=ESCOL_)) + geom_point()+geom_smooth(method=lm)

gruporisco <- intersect(plriscoaq, plriscoimp)
gruporisco <- intersect(gruporisco, plriscoexp)
print(gruporisco)

for (i in 1:length(gruporisco)) {
  mat[gruporisco[i], 7] <- 1
}

y <- as.data.frame(cbind(dados$playerAlias, dados$Idade))

z <- as.data.frame(cbind(as.numeric(y[, 1]), dados$Idade))



idade <- unique(z)
idade <- as.data.frame(idade)
names(idade) <- cbind("jog", "bla")

grrisco <- rep(0, length(idade$bla))

regression<-lm(idade$bla~grrisco)
print(regression)
#plot(regression)


tabrisco<-cbind(moca$MoCA_TOTAL , as.factor(mat$risco))
tabrisco<-as.data.frame(tabrisco)
tabrisco$risco<-as.factor(tabrisco$risco)
names(tabrisco)<-c("moca","risco")
coefs2<-glmnet(moca~risco,tabrisco,method=lm,trControl=trainControl(method="cv",number=10,verboseIter=TRUE))
plot(tabrisco)
print(coefs2)

##ARVORE DE FATORES
mat<-cbind(mat,idade)
arvore<-rpart(risco~mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
print(rpart.plot(arvore))
arvore2<-rpart(risco~escol+mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
print(rpart.plot(arvore2))
arvore3<-rpart(risco~bla+escol+mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
print(rpart.plot(arvore3))
arvore4<-rpart(risco~bla+mat[,16]+mat[,17]+mat[,18]+mat[,19]+mat[,20]+mat[,21]+mat[,22],data=mat)
print(rpart.plot(arvore4))

dev.off()