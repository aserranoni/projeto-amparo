library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(vcd)
#leitura da tabela


dados <- read_excel("C:/Users/User/Desktop/projetoamparo-master/dados2.xlsx")

#extracao das arvores


v<-unique(dados$`tree`)

  #vetor de contextos possiveis
  
poscont<-c("0","1","2","00","01","02","10","20","11","12","21",
      "22","000","001","002","010","020","100","200","011",
      "021","101","201","012","022","102","202","110","210",
      "120","220","111","211","121","221","112","212","122","222")
probs<-matrix(nrow=length(poscont),ncol=20)
#funcao para pegar os padroes
  nums <-grep("[0-9]+:[0-9]+;[0-9]+", v, value=TRUE)
  nums2 <-strsplit(nums,' *\\| *')
  l<-1
for(i in 1:length(nums2)){
      
         for(j in 1:length(nums2[[i]])){
      
                    contx<-str_trim(strsplit( nums2[[i]][j],":")[[1]][1])
                    linha<-which (poscont == contx)
    
                    pr<-strsplit(nums2[[i]][j],":")[[1]][2]
                    p1<-strsplit(pr,";")[[1]][1]
                    p2<-strsplit(pr,";")[[1]][2]
  
                    probs[linha,l] <- as.numeric(p1)
    
                    probs[linha,l + 1] <- as.numeric(p2)
    
                    probs[linha,l + 2] <- 1 - probs[linha,l] - probs[linha,l + 1]
    
                    
  
         }
  l<-l+3
}
  
## matriz com os placares de cada jogador em cada jogada  
  
  x <- data.frame(as.character(dados$playerAlias) , dados$move, dados$`optionChoosen`, dados$tree , dados$sequExecuted , dados$`movementTime(s)` , dados$correct, dados$game ,dados$phase,dados$`Escala HY`,dados$Escolaridade)

 names(x) <- cbind("player","move","escolhido","tree","seqreal","tempo","acertou","jogo","fase","escalaHY","escolaridade")
  
player<-unique(x$player)
score<-matrix(ncol=length(player),nrow = 200)
k<-rep(1,length(player))
nums2<-strsplit(v, ' *\\| *')
vecs <- vector(length = nrow(x))
for(i in 1:nrow(x)) {
        
  t<-which(v==x[i,"tree"])
  if (length(t) == 0) {
    score[k[jog],jog] <- x[i,"acertou"]
    vecs[i] <- x[i,"acertou"]
    next
  }
  
  tdcont<-array(length(nums2[[t]]))

  
  for(j in 1:length(nums2[[t]])){
    tdcont[j]<- strsplit(nums2[[t]][j], ':')[[1]][1]
  }
  past<-substr(x[i,"seqreal"], 1, x[i,"move"] - 1)
  for (j in 1:length(tdcont)) {
    if(!is.na(tdcont[j]) && endsWith(past, tdcont[j])) {
      cntx<-which(poscont==tdcont[j])
    }
  }
      
  jog<-which(player==x[i,"player"])
  
  if (is.na(probs[cntx,3*(t-2)+x[i,"escolhido"]+1])) {
    score[k[jog],jog] <- x[i,"acertou"]
    vecs[i] <- x[i,"acertou"]
  } 
  else {
    score[k[jog],jog]<-probs[cntx,3*(t-2)+x[i,"escolhido"]+1]
      vecs[i] <-probs[cntx,3*(t-2)+x[i,"escolhido"]+1]     
    }
  k[jog]<-k[jog]+1
}
  x<-cbind(x,vecs)

  #score <- desc(score)

  
  
############# Algumas medidas ######################

media_ind <- vector()

for(i in 1:ncol(score)){
  media_ind[i] <- mean(score[,i],na.rm=TRUE)
}

media_geral <- mean(media_ind,na.rm=TRUE)

temps <- matrix(nrow=200,ncol=200)

k<-rep(1,length(player))
jog <- which ( player == x[1,"player"] )

for ( i in 1:nrow (x) ) {
  
  
  jog <- which ( player == x[i,"player"] )
  
  temps[k[jog],jog] <- x[i,"tempo"]
  
  k[jog]<-k[jog]+1
  
}

############## Análise ###################

jogos <- unique ( x$jogo )
pdf("grafis.pdf" )

#cada jogo
for ( i in 1 : length(jogos) ) {
  
  fases <- filter ( x, jogo== jogos[i] )
  
  fases2<-unique(fases$fase)

  #cada fase  
  for ( j in 1 : length(fases2) ){
    tabela<-filter(fases , fase==fases2[j])
    mediascore <- mean (tabela$vecs , na.rm = TRUE )
    mediatempo <- mean ( tabela$tempo , na.rm = TRUE )
    fases3<-unique(tabela$player)
    mat<-matrix(nrow=length(fases3),ncol = 4)
    
 #cada jogador
    for(k in 1:length(fases3)){ 
      
    tabjog <-  filter(tabela, player==fases3[k])
    
    mej<-mean(tabjog$vecs , na.rm = TRUE)
    
    met<-mean( tabjog$tempo, na.rm=TRUE)
    
    mat[k,3]<-tabjog$escolaridade[1]
    mat[k,4]<-tabjog$escalaHY[1]
    
    
      if(mej >= mediascore && is.nan(mej)==FALSE ){
      
      mat[k,1]<-1
      
    }
    else{
      mat[k,1]<-0
      
    }
    
    if(met<= mediatempo && is.nan(met)==FALSE ){
      
      mat[k,2]<-1
    }
    else{
      mat[k,2]<-0
      }
    
    
   #GRAFICO
      ####colocar aqui o grafico a ser feito
      #grafico  aes=eixos
      g<-ggplot(tabjog, aes(x=move, y=tempo) )
      #fala pra fazer pontos e colocar cores diferentes de acordo com o valor de acerto
      g<-g+geom_point(aes(color=factor(acertou)),rm.na=TRUE)
       
      #curvinha maneira
      #+geom_smooth()
      #matriz de graficos
      #+facet_grid (. ~fase )
      #### nomes
    g<-g+labs (title=str_c(str_c(str_c(str_c(as.character(fases2[j]), as.character(jogos[i]),sep=";"),as.character(fases3[k]),sep=":"),as.character(tabjog$escalaHY[1]),sep = ":"),as.character(tabjog$escolaridade),sep=":"),x="jogada", y="tempo" )    
   #fecha jogadodor
      print(g)
      
      }
#fecha fase
    if(length(mat[,1])==length(mat[,2])){
    nem<-table(as.factor(mat[,1]),as.factor(mat[,2]))
    print(nem)
   
     #assoc(nem, nem[2,], nem[,2], shade=TRUE , legend=TRUE)
     #as.matrix(nem)
     mos<-mosaicplot(nem, main = "concentracao total",xlab = "pontuacao na fase",ylab = "tempo medio na fase",color = c(56,44))
     print(mos)
     #fecha jogo    
     }
    }  

  }
 

dev.off()


#Analise





