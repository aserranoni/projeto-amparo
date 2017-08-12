library(readxl)
library(stringr)
library(dplyr)

#leitura da tabela


dados <- read_excel("C:/Users/Atilio/Desktop/projetoamparo-master/dados.xlsx")

#extracao das arvores


v<-unique(dados$`tree
`)

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
  
x <- cbind(as.character(dados$`player
Alias`),dados[,23],dados[,28],dados[,32],dados[,33],dados$`movementTime(s)`,dados$`correct`, dados[,3], dados[,5] )
  
names(x) <- c("player","move","escolhido","tree","seqreal","tempo","acertou","jogo","fase")
as.data.frame(x)
  
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

#cada jogo
for ( i in 1 : length(jogos) ){
  
  fases <- filter ( x, jogo == jogos[i] )
  
  fases2<-unique(fases$fase)
#cada fase  
  for ( j in 1 : length(fases2) ){
    
    tabjog <- filter ( x, jogo == jogos[i] )
    
    tabela <- filter ( tabjog, fase == fases2 [j] ) 
    ####colocar aqui o grafico a ser feito
    
    hist(tabjog$tempo)
  
    ####
    
    }
}









