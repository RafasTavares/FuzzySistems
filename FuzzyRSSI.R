
funcFuzzyRSSI <- function() {
  #Azul - Excelente - Trapezoidal ========================================
  x<-c(-100:-30)
  a<-(-55)
  m<-(-50)
  n<-(-30)
  b<-(-30)

  y<-1:1
  i<-1
  
  for (i in 1:length(x)) {
    
    if (x[i] < a)  {
      
      y[i] = 0;
      
    } else if(x[i]>=a & x[i] < m) {
      
      y[i]=(x[i]-a) / (m-a)
      
    } else if(x[i]>=m & x[i] < n) {
      
      y[i]=1
      
    } else if(x[i]>=n & x[i] <= b) {
      
      y[i]=(b-x[i]) / (b-n)
      
    } else {
      
      y[i] = 0
      
    }
      
    
  }
  
  yExl <- y
  xExl <- x 
  #============================================
  #Vermelho -Fraco - Trapezoidal ========================================
  a<-(-100)
  m<-(-100)
  n<-(-70)
  b<-(-65)
  
  y<-1:1
  i<-1
  
  for (i in 1:length(x)) {
    
    if (x[i] < a)  {
      
      y[i] = 0;
      
    } else if(x[i]>=a & x[i] < m) {
      
      y[i]=(x[i]-a) / (m-a)
      
    } else if(x[i]>=m & x[i] < n) {
      
      y[i]=1
      
    } else if(x[i]>=n & x[i] <= b) {
      
      y[i]=(b-x[i]) / (b-n)
      
    } else {
      
      y[i] = 0
      
    }
    
    
  }
  
  yWeak <- y
  xWeak <- x 
  #============================================
  
  #Verde - Triangular - Bom ==================================
  a<-(-65)
  m<-(-55)
  b<-(-50)
  
  y <- 1:1
  i<-1
  
  for (i in 1:length(x)) {
    
    ## desenha o triângulo 
    
    if(x[i] <= a ) { 
      
      y[i]<-0 
      
    } else if (x[i]>=a & x[i] < m ) { 
      
      y[i] <- (x[i]-a) /(m-a)
      
    } else if (x[i] >= m & x[i]<=b) { 
      
      y[i] <- (b-x[i]) / (b-m)
      
    } else {
      
      y[i] <- 0
    }
  }
  
  yGood <- y
  xGood <- x 
  #===========================================
  
  #Amarelo - Triangular - Razoável ==================================
  a<-(-70)
  m<-(-65)
  b<-(-55)
  
  y <- 1:1
  i<-1
  
  for (i in 1:length(x)) {
    
    ## desenha o triângulo
    
    if(x[i] <= a ) { 
      
      y[i]<-0 
      
    } else if (x[i]>=a & x[i] < m ) { 
      
      y[i] <- (x[i]-a) /(m-a)
      
    } else if (x[i] >= m & x[i]<=b) { 
      
      y[i] <- (b-x[i]) / (b-m)
      
    } else {
      
      y[i] <- 0
    }
  }
  
  yFair <- y
  xFair <- x 
  #============================================================
 

  plot(yExl~xExl,type="l",ylim = c(0,1),ylab = "Pertinência",xlab="RSSI (dBm)", col="blue", lwd=2)
  lines(xWeak, yWeak, col="red", type="l", lwd=2)
  lines(xGood, yGood, col="green", type="l", lwd=2)
  lines(xFair, yFair, col="yellow", type="l", lwd=2)
  legend(-100, 0.8, legend=c("Fraco", "Razoável", "Bom", "Excelente"), text.font=2,
         col=c("red", "yellow", "green", "blue"), lty=1, cex=0.4, pt.cex=1)
  #==============================================================
  # União entre os conjuntos: Categoriza o RSSI para o conjunto Fuzzy de Maior Pertinência
  fuzzySetUnion <- function(setA, setB) {
    uniao<-1:1
    i<-1
    for (i in 1:length(setA)) {
      uniao[i] = max(setA[i], setB[i])
    }
    return (uniao)
  }
  totalUnion <- fuzzySetUnion(fuzzySetUnion(fuzzySetUnion(yWeak,yFair),yGood),yExl)
  plot(totalUnion~xExl,type="l",ylim = c(0,1),ylab = "Pertinência",xlab="RSSI (dBm)", col="blue", lwd=2)
  legend(-100, 0.8, legend=c("União dos Fuzzy Sets"), text.font=2,
         col=c("blue"), lty=1, cex=0.4, pt.cex=1)
  #=============================================================

  # Agora Categoriza os dados Crisp RSSI para o conjunto Fuzzy do DataSet indoor localization 
  # with WIFI 489 Colunas (APs) e 446 Linhas (Número de registros)
  
  dataRSS <- read.csv("Data/Training_rss.csv", header = FALSE)
  dataCOOR <- read.csv("Data/Training_coordinates.csv", header = FALSE)
  tempFuzzyMatrix = matrix(nrow = nrow(dataRSS), ncol = ncol(dataRSS))
  
  for(i in 1:nrow(dataCOOR)){
    #Coordenadas em metros locais Atual
    x = dataCOOR[i,1]
    y = dataCOOR[i,2]
    z = dataCOOR[i,3]
   
    #Para essa localidade específica x,y,z
    #Obter os RSSI para cada um dos 489 APs
    for(j in 1:ncol(dataRSS)){
      rssi = dataRSS[i,j]
      # Categorizar o valor Crisp RSSI para conjuntos Fuzzy
      if(rssi < -70){
        
        tempFuzzyMatrix
      }
    }
    
    
  }
  
}
