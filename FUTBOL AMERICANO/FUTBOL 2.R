pagina<-'http://www.espn.com.mx/futbol-americano/nfl/posiciones'
download.file(pagina, "futbol.csv")
datos<-read.table("./futbol.csv",sep=",", header=TRUE)
library(XML)
nodos<-htmlTreeParse(pagina,useInternalNodes=TRUE)
EQUIPOS<-xpathSApply(nodos,"//span[@class='team-names']|//span[@class='team-names inactive']",xmlValue)
conf<-xpathSApply(nodos,"//span[@class='long-caption']",xmlValue)
datosjuntos<-xpathSApply(nodos,"//td[@class='']",xmlValue)
puntosjuntos<-xpathSApply(nodos,"//td[@class='center']",xmlValue)
CONFERENCIA<-c()
for (i in 1:32){
    if(i<=15){
        nombrecon<-conf[1]
    } else if (i>15){
        nombrecon<-conf[2]
    }
    CONFERENCIA<-c(CONFERENCIA,nombrecon)
}



GANADO<-c()
y=1
for( k in 1:length(datosjuntos)){
    ganar<-datosjuntos[y]
    y=(6*k)+(k+1)
    GANADO<-c(GANADO,ganar)
}
GANADOS<-GANADO[!is.na(GANADO)]


perdido<-c()
y=1
for( k in 1:length(datosjuntos)){
    perder<-datosjuntos[y+1]
    y=(6*k)+(k+1)
    perdido<-c(perdido,perder)
}
PERDIDOS<-perdido[!is.na(perdido)]



empat<-c()
y=1
for( k in 1:length(datosjuntos)){
    empate<-datosjuntos[y+2]
    y=(6*k)+(k+1)
    empat<-c(empat,empate)
}
EMPATES<-empat[!is.na(empat)]



porcentaje<-c()
y=1
for( k in 1:length(datosjuntos)){
    por<-datosjuntos[y+3]
    y=(6*k)+(k+1)
    porcentaje<-c(porcentaje,por)
}
PORCENTAJ<-porcentaje[!is.na(porcentaje)]



gf1<-c()
y=1
for( k in 1:length(datosjuntos)){
    gf2<-datosjuntos[y+4]
    y=(6*k)+(k+1)
    gf1<-c(gf1,gf2)
}
GF<-gf1[!is.na(gf1)]


gc1<-c()
y=1
for( k in 1:length(datosjuntos)){
    gc2<-datosjuntos[y+5]
    y=(6*k)+(k+1)
    gc1<-c(gc1,gc2)
}
GC<-gc1[!is.na(gc1)]


r1<-c()
y=1
for( k in 1:length(datosjuntos)){
    r2<-datosjuntos[y+6]
    y=(6*k)+(k+1)
    r1<-c(r1,r2)
}
R<-r1[!is.na(r1)]


lo1<-c()
y=1
for( k in 1:length(puntosjuntos)){
    lo2<-puntosjuntos[y]
    y=(k*4)+1
    lo1<-c(lo1,lo2)
}
LOCAL<-lo1[!is.na(lo1)]


v1<-c()
y=1
for( k in 1:length(puntosjuntos)){
    v2<-puntosjuntos[y+1]
    y=(k*4)+1
    v1<-c(v1,v2)
}
VISITANTE<-v1[!is.na(v1)]


di1<-c()
y=1
for( k in 1:length(puntosjuntos)){
    di2<-puntosjuntos[y+2]
    y=(k*4)+1
    di1<-c(di1,di2)
}
DIV<-di1[!is.na(di1)]
data.frame(EQUIPOS,CONFERENCIA,GANADOS, PERDIDOS,EMPATES,PORCENTAJ,LOCAL,VISITANTE,DIV,GF,GC,R)