### create node XML
airports <- read.csv("airports.dat",header=FALSE)
head(airports)
airports$nodes <- paste('<node id="', as.character(airports$V1), '.0" label="',airports$V5, '"/>',sep="")

### create edge XML
routes <- read.csv("routes.dat",header=FALSE)
routes$edgenum <- 1:nrow(routes)
head(routes)
routes$edges <- paste('<edge id="', as.character(routes$edgenum),'.0" source="', routes$V3, '" target="',routes$V5, '"/>',sep="")

### build metadata
gexfstr <- '<?xml version="1.0" encoding="UTF-8"?>
<gexf xmlns:viz="http:///www.gexf.net/1.1draft/viz" version="1.1" xmlns="http://www.gexf.net/1.1draft">
<meta lastmodifieddate="2010-03-03+23:44">
<creator>Gephi 0.7</creator>
</meta>
<graph defaultedgetype="undirected" idtype="string" type="static">'

### append nodes
gexfstr <- paste(gexfstr,'\n','<nodes count="',as.character(nrow(airports)),'">\n',sep="")
fileConn<-file("output.gexf")
for(i in 1:nrow(airports)){
  gexfstr <- paste(gexfstr,airports$nodes[i],"\n",sep="")}
gexfstr <- paste(gexfstr,'</nodes>\n','<edges count="',as.character(nrow(routes)),'">\n',sep="")


### append edges and print to file
for(i in 1:nrow(routes)){
  gexfstr <- paste(gexfstr,routes$edges[i],"\n",sep="")}
gexfstr <- paste(gexfstr,'</edges>\n</graph>\n</gexf>',sep="")
writeLines(gexfstr, fileConn)
close(fileConn)