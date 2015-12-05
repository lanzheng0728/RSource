cf <- read.table(file="cfmd.txt",sep="\t",header=FALSE)
cf.df <- data.frame(name=as.character(cf$V1),
                    date=as.Date(as.character(cf$V2),"%Y/%m/%d",),
                    city=as.character(cf$V3),
                    lat=cf$V4,
                    lon=cf$V5,
                    addr=as.character(cf$V6),stringsAsFactors=FALSE                    
                    )
table(is.na(cf.df))
names(cf.df)
dim(cf.df)[2]
cf.df$node=NULL

for (i in 1:dim(cf.df)[2])
{
 cf.df$node <- paste(cf.df$node,"<",names(cf.df)[i],">",cf.df[,i],"</",names(cf.df)[i],">\n",sep="") 

}
cf.df$node <- paste("<cf>\n",cf.df$node ,"</cf>\n")

xml <- NULL
for( j in 1:dim(cf.df)[1]){
  xml <- paste(xml,cf.df[j,]$node,sep="")
}

xml <- paste("<cfs>\n",xml,"</cfs>\n",sep="")
fileConn<-file("output.xml")
writeLines(xml, fileConn)
close(fileConn)