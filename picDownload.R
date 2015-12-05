url <- "http://manhua.dmzj.com/cikexintiao/11917-2.shtml"
myHttpHeader <- c(
  "Accept"  = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
  "Accept-Language" = "en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4",
  "Cookie"="bdshare_firstime=1426761184044; doHit=6811_11917; CNZZDATA1000465408=1737651556-1426759350-http%253A%252F%252Fwww.baidu.com%252F%7C1426775562; CNZZDATA1000465515=90445961-1426756466-http%253A%252F%252Fwww.baidu.com%252F%7C1426772635; Hm_lvt_645dcc265dc58142b6dbfea748247f02=1426761184,1426775095; Hm_lpvt_645dcc265dc58142b6dbfea748247f02=1426777761; Hm_lvt_2a383b4e7c51ec3e9a63af7aa003a6e2=1426761184,1426775095; Hm_lpvt_2a383b4e7c51ec3e9a63af7aa003a6e2=1426777761",
  "Host"="manhua.dmzj.com",
  "Cache-Control"="max-age=0",
  "Referer"="http://manhua.dmzj.com/cikexintiao/11917-20.shtml",
  "Connection"= "keep-alive",
  "User-Agent"= "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36"
  )

d = debugGatherer()
tmp <- getURL(url, header= TRUE, debugfunction=d$update,verbose = TRUE， httpheader=myHttpHeader)

patten <- "http://images.dmzj.com/c"
reg <- regexpr(patten,tmp)

url <- "http://images.dmzj.com/c/%E5%88%BA%E5%AE%A2%E4%BF%A1%E6%9D%A1/CH01/021.jpg"


library(RCurl)
chapter <- "03"
pages <- 1:26
site <- paste("http://images.dmzj.com/c/%E5%88%BA%E5%AE%A2%E4%BF%A1%E6%9D%A1/",chapter,"/",sep = "")
if(!file.exists(chapter)){
  dir.create(chapter)
}
picHttpHeader <- c(
  "Accept"="image/webp,*/*;q=0.8",
  "Accept-Encoding"="gzip,deflate,sdch",
  "Accept-Language"="en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4",
  "Connection"="keep-alive",
  "Cookie"="Hm_lvt_645dcc265dc58142b6dbfea748247f02=1426761184,1426775095; Hm_lpvt_645dcc265dc58142b6dbfea748247f02=1426781153",
  "Host"="images.dmzj.com",
  "Referer"="http://manhua.dmzj.com/cikexintiao/",
  "User-Agent"="Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36"
)
for(i in pages){
  filename <- paste(formatC(i,width=3,flag="0"),".jpg",sep = "")
  url <- paste(site,filename,sep ="")
  bin <- getBinaryURL(url, httpheader=picHttpHeader)
  con <- file(paste(chapter,"\\",filename,sep = ""), open = "wb")
  writeBin(bin, con)
  close(con)
}

# site <- paste("http://images.dmzj.com/c/%E5%88%BA%E5%AE%A2%E4%BF%A1%E6%9D%A1/",chapter,"/",sep = "")
# filename <- paste(formatC(1,width=3,flag="0"),".jpg",sep = "")
# url <- paste(site,filename,sep ="")
# bin <- getURL(url, httpheader=picHttpHeader,.mapUnicode = TRUE)
# #bin <- getURL(url)
# con <- file(paste(chapter,"\\",filename,sep = ""), open = "wb")
# writeBin(bin, con)
# close(con)




