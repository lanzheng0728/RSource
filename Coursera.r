library(RCurl)
myHttpHeader <- c(
  "Accept"="*/*",
  "Accept-Encoding"="gzip,deflate,sdch",
  "Accept-Language" = "en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4",
  "Cookie"="__204u=4073045092-1426558443104; __204r=; ip_origin=CN; ip_currency=USD; ab-experiments-session=specializations_subhead_field; ab-experiments-user=in_class_qqs; __400v=3f2ca380-4a61-4e9d-f77b-5bd88134cce2; __utmt=1; __utma=158142248.1924142251.1426558445.1427945702.1427948796.3; __utmb=158142248.4.10.1427948796; __utmc=158142248; __utmz=158142248.1426558445.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __400vt=1427948821160; csrftoken=k7BTlyHp9qizvh9I6cMJJWHx; csrf2_token_CNoqBD9P=7lrOJbTZOdGMxJ9lodlL4Ez1",
  "Host"="www.coursera.org",
  "Cache-Control"="max-age=0",
  "Origin"="https://www.coursera.org",
  "Referer"="https://www.coursera.org/",
  "Connection"= "keep-alive",
  "Content-Length"="85",
 "Content-Type"="application/json; charset=UTF-8",
 "User-Agent"= "Mozilla/5.0 (Windows NT 6.2; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.117 Safari/537.36",
  "X-CSRF2-Cookie"="csrf2_token_CNoqBD9P",
  "X-CSRF2-Token"="7lrOJbTZOdGMxJ9lodlL4Ez1",
  "X-CSRFToken"="k7BTlyHp9qizvh9I6cMJJWHx",
  "X-Requested-With"="XMLHttpRequest"
)

cHandle <- getCurlHandle(debugfunction=d$update, verbose=T,
                         ssl.verifyhost=F, ssl.verifypeer=F, followlocation=T)
d = debugGatherer()
postinfo <- c(email="lanzheng0728@live.com",password="l0vetina",code="",webrequest="true")
# forward=&jumpurl=http%3A%2F%2Fcos.name%2Fbbs%2F&step=2&lgt=0&pwuser=yourname&pwpwd=yourpw&hideid=0&cktime=31536000
# myPost <- function(x){
#   post <- scan(x,what="character",quiet=TRUE,sep="\n")
#   abcd=strsplit(post,"&")[[1]]
#   abc=gsub("(^.*)(=)(.*$)","\\3",abcd)
#   abcnames=gsub("(^.*)(=)(.*$)","\\1",abcd)
#   names(abc)=abcnames
#   return(abc)
# }
# postinfo <- myPost("clipboard")

temp <- postForm("https://www.coursera.org/api/login/v3",
                 httpheader=myHttpHeader,
                 .params=postinfo,
                 .opts=list(cookiefile=""),
                 curl=cHandle,style="post")

getCurlInfo(cHandle)[["cookielist"]]
