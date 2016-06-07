hist <- 0
dart <- 100000
for(i in 1:dart)
{
  x <- runif(1)
  y <- runif(1)
  if(sqrt(x^2+y^2)<1)
    hist <- hist+1
}
pi <- 4*hist/dart
pi