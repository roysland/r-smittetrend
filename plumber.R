# plumber.R

#* @apiTitle Plumber Example API

#* Return trend of smittecases
#* @param p Antall personer i kommunen
#* @param x2 Antall smittede forrige to uker
#* @param y antall smittede denne uke
#* @get /smittetrend
function(p,x2,y){
  p <- as.numeric(p)
  x2 <- as.numeric(x2)
  y <- as.numeric(y)
  alpha.pri <- 1 
  beta.pri <- 7*p/10000 

  alpha.post <- alpha.pri + x2
  beta.post <- 1.0/(2+1/beta.pri)
  

  nbin.size <- alpha.post
  nbin.prob <- 1.0/(1.0+beta.post)
  

  grense.opp <- qnbinom(p=0.95,size=nbin.size,prob=nbin.prob)
  grense.ned <- qnbinom(p=0.05,size=nbin.size,prob=nbin.prob)
  

  if(y<grense.ned) {
    list(msg = "Avtagende trend.")
  } else if (y>=grense.ned & y<grense.opp) {
      list(msg = " Stabil trend.")
  } else if (y>=grense.opp) {
    list(msg = " Okende trend.")
  }
}