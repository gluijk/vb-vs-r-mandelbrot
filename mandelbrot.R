# Visual Basic para Aplicaciones vs R
# www.overfitting.net
# https://www.overfitting.net/2017/07/visual-basic-para-aplicaciones-contra-r.html

library(tiff)
Gamma = 2.2  # Curva Gamma para deslinealizar imágenes de salida

ITERA = 100  # Iteraciones
WIDTH = 1920  # FUll HD
HEIGHT = 1080


# MANDELBROT CON BUCLES
Mandel = array(0, c(WIDTH, HEIGHT))

HALFHEIGHT = HEIGHT / 2

for(px in 1:WIDTH) {
  x0 = px / HALFHEIGHT - 2.25
  for(py in 1:HEIGHT) {
    
    y0 = py / HALFHEIGHT - 1
    x = x0
    y = y0
    c = 0
    i = 0
    
    while (c <= 4 && i < ITERA) {   
      x1 = x * x - y * y + x0
      y1 = 2 * x * y + y0
      x = x1
      y = y1
      c = x1 * x1 + y1 * y1
      i = i + 1
    }
    if(i < ITERA) Mandel[px, py] = i / (ITERA - 1)

  }
}

writeTIFF(Mandel ^ (1 / Gamma), "mandelbrotbucles.tif", bits.per.sample=16, compression="LZW")


# MANDELBROT MATRICIAL
xmin = -2.248148
xmax = 1.305556

ymin = -(xmax - xmin) * HEIGHT / WIDTH / 2
ymax = -ymin

x = seq(xmin, xmax, length.out = WIDTH) 
y = seq(ymin, ymax, length.out = HEIGHT) 
c = outer(x, y * 1i, FUN="+") 
z = array(0, c(length(x), length(y)))
Mandel = z

for (i in 1:ITERA) {
  indices = which(Mod(z) <= 2)
  z[indices] = z[indices] ^ 2 + c[indices]
  Mandel[indices] = Mandel[indices] + 1
} 

Mandel[Mandel == ITERA] = 0
Mandel = Mandel / max(Mandel)

writeTIFF(Mandel ^ (1 / Gamma), "mandelbrotmatricial.tif", bits.per.sample=16, compression="LZW")



