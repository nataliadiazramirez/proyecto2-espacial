library(spatstat)

# Cargamos los datos

database <- read.csv("database.csv")

# Especificamos la region

# database <- database[database$Region == "MÃ©xico and Central America", ]

# Definimos la ventana

wndw <- owin(x = c(min(database$Longitude), max(database$Longitude)),
             y = c(min(database$Latitude), max(database$Latitude)))

# Hacemos el objeto ppp

spec <- ppp(x = database$Longitude, database$Latitude, window = wndw)

# Hagamos los gráficos

plot(spec)
plot(density(spec))

# Función K

rip.k <- Kest(spec)
plot(rip.k)

# Función L

rip.l <- Lest(spec)
plot(rip.l)

# Simulación de Poissones

set.seed(1) # Establecemos una semilla para reproducibilidad
env <- envelope(spec, fun = Lest, nsim = 999)
plot(env)

# Cantidad de puntos cercanos según aumenta el radio

l.iso <- rip.l$iso
l.theo <- rip.l$theo
l.dev <- l.iso - l.theo
plot(l.dev ~ rip.l$r, type = "l")

# Modelo con solo x y y

fit1 <- ppm(spec, ~ x + y)
plot(fit1, how = "image", se = FALSE, pause = FALSE)
persp(predict(fit1), theta = 270)

# Modelo con un polinomio de grado 2 con x y y

fit2 <- ppm(spec, ~ polynom(x, y, 2))
plot(fit2, how = "image", se = FALSE, pause = FALSE)
persp(predict(fit2), theta = 180)



