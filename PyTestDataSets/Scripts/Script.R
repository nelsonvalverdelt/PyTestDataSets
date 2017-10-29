#--------------------- CONFIGIGURACION -------------------------------
setwd("../PyTestDataSets") ##Cargamos el diretorio raiz de mi proyecto
getwd() ## Devolvemos la ruta seteada
source("Scripts/Config.R") ## Cargamos nuestro Script Config.R
#------------------------------------------------------------
## Ojo: Reemplazar los "-" por "." dentro de las consultas
#------------------------------------------------------------

autoData <- read.csv("Datasets/AutoData.csv") #Cargamos CSV del autoData
facebookData <- read.csv("Datasets/FacebookData.csv") #Cargamos CSV de FacebookData 

#----------------------CONSULTAS--------------------------------


## 1 - Modelos de autos que causaron perdidas

result <- autoData %>% filter(normalized.losses == "?") %>% distinct(make)
View(result)


## 2 - Modelo de Autos menos peligrosos

lista_autos <- autoData %>% group_by(symboling, make, price, fuel.type, body.style) %>% filter(symboling <= 0) ## elegimos el valor 2 o mayor que implica que son peligrosos
lista_autos_desc <- lista_autos %>% select(symboling, make, price, fuel.type, body.style) %>% arrange(desc(price))
View(lista_autos_desc)

## 3 - Precio / Precios de los autos más rápidos

list_rpm <- autoData %>% select(peak.rpm) %>% filter(peak.rpm != "?") # eliminamos las filas con datos "?"
distinct_rpm <- list_rpm %>% distinct(peak.rpm) #eliminamos los datos duplicados de peak.rpm
max_rpm <- data.frame(distinct_rpm %>% filter(as.numeric(peak.rpm) == max(as.numeric(peak.rpm)))) #filtro del mayor dato de peak.rpm
max_rpm <- max_rpm[,1] #Obtenemos el dato máximo del data.frame
auto_rapido <- autoData %>% filter(peak.rpm != "?" & peak.rpm == max_rpm) %>% select(make, price, fuel.type, num.of.doors,body.style, peak.rpm)
View(auto_rapido)

## 4 - Mostrar los 3 modelos de Autos más avendidos

list_make <- autoData %>% group_by(make) %>% count(make)
list <- data.frame(list_make %>% arrange(desc(n)))
View(list[1:3,])

## 5 - Cantidad de modelo de autos que utilicen gas

filtro_modelo <- autoData %>% filter(fuel.type == "gas") %>% select(make, fuel.type) %>% distinct(make, fuel.type)
View(filtro_modelo)

## 6 - Cantidad de tipos autos para uso familiares

num_doors = c("one", "two", "three", "four")
autos_familiares <- autoData %>% select(make, num.of.doors) %>% filter(num.of.doors == num_doors[4]) %>% count(make)
View(autos_familiares)

## 7 - Modelos de autos más baratos
filtro_automobiles <- autoData %>% select(price) %>% filter(price != "?") %>% distinct(price) # eliminamos las filas con datos "?" y los precios duplicados
menor_precio <- data.frame(filtro_automobiles %>% arrange(price))
menor_precio <- menor_precio[1, 1] # tomamos el primer numero de la fila ya que es ascendente la lista de nuestro dara.frame
automobil_barato <- autoData %>% filter(peak.rpm != "?" & price == menor_precio) %>% select(make, price, fuel.type, num.of.doors, body.style, peak.rpm)
View(automobil_barato)

## 8 - Modelos de autos no apto para uso familiares
autos_no_familiar <- autoData %>% select(make, num.of.doors) %>% filter(str_detect(num.of.doors, "two")) %>% distinct(make)
View(autos_no_familiar)

## 9 - Autos muy arriesgados con precios muy altos

lista_autos <- autoData %>% select(symboling, make, price, fuel.type, body.style) %>% filter(symboling >= 2) ## elegimos el valor 2 o mayor que implica que son peligrosos
lista_autos_desc <- lista_autos %>% select(symboling, make, price, fuel.type, body.style) %>% arrange(desc(price))
View(lista_autos_desc[1:8,])

## 10 - Automoviles al alcance de tu bolsillo

filtro_automobiles <- autoData %>% select(make, price) %>% filter(price != "?") # eliminamos las filas con datos "?"
precio_a_pagar = 10500;
lista_automobiles <- filtro_automobiles %>% filter(as.numeric(price) <= precio_a_pagar) %>% select(price)
View(lista_automobiles)


# ---------------------------------- CONSULTAS FACEBOOK ------------------------------------------------

## 11 - Cantidade de estados y fotos en diciembre

facebook <- facebookData %>% group_by(Type, PostMonth) %>% count(Type) %>% filter(PostMonth == 12)
mutateFacebook <- data.frame(facebook %>% mutate(Tipo=Type, Mes=PostMonth, CantidadTipos=n))
View(mutateFacebook %>% select(Tipo,Mes,CantidadTipos))


## 12 - categorias con más likes tiene
categorias <- c("Educacion", "Deporte", "Arte y Ocio")
facebook <- facebookData %>% select(Category, like) %>% count(Category) %>% mutate(categoria = categorias[Category], likes=n)
View(facebook %>% select(likes, categoria))

## 13 - categorias con más compartidos tiene en el primer mes
meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
categorias <- c("Educacion", "Deporte", "Arte y Ocio")

facebook <- facebookData %>% select(Category, PostMonth, share) %>% group_by(PostMonth) %>% filter(PostMonth == 1) %>% count(Category)
resultado_mutate <- data.frame(facebook %>% mutate(compartidos = n, mes = meses[PostMonth], categoria = categorias[Category]))
View(resultado_mutate %>% select(mes, categoria, compartidos))

## 14 cuantas publicaciones fueron pagadas y cuantas no fueron pagadas

facebook <- facebookData %>% select(Paid) %>% count(Paid) %>% filter(Paid != "NA")
resultado_mutate <- facebook %>% mutate(pago = Paid, publicaciones = n)
resultado <- data.frame(resultado_mutate %>% select(pago, publicaciones))
resultado[1, 1] <- "NO"
resultado[2,1] <- "SI"
View(resultado)

## 15 - Descripcion de la Cantidad de Publicaciones que se realizaron por las mañanas

facebook <- data.frame(facebookData %>% select(Type, Category, PostHour) %>% filter(PostHour <= 12)) 
facebook[, 3] <- "manana" # cambiamos las horas del 1 al 12 a mañana para contabilizarlo con un count y no los cuento por cada hora
facebook <- data.frame(facebook %>% select(Type, Category, PostHour) %>% count(PostHour))
facebook_mutate <- facebook %>% mutate(Turno = PostHour, Publicaciones = n)
View(facebook_mutate %>% select(Turno, Publicaciones))

## 16 - publicacion del tipo Photo que se realizaron en enero y febrero (Vaciones Laborales) para las comprobaciones de sobrecarga de data de fotografias
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
tipos <- "Photo"
facebook <- facebookData %>% select(Type, Category, PostMonth) %>% group_by(PostMonth) %>% filter(str_detect(Type, "Photo"), PostMonth <= 2) %>% count(Type)
facebook_mutate <- data.frame(facebook %>% mutate(Mes = meses[PostMonth], Tipo = Type, Publicaciones = n))
View(facebook_mutate %>% select(Mes, Tipo, Publicaciones))

## 17 - Que tipo de publicaciones fueron pagadas

facebook <- data.frame(facebookData %>% select(Type, Paid) %>% group_by(Type) %>% filter(Paid == 1) %>% count(Paid))
facebook[,2] <- "Si" #reemplazadamos los 1 = true por un Si
facebook_mutate <- facebook %>% mutate(Tipo=Type, Pago=Paid, Publicaciones=n)
View(facebook_mutate %>% select(Tipo, Pago, Publicaciones))

## 18 - Reporte del tipo que realizaron pocas publicaciones
facebook <- data.frame(facebookData %>% select(Type) %>% count(Type))
facebook <- facebook %>% arrange(n) #} Ordenamos de forma ascendente para posteriormente utilizar la primera fila como menor dato
facebook_mutate <- data.frame(facebook %>% mutate(Tipo=Type, Publicaciones = n))
resultado <- data.frame(facebook_mutate %>% select(Tipo,Publicaciones))
View(resultado[1,]) # retornamos la primera fila

## 19 - Tipo de publicaciones que dieron menores ingresos
facebook <- facebookData %>% select(Type, Paid) %>% group_by(Type) %>% count(Paid) %>% filter(Paid == 0)
facebook_mutate <- facebook %>% mutate(Tipo=Type, CantidadPagos=n)
facebook_ordenar <- data.frame(facebook_mutate %>% arrange(CantidadPagos))
resultado_facebook = facebook_ordenar %>% select(Tipo, CantidadPagos)
View(resultado_facebook[1,])

## 20 - Reporte del tipo de publicacion y el Mes donde se adquirieron mas ingresos
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
facebook <- data.frame(facebookData %>% select(Type, Paid, PostMonth) %>% group_by(Type,PostMonth) %>% filter(PostMonth >= 11, Paid==1) %>% count(Type))
facebook_mutate <- facebook %>% mutate(Tipo = Type, CantidadPagos = n, Mes = meses[PostMonth])
facebook_resultado <- facebook_mutate %>% select(Tipo, CantidadPagos, Mes)
resultado <- facebook_resultado %>% arrange(desc(CantidadPagos)) #Ordenamos para tomar los campos que requerimos desde los mayores a menores
#seleccionamos dos ultimas filas para el ejemplo y lo mostramos
View(resultado[1:2,])