install.packages("readr") #Instalación de la librería readr
file.choose() #Selección de carpeta
ruta2<-"C:\\Users\\PC\\Documents\\Encuesta sobre consumo de plástico (respuestas).xlsx" #Se guarda en una variable la ubicación del archivo
encuesta2<-read_xlsx(ruta2) #Se pasa como parámetro la variable que contiene la ubicación del archivo  
View(encuesta2) #Se muestra la tabla
#CAMBIO DE NOMBRE DE VARIABLES
names(encuesta2)<-c("fecha","correo","genero","nombre","edad","provincia","nivel_estudios","frecuencia_uso_plastico","uso_productos_plasticos","consciencia_ambiental","interes_reciclaje","separa_residuos","puestos_reciclaje_cerca_domicilio","dispuesto_llevar_residuos","interes_servicio_tercerizado_recolector","sistema_deposito_envases_en_supermercados","retribucion_economica_por_reciclar","predisposicion_reciclaje_por_retribucion","interes_app_reciclaje")
View(encuesta2) #Se muestra la tabla con las nuevas variables asignadas en names
#ELABORACION DE TABLAS PARA ALGUNAS VARIABLES
table(encuesta2$frecuencia_uso_plastico) #Se crea una tabla desde una variable
View(table(encuesta2$frecuencia_uso_plastico)) #Se muestra la tabla
#GRAFICO
barplot(table(encuesta2$frecuencia_uso_plastico),col = "black", main = "¿Con qué frecuencia usa plástico?",xlab = "Frecuencia", ylab = "Cantidad", ylim = c(0,60))
table(encuesta2$genero) #Se crea una tabla desde una variable
View(table(encuesta2$genero)) #Se muestra la tabla
color = c("pink", "lightblue") #Se asignan colores a la variable color
barplot(table(encuesta2$genero), main = "Género",xlab = "Géneros", ylab = "Cantidad", col= color, ylim = c(0,70))
table(encuesta2$nivel_estudios) #Se crea una tabla desde una variable
View(table(encuesta2$nivel_estudios)) #Se muestra la tabla
barplot(table(encuesta2$nivel_estudios), main = "Nivel de estudios",xlab = "Niveles", ylab = "Frecuencia", col= "black", ylim = c(0,60))
table(encuesta2$consciencia_ambiental)
View(table(encuesta2$consciencia_ambiental))
color2 = c("red3", "green3", "blue3") #Se asignan colores a la variable color2
barplot(table(encuesta2$consciencia_ambiental), main = "Consciencia del impacto ambiental",xlab = "Opciones", ylab = "Frecuencia", col= color2, ylim = c(0,100))
#Se crean clases para trabajar con las edades
rango_edades <- c(50,29,29,31,33,38,41,31,43,31,29,34,29,26,38,29,51,33,34,47,27,34,29,21,34,35,31,31,31,21,28,29,45,28,29,36,44,22,22,32,29,23,48,41,68,36,25,35,35,25,23,24,42,41,
                  43,48,67,25,24,49,58,24,22,45,50,56,48,21,60,69,25,45,47,41,38,22,20,31,34,42,40,18,18,27,25,27,40,35,49,40,46,41,36,25,18,45,37,31,50,31,34)
edades_ordenadas <- sort(rango_edades) #Se ordenan los valores de edad y se guardan en una variable
edades_ordenadas #Se muestran las edades ordenadas
n <- length(rango_edades) #Se guarda en una variable el total de edades
n #Se muestra el total de edades
rango <- range(rango_edades)
rango
amplitud <- diff(rango)
amplitud
rangointervalos <- amplitud / 5
rangointervalos
clases<-nclass.Sturges(rango_edades)
clases
max = max(rango_edades)
min = min(rango_edades)
rango2 <- max - min
rango2
longitud <- rango2 / clases
longitud
tabla.intervalos1 <- transform(table(cut(rango_edades, breaks = clases)))
tabla.intervalos1
#GRAFICO
ggplot(data = encuesta2) + geom_col(mapping = aes(x=edad, y=interes_app_reciclaje))
#GRAFICO CIRCULAR
count <- c(48, 28, 24) #Se guarda en una variable el vector con los valores que va a tener el gráfico
pie(count) #Se llama a la función pie para hacer un diagrama circular
pie(count, labels = paste0(count, "%")) 
color_1 <- c("bisque3", "aquamarine4", "orangered3")
pie(count, labels = count, col = color_1, density = 30, angle = 30) #Sombreado 
pie(count, labels = count, col = color_1, density = 30, angle = 30, main = "¿Separa los residuos plásticos del resto de los residuos?")
legend("topright", c("A veces", "Nunca", "Siempre"), cex = 0.8, fill = color_1)
#GRAFICO CIRCULAR
valores <- c(54, 38, 9)
pie(valores)
pie(valores, labels = paste0(valores, "%"))
color_3 <- c("lightgreen", "thistle4", "coral1")
pie(valores, labels = valores, col = color_3, density = 30, angle = 30, main = "¿Tiene puestos de reciclaje cercanos a su domicilio?")
legend("topright", c("No", "Sí", "No sé"), bty = "n", fill = color_3) #bty = "n" -> leyenda sin bordes
#GRAFICO CIRCULAR
count <- c(72, 12, 16) #Se guarda en una variable el vector con los valores que va a tener el gráfico
pie(count) #Se llama a la función pie para hacer un diagrama circular
pie(count, labels = paste0(count, "%")) 
color_1 <- c("coral", "darkorchid", "chartreuse")
pie(count, labels = count, col = color_1, density = 30, angle = 30) #Sombreado 
pie(count, labels = count, col = color_1, density = 30, angle = 30, main = "¿Llevaría los residuos a un punto de reciclaje cercano?")
legend("topright", c("Si", "No", "No sé"), cex = 0.8, fill = color_1)
#GRAFICO CIRCULAR
valores <- c(80, 13, 7)
pie(valores)
pie(valores, labels = paste0(valores, "%"))
color_3 <- c("darkolivegreen1", "deeppink", "darkgoldenrod1")
pie(valores, labels = valores, col = color_3, density = 30, angle = 30, main = "¿Preferiría un servicio tercerizado?")
legend("topright", c("Si", "No", "No sé"), bty = "n", fill = color_3) #bty = "n" -> leyenda sin bordes
#GRAFICO CIRCULAR
count <- c(90, 5, 5) #Se guarda en una variable el vector con los valores que va a tener el gráfico
pie(count) #Se llama a la función pie para hacer un diagrama circular
pie(count, labels = paste0(count, "%")) 
color_3 <- c("blue4", "cyan1", "darksalmon")
pie(count, labels = count, col = color_3, density = 30, angle = 30) #Sombreado 
pie(count, labels = count, col = color_3, density = 30, angle = 30, main = "¿Depósito de envases en Supermercados cercanos?")
legend("topright", c("Si", "No", "No sé"), cex = 1, fill = color_3)
#GRAFICO BARRAS
color2 = c("red3","green1", "blue3", "yellow2") #Se asignan colores a la variable color2
barplot(table(plastico$`¿Qué opina de tener una retribución económica por cada envase depositado en estos sistemas de depósito? (Cashback, Subsidio en Impuestos, etc)`), main = "¿Retribución económica por envase depositado?",xlab = "Opciones", ylab = "Frecuencia", col= color2, ylim = c(0,100))
#GRAFICO BARRAS
color_3 = c("blueviolet","aquamarine", "deeppink4") #Se asignan colores a la variable color3
barplot(table(plastico$`Si tuviese una retribución económica por reciclar ¿estaría mejor predispuesto a hacerlo?`), main = "¿Retribución por reciclar, mejora predisposición?",xlab = "Opciones", ylab = "Frecuencia", col= color_3, ylim = c(0,100))
