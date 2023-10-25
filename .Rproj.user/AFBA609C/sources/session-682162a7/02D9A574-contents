#https://www.google.com/search?q=rese%C3%B1a+de+la+pelicula+barbie&rlz=1C1FCXM_pt-PTCO1011CO1011&oq=rese%C3%B1a+de+la+pelicula+barbie+&gs_lcrp=EgZjaHJvbWUyBggAEEUYOTIHCAEQABiABDIICAIQABgWGB4yCAgDEAAYFhgeMggIBBAAGBYYHjIKCAUQABgKGBYYHjIICAYQABgWGB7SAQg1NDYxajBqN6gCALACAA&sourceid=chrome&ie=UTF-8
sentimientos<- c(
  "Definitivamente Barbie es la mejor Pel?cula que se ha estrenado en este a?o,", 
  "realmente merece demasiado ?xito como el que tiene ahora mismo y mucho m?s. ",
  "No esperaba menos de Greta, supo capturar la esencia de lo que es el machismo y misoginia actualmente, ",
  "de como la vida real puede llegar a ser tan abrumadora y el hecho de que haya tocado temas como la muerte o la crisis existencial,",
  "crecer mirar al pasado, los cambios, el autoestima y la necesidad y el deseo de encontrarse a uno mismo cuando abres los ojos y te das cuenta",
  "de que no eres todo lo que otros dicen que eres y te hicieron creer que eras. Es tan magn?fico como una pel?cula puede ser incre?blemente maravillosa",
  "y de como su mensaje puede ser tan profundo, aunque soy chico puedo decir que realmente me identifica demasiado la pel?cula, me sent? demasiado conmovido",
  "y sin exagerar estuve llorando aproximadamente dos horas y med?a despu?s de acabarla.",
  "Ame mucho a Allan, es como yo, es un chico que no siente confort con otros(los KEN'S) pero siente seguridad con las Barbies y",
  "que incluso aveces se siente ignorado o diferente al resto. Otra cosa es que realmente aunque la trama diera un cambio a Ken se mes dif?cil tolerarlo, ",
  "es un personaje que considero desde principio a fin demasiado problem?tico.",
  "Ame demasiado los di?logos de la se?ora y de Barbie, sus di?logos me pusieron demasiado sencible.",
  "Para las personas que se quejan de que los ni?os no entendieron la pel?cula o que se aburr?an o que la pel?cula no esa para ni?os,",
  "eso ya era demasiado claro, desde antes se sab?a que no era para ni?os por los teaser, promociones, entrevistas y incluso por la clasificaci?n.",
  "Los otros hombres que dicen que Barbie busca humillar, o crear una imagen falsa pues realmente no entendieron la pel?cula, y se est?n v?ctimizando", 
  "(que no es nada nuevo, y lo digo siendo yo un chico) y tambi?n las chicas que est?n diciendo que es mala por x motiv?", 
  "(algunas personas que lo hacen por aprobaci?n) realmente dan a entender que les han lavado la mente con ideas demasiado repugnantes.",
  "Las personas que fueron solo para criticar el feminismo y el mensaje progre es de las cosas m?s vergonzosas en una persona, ojal? tuviesen la madurez y ",
  "el poder profundizar en temas tan oscuros y reales y aceptarlos, porque es cierto, Barbie puede y es todo y Ken no es m?s que Ken.",
  "Definitivamente Barbie es la mejor pel?cula del a?o, y todo de ella esta muy bien hecho.",
  "Ojal? hubiesen agregado a Skypper, Stacie, Chelsea y Raquel, pero no quita lo maravilloso que fue.",
  "Barbie es una experiencia ?nica para ser uno mismo, saber que incluso la persona m?s perfecta puede sufrir y que por m?s que queramos ser ",
  "perfecto podemos ser mucho m?s que perfecto, podemos ser imperfectos y sentir felicidad y tristeza, frustraci?n, podemos vivir y sentirnos muertos, y a?n as? ",
  "la idea de no querer ser parte de la idea, m?s que eso ser parte de quienes crean est?s ideas y aportan de una forma tan ?nica y distinta, con barbie se puede ser lo que ",
  "quieras ser y es realmente inspiradora, si no lo entendieron pues realmente?Que se puede hacer?"
  )


sentimientos<- tolower(sentimientos)
sentimientos<- gsub("[[:punct:]]", "", sentimientos)
sentimientos<- strsplit(sentimientos, " ")
palabras<- c("es", "la", "de", "y","pero","al","dos","lo","das","uno", "una","su","ser","tan","soy","sin", "el","o","a","e","i","u","no", "fue", "un", "en", "los","era","por","para","se","ya","eso","que","me","con","las","como","te")
sentimientos<- lapply(sentimientos, function(sentimientos) sentimientos [!sentimientos %in% palabras] )
sentimientos<- sapply(sentimientos, paste, collapse = " ")
positivo<-c("emocionante", "bien", "excepcional", "bueno", "excelente","magn?fico","maravillosa","?xito","mejor")
negativa<-c("aburrida","abrumadora","muerte","mal","peor")
sentimientos1<-numeric(length(sentimientos))

for (i in 1:length(sentimientos)){
  palabras1<- unlist(strsplit(sentimientos[i], " "))
  sentimientos1[i]<- sum(palabras1 %in% positivo) - sum(palabras1 %in% negativa)
}
sentimiento_df<- data.frame(
  Resena = sentimientos,
  Sentimiento = sentimientos1
)

  
eti <- c("emocionante", "bien", "excepcional", "bueno", "excelente","magn?fico","maravillosa","?xito","mejor","aburrida","abrumadora","muerte","mal","peor","llorar","reir","felicidad","tristeza","sonreir","sonrojarse", "maravilloso", "fabuluso", "explendido","hermoso","feo")

barplot(sentimiento_df$Sentimiento, names.arg = eti,
        main="Sentimientos en peliculas",
        xlab = "Resena" ,
        ylab= "Sentimiento")


