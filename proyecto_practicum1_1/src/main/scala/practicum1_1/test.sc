val mapa = List(Map("id" -> "123", "name"-> "cris", "lastname"-> "jimenez"), Map("id" -> "345", "name"-> "miguel", "lastname"-> "robles"))
//imprimir(mapa)
mapa.find(_("id") == ("345")).map(_("name")).get


val lista = List("a", "b", "c")
val resultado = lista.mkString(",", ";")

println(resultado)
// Salida: "a, b, c"

val listaDeMapas: List[Map[String, Any]] = List(Map("a" -> 1, "b" -> "texto"), Map("c" -> 2, "d" -> 3.14), Map("e" -> true))

val numeroDeMapas: Int = listaDeMapas.count(_.isInstanceOf[Map[String, Any]])

println(s"El número de mapas en la lista es: $numeroDeMapas")

val lista1 = List(1, 2, 3, 4)
val lista2 = List(5, 6, 7, 8)

// Usando zip y map para sumar los elementos correspondientes
val resultado = lista1.zip(lista2).map { case (a, b) => a + b }

println(resultado)
