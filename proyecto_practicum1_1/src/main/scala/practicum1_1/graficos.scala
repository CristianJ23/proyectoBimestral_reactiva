package practicum1_1
import com.github.tototoshi.csv.*

import java.io.File
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.{DataSource, HistogramData}
import org.nspl.saddle.{barplotHorizontal, barplotVertical, rasterplotFromFrame}
import org.saddle.spire.random.GlobalRng.nextDouble
import org.saddle.{Frame, Series}

import scala.language.postfixOps

object graficos {
  def main(args: Array[String]): Unit = {
    val pathDataFile: String = "C:\\tercer_semestre\\practicum1.1_2023\\dsAlineacionesXTorneo.csv"
    val reader = CSVReader.open(new File(pathDataFile))
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()
    // archivo torneos y goles
    val pathGoles: String = "C:\\tercer_semestre\\practicum1.1_2023\\dsPartidosYGoles.csv"
    val reader2 = CSVReader.open(new File(pathGoles))
    val contentFileGoles: List[Map[String, String]] = reader2.allWithHeaders()
    reader.close()
    reader2.close()

    println("¿Cuál es el promedio de penaltys por torneo?")
    //val penales = penaltysPromedio(contentFileGoles)
    //println(penales)
    //graficaPenaltysPromedio(penales)

    println("¿cual es el jugador de menor edad, por cada mundial?")
    //val jugadorMenor = menorEdad(contentFile)
    //println(jugadorMenor)
    //graficaMenorJugadorMundial(jugadorMenor)

    println("grafico densidad de minutos en q realizaron los goles")
    //val minutos = minutosGoles(contentFileGoles)
    //print(minutos)
    //graficaMinutos(minutos)

    println("histograma años de nacimiento")
    //val anios = anioNacimiento(contentFile)
    //print(anios)
    //graficaAnio(anios)

    println("numero goles marcados y recibidos del equipo T-01")
    //val puntuacion = equiposGoles(contentFileGoles)
    //print(puntuacion)
    //golesEquipos(puntuacion)

    println("numero de equipos en los torneos")
    //val squads = squadsTorneos(contentFile)
    //print(squads)
    //bloxplot(squads)

    println("cantidad de hombre y mujeres historico")
    val sexos = hombreMujeres(contentFile)
    print(sexos)
    graficaAnio(sexos)

  }
//-------------------------******************************---------------------------
  def penaltysPromedio(data: List[Map[String, String]]) =
    val tournamentsUnique = data
    .map(_("matches_tournament_id")).distinct.sorted
    val penaltys = tournamentsUnique
    .map(torneo =>
      val lista = data.filter(_("matches_tournament_id") == torneo)
      .map(_("matches_penalty_shootout").toInt)
      val numero_total = lista.size
      val penaltys_hechos = lista.filter(_ == 1).size
      //println(s"${penaltys_hechos}  / ${numero_total}")
      val promedio = (penaltys_hechos.toDouble / numero_total) * 100
      val formattedPromedio = f"$promedio%.3f".toDouble
      (torneo, formattedPromedio)
    )
    penaltys
//---------------------------------*****************************---------------------------
  // ¿cual es el jugador de menor edad, por cada mundial?
  def menorEdad(data: List[Map[String, String]]) =
    val tournamentsUnique = data
      .map(_("squads_tournament_id")).distinct.sorted
    val playersUn = tournamentsUnique
      .map(torneo =>
        (torneo,
          data.filter(_("squads_tournament_id") == torneo).map(_("squads_player_id")))
      ).map(x => (x._1, x._2.distinct.sorted))

    val players = playersUn.map(jugador =>
      val birthDates = jugador._2.flatMap(playerId =>
        data.filter(d =>
          d("squads_player_id") == playerId && d.contains("players_birth_date")
        ).map(_("players_birth_date").trim).flatMap(x => limpiarnull(x).map(_.take(4).toInt))
      ).max
      (jugador._1, birthDates)
    )
    val menorJugador = players
      //.map(x => (x._1.substring(3, 7), x._2))
      .map(x => (x._1, (x._1.substring(3, 7).toInt - x._2).toDouble))
    menorJugador
//---------------------------------------**************************-------------------------------
  def minutosGoles(data: List[Map[String, String]]) =
    val lista = data.map(_("goals_minute_regulation"))
      .flatMap(limpiarnull(_)).map(_.toDouble)
    lista

//--------------------------*****************************--------------------------------
  def anioNacimiento(data: List[Map[String, String]])=
    val players = data
      .map(_("squads_player_id").trim).distinct
    val anios = players
      .map(player=>
        val anio = data.find(_("squads_player_id") ==player).map(_("players_birth_date"))
          .flatMap(limpiarnull(_)).map(_.take(4).toInt).getOrElse(0)
        (anio)
      )
    anios.distinct

//----------------------**********************************************-----------------------------------
  def equiposGoles(data: List[Map[String, String]])=
    val equipo = data.filter(_("matches_tournament_id") == "WC-2022")
      .map(eq =>
      val local = eq.getOrElse("matches_home_team_score", "").toDouble
      val away = eq.getOrElse("matches_away_team_score", "").toDouble
      (local, away)
      )
    equipo
//---------------------------------********************************------------------------
  def squadsTorneos(data: List[Map[String, String]])=
    val torneos = data.map(_("squads_tournament_id")).distinct.sorted
    val squads = torneos
      .map(torneo=>
      val equipos = data.filter(_("squads_tournament_id") == torneo)
        .map(_("squads_team_id")).size.toDouble
      (equipos)
      )
    squads

//-------------------------*************************----------------------------------
  def hombreMujeres(data: List[Map[String, String]])=
    val lista = data
    .map(fila=>
      fila("players_female").toInt
    ).map { case 0 => 1 case 1 => 2}
    lista
//-------------------------***************************------------------------
                              //graficas
//------------------------*************************-------------------------
  def graficaPenaltysPromedio(data: List[(String, Double)]) =
    // Crear la serie a partir de la lista de tuplas
    val series: Series[String, Double] = Series(data: _*)
    val bar1 = barplotHorizontal(
      series,
      //Series("a" -> (-2d), "b" -> (-1d), "c" -> 0d, "d" -> 1d, "e" -> 2d),
      color = RedBlue(-2, 2)
    )(par)
    pngToFile(new File("C:/tercer_semestre//practicum1.1_2023//graficosProyecto//imagen6.png"), bar1.build, width=1000)
//---------------------*****************************--------------------------
  def graficaMenorJugadorMundial(data: List[(String, Double)]) =
    // Crear la serie a partir de la lista de tuplas
    val series: Series[String, Double] = Series(data: _*)
    val bar1 = barplotVertical(
      series,
      //Series("a" -> (-2d), "b" -> (-1d), "c" -> 0d, "d" -> 1d, "e" -> 2d),
      color = RedBlue(-2, 2)
    )(par)
    pngToFile(new File("C:/tercer_semestre//practicum1.1_2023//graficosProyecto//imagen7.png"), bar1.build, width=1000)
//-----------------------------------**************************--------------------------
  def graficaMinutos(data: List[Double]) =
    val density1 = xyplot(
      density(data.toIndexedSeq) -> line(
        stroke = StrokeConf(0.5 fts)
      )
    )(par.withXLab("Minutos Reglamentarios").withYLab("dens.").withMain("Loading distribution"))
    pngToFile(new File("C:/tercer_semestre//practicum1.1_2023//graficosProyecto//imagen8.png"), density1.build, width = 1000)

//-----------------------------*********************************-----------------------------
  def graficaAnio(data: List[Int]) =
    val data2 = data.map(_.toDouble)
    val histogramAnio = xyplot(HistogramData(data2, 2) -> bar())(
      par //implicito de la libreria
        .xlab("sexo")
        .ylab("freq.")
        .main("sexo de jugadores")
        //.xlim(Some(1880d -> 2010d))
    )
    pngToFile(new File("C:/tercer_semestre//practicum1.1_2023//graficosProyecto//imagen13.png"), histogramAnio.build, width = 1000)


//----------------------------***************************---------------------------------------
  def golesEquipos(data: List[(Double, Double)])=
    val plotEquipos = xyplot(data)(
      par.withMain("Goles Hechos vs. Goles Recibidos")
        .withXLab("Goles Hechos")
        .withYLab("Goles Recibidos")
    )
    pngToFile(new File("C:/tercer_semestre//practicum1.1_2023//graficosProyecto//imagen11.png"), plotEquipos.build, width = 1000)
//-----------------------*****************************------------------------------
  def bloxplot(data: List[(Double)])=
    val plot5 = xyplot(data -> line())(
      par
        .xlab("Torneos")
        .ylab("Numero de equipos"))
    pngToFile(new File("C:/tercer_semestre//practicum1.1_2023//graficosProyecto//imagen12.png"), plot5.build, width = 1000)


  //---------------*************************-----------------------------------------
  def limpiarnull(chain: String) =
    if (chain == "not available" || chain == "" || chain == "NA") None
    else Some(chain)
}
