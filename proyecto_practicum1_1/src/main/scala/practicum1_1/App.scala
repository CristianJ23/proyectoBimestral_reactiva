package practicum1_1

import doobie.implicits.*
import com.github.tototoshi.csv.*

import java.io.{File, PrintWriter}

implicit object MyFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';'
}

object App {
@main
def exportFunc(): Unit =
  val pathDataFile: String = "C:\\tercer_semestre\\practicum1.1_2023\\dsAlineacionesXTorneo.csv"
  val reader = CSVReader.open(new File(pathDataFile))
  val contentFile: List[Map[String, String]] = reader.allWithHeaders()
  //achivo torneos y goles
  val pathGoles: String = "C:\\tercer_semestre\\practicum1.1_2023\\dsPartidosYGoles.csv"
  val reader2 = CSVReader.open(new File(pathGoles))
  val contentFileGoles: List[Map[String, String]] = reader2.allWithHeaders()
  reader.close()
  reader2.close()

  //println(contentFileGoles)
  //println(contentFile)
  //val claves: List[Iterable[String]] = contentFileGoles.map(_.keys).distinct
  //claves.foreach(println(_))

  //println(players)
  println("¿cuantos jugadores han jugado en los mundiales?")
  //val players = jugadores(contentFile)
  //println(players)

  println("¿cual es el jugador de menor edad historico?")
  //val jugadorMenorHistorico = edadMenor(contentFile)
  //println(jugadorMenorHistorico)

  println("¿cual es el numero de goles mas alto en un torneo?")
  //val goles_Torneos = golesAltoTorneo(contentFileGoles)
  //println(goles_Torneos)

  println("¿cual es el numero de penaltys marcados mas alto en un torneo?")
  //println(penaltyMaximo(contentFileGoles))

  //println("numero de goles promedio por pais")
  println("¿cual es el minimo tiempo en que se ha hecho un gol?")
  //println(golRapido(contentFileGoles))

  println("¿cual es el promedio de tiempo de goles")
  //println(promedioGoles(contentFileGoles))

  println("¿cual es el maximo de equipos en un torneo?")
  //println(equiposTorneos(contentFile))

  println("¿cual es el promedio de equipos en un torneo?")
  //println(promediEquipos(contentFile))

  println("¿cual es la moda de los minutos de gol?")
  //println(modaTiempoGol(contentFileGoles))

  println("¿cual es la mediana de la capacidad de los estadios?")
  println(medianaEstadios(contentFileGoles))
  /*------------------------------*******************************------------------------------*/
  //¿cuantos jugadores han jugado en los mundiales?
def jugadores(data: List[Map[String, String]]) =
  val players = data
    .map(_("squads_player_id").trim).distinct.size
  players

  //-----------------------------***************************----------------------
 // ¿cual es el jugador de menor edad historico?
def edadMenor(data: List[Map[String, String]]) =
  val players = data
   .map(_("squads_player_id").trim).distinct

  val menor = players.map(jugador =>
   (jugador,
     data.find(_("squads_player_id") == jugador).map(_("players_birth_date")).get)
  )

  val menor_2 = menor.map(jugador => (jugador._1, limpiarnull(jugador._2).map(x => x.take(4).toInt))
  )
  val minAge = menor_2.flatMap(_._2).max

  val jugadorMasJoven = menor_2.filter(x => x._2.contains(minAge))
  jugadorMasJoven

  //----------------------------**************************----------------------------
  //numero de goles mas alto en un torneo
def golesAltoTorneo(data: List[Map[String, String]])=
  val torneosUnique = data
    .map(_("matches_tournament_id")).distinct

  val goles = torneosUnique
    .map(torneo =>
      val home_score = data.filter(_("matches_tournament_id") == torneo).map(_("matches_home_team_score").toInt)
      val away_score = data.filter(_("matches_tournament_id") == torneo).map(_("matches_away_team_score").toInt)
      val home_penalty = data.filter(_("matches_tournament_id") == torneo).map(_("matches_home_team_score_penalties").toInt)
      val away_penalty = data.filter(_("matches_tournament_id") == torneo).map(_("matches_away_team_score_penalties").toInt)
      val suma = home_score.zip(away_score).zip(home_penalty).zip(away_penalty).map{case (((a, b),c),d) => a + b + c +  d}.sum
      (torneo, suma)
    )

  val gol_2 = goles.map(x => x._2).max
  val golAlto = goles.filter(x => x._2 == gol_2)
  golAlto
//---------------------------------***********************************------------------------------
//¿cual es el maximo numero de goles de penalty hechos en un torneo
def penaltyMaximo(data: List[Map[String, String]]) =
  val torneos = data
    .map(_("matches_tournament_id")).distinct.sorted
  val penaltys = torneos
    .map(torneo =>
      val penales = data.filter(_("matches_tournament_id") == torneo)
        .map(_("goals_penalty"))
        .flatMap(limpiarnull(_)).map(_.toInt).sum
      (torneo, penales)
    )
  val result = penaltys.maxBy(_._2)
  result
  //--------------------------------*******************************----------------------------
//¿cual es el minimo tiempo en que se ha hecho un gol?
def golRapido(data: List[Map[String, String]]) =
  val goles = data
    .map(_("goals_minute_regulation")).min
  goles

  //------------------------------***************************--------------------------------------
  //¿cual es el promedio de tiempo de goles
def promedioGoles(data: List[Map[String, String]]) =
  val goles = data
    .map(_("goals_minute_regulation")).flatMap(limpiarnull(_)).map(_.toInt)
  val result = goles.sum / goles.size
  result

  //-----------------------------------*******************------------------------------------
  //¿cual es el maximo de equipos en un torneo?
def equiposTorneos(data: List[Map[String, String]]) =
  val torneos = data
    .map(_("squads_tournament_id")).distinct.sorted
  val equipos = torneos
    .map(torneo=>
    val equipos = data.filter(_("squads_tournament_id") == torneo)
      .map(_("squads_team_id")).size
    (torneo, equipos)
    )
  val result = equipos.maxBy(_._2)
  result

  //-----------------------------------*****************************-----------------------------
  //¿cual es el promedio de equipos por torneo?
def promediEquipos(data: List[Map[String, String]]) =
  val torneos = data
    .map(_("squads_tournament_id")).distinct.sorted
  val equipos = torneos
    .map(torneo =>
    val equipos = data.filter(_("squads_tournament_id") == torneo)
      .map(_("squads_team_id")).size
    (torneo, equipos)
    )
  val promedioEquipos = equipos.map(_._2).sum.toDouble / equipos.size.toDouble
  promedioEquipos

  //---------------------------------*************************----------------------------------
  //moda tiempo de goles
def modaTiempoGol(data: List[Map[String, String]]) =
  val tiempos = data.map(_("goals_minute_regulation")).flatMap(limpiarnull).map(_.toInt)
  val result = tiempos.groupBy(identity)
  val moda = result.maxBy(z => z._2.size)._1
  moda

  //---------------------------**********************************------------------------------
  //¿cual es la mediana de la cpacidad de los estadios?
def medianaEstadios(data: List[Map[String, String]])=
  val estadios = data
    .map(_("matches_stadium_id")).distinct.sorted
  val capacidad = estadios.map(estadio =>
    val capacity = data.find(_("matches_stadium_id") == estadio).map(_("stadiums_stadium_capacity").toInt).getOrElse(0)
    (capacity)
  ).sorted
  val longitud = capacidad.length
  //println(longitud)

  if (longitud == 0) {
    0.0
  } else if (longitud % 2 == 1) {
    capacidad(longitud / 2)
  } else {
    val mitadSuperior = capacidad(longitud / 2)
    val mitadInferior = capacidad(longitud / 2 - 1)
    (mitadSuperior + mitadInferior) / 2.0
  }
  
  //-----------------------------------***********************************------------------------------
def limpiarnull(chain: String) =
  if (chain == "not available" || chain == "" || chain == "NA") None
  else Some(chain)




}

/*def dataPlayers(data: List[Map[String, String]]) =
  val insertFormat = s"('%s', '%s', '%s', '%s', %s)"
  val uniquePlayers = data.map(_("squads_player_id").trim).distinct
  val playersInserts = uniquePlayers
    .map(player =>
      (player,
      data.find(_("squads_player_id") == player).map(_("players_family_name").trim).get,
      data.find(_("squads_player_id") == player).map(_("players_given_name").trim).get,
      data.find(_("squads_player_id") == player).map(_("players_birth_date").trim).get,
      data.find(_("squads_player_id") == player).map(_("players_female").trim).get)
    ).sorted.map(t5 => insertFormat.format(t5._1, t5._2, t5._3, t5._4, t5._5))
    .mkString("", ",\n", ";")
  //playersInserts.mkString(",\n")
    val result = "INSERT INTO players(idplayers, family_name, given_name, birth_date, female) VALUES\n" + playersInserts
  result

def imprimirArchivo(cadena: String, nombre: String) =
  val writer = new PrintWriter(s"C:\\tercer_semestre\\practicum1.1_2023\\querys\\${nombre}.txt")
  try{
    writer.write(cadena)
  }finally {
    writer.close()
  }*/
