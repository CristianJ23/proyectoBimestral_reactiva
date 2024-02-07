import com.github.tototoshi.csv.*
import java.io.File

//import para librerias:
/*"io.github.pityka" %% "nspl-awt" % "0.10.0"
"io.github.pityka" %% "nspl-core" % "0.10.0"*/
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData

package object proyecto extends App {

  implicit object MyFormat extends DefaultCSVFormat {
    override val delimiter: Char = ';'
  }

  object App {
    @main//(anotacion)convierte a pintegra en el objeto principalpara ejecutar el programa
    def pintegra() =
      val pathDataFile: String = "C://practicum1.1_2023//dsAlineacionesXTorneo.csv"
      //para windows-> c://archivos//data//archivo
      val reader = CSVReader.open(new File(pathDataFile))
      //val contentFile: List[List[String]] = reader.all()
      val contentFile: List[Map[String, String]] = reader.allWithHeaders()
      reader.close()
      //println(contentFile)
      println(contentFile.take(2))
      //conseguir tamaño de filas y columnas del csv
      // println(s"filas: ${contentFile.length} y columnas: ${contentFile(1).keys.size}")

      charting(contentFile)

    def charting(data: List[Map[String, String]]): Unit = {
      val listNroShirt: List[Double] = data
        .filter(row => row("squads_position_name") == "forward" && row("squads_shirt_number") != "0")
        .map(row => row("squads_shirt_number").toDouble)

      val histoForwardShirtNumber = xyplot(HistogramData(listNroShirt, 10) -> bar())(
        par //implicito de la libreria
          .xlab("shirt number")
          .ylab("freq.")
          .main("forward shirt shirt number")
      )
      pngToFile(new File("C://practicum1.1_2023//graficosProyecto//imagen.png"), histoForwardShirtNumber, build, 1000)
      //renderToByteArray(histoForwardShirtNumber.build, width = 2000)
    }

  }










  //buscar cual es el valor minimo y cual es el maximo de capacidad de los estadios,
  //cual es la capacidad media de todos los estadios ->nombre de estadios repetidos

}
