package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

abstract class Transporte(val serviciosExtra: Set[ServicioExtra], var sucursalOrigen: Sucursal)
      extends Estadisticas{
  val volumenDeCarga: Int
  val costoPorKm: Int
  val velocidad: Int
  var sucursalDestino: Sucursal = null
  var enviosAsignados: Set[Envio] = Set()
  var fechaEnvio: Date = null
  val valorPeaje: Int
  val volOcupadoMulti: Double = 0.2
  var viajesRealizados : Set[Viaje] = Set()
  // Inicializacion de los transportes
  if (sucursalOrigen != null)
    sucursalOrigen.agregarTransporte(this)


  def puedeCargarUrgentes() = {
    true
  }
  def puedeCargarFragiles() = {
    false
  }
  def puedeCargarRefrigerados() = {
    false
  }

  def sinEnviosAsignados: Boolean = {
    this.enviosAsignados.isEmpty
  }

  def volumenEnvios() :Int = {
     this.enviosAsignados.toList.map((e:Envio) => e.volumen).sum
  }

  def volumenDisponible(): Int = {
    this.volumenDeCarga - this.volumenEnvios
  }

  //Funcion utilizada para validar que un transporte pueda cargar un envio
  def puedeCargar(envio: Envio): Boolean = { 
    coincideDestino(envio) && entraEnTransporte(envio) && infraestructuraNecesaria(envio) && coincideTipoDeEnvio(envio) && 
    (envio match {
      case envio: Fragil => puedeCargarFragiles
      case envio: Urgente => puedeCargarUrgentes
      case envio: Refrigeracion => puedeCargarRefrigerados
      case _ => true
    }) 
  }

  def coincideDestino(envio: Envio): Boolean = {
    if(this.sinEnviosAsignados)
      true
    else {
      this.enviosAsignados.forall((e:Envio) => e.sucursalDestino==envio.sucursalDestino);
    }
  }

  def coincideTipoDeEnvio(envio: Envio) : Boolean ={
    enviosAsignados.forall(_.puedeEnviarseCon(envio))
  }

  def entraEnTransporte(envio: Envio): Boolean = {
    this.volumenDisponible >= envio.volumen
  }

  def infraestructuraNecesaria(envio: Envio): Boolean = {
    envio.caracteristicas.forall(carac => this.serviciosExtra.contains(carac))
  }


 lazy  val costoDeEnvios = this.enviosAsignados.toList.map((e: Envio) => e.costoBase()).sum

 lazy  val sumaPrecioPeajes = { (unCosto: Double) => unCosto + precioPeajes }
 
 lazy  val sumaCostoRefrigeracion = { (unCosto: Double) => unCosto + costoDeRefrigeracion }

 lazy  val avionConPeaje = { (unCosto: Double) =>
    unCosto + (sucursalOrigen.pais == this.sucursalDestino.pais match {
      case true => 0
      case _ => costoDeTransporte * impuestoAvion
    })
  }

 lazy  val costoDeTransporte = this.costoTransporte(sucursalOrigen, sucursalDestino)

 lazy  val costoBaseDeTransporte = (costoDeTransporte + costoDeEnvios)
 
  lazy val sumaCostoRevisionTecnica = { (unCosto: Double) =>
    unCosto + ((this.sucursalDestino.esCasaCentral() && this.ultimaSemanaDelMes()) match {
      case true => costoRevisionTecnica(costoDeTransporte)
      case _ => 0
    })
  }

 lazy  val sumaReduccionDeInsumos = { (unCosto: Double) =>
    unCosto - ((pasadoElDia20 && sucursalDestino.esCasaCentral) match {
      case true => reduccionInsumos(costoDeTransporte)
      case _ => 0
    })
  }

 lazy  val multiplicaCostoTransporte = { (unCosto: Double) => unCosto + costoDeTransporte * multiplicador }

 lazy  val sumaCostoGPS = { (unCosto: Double) => unCosto + costoGPS }
 lazy  val sumaCostoVideo = { (unCosto: Double) => unCosto + costoVideo }
 lazy  val sumaCostoSustanciasPeligrosas = { (unCosto: Double) => unCosto + costoSustanciasPeligrosas }
 lazy  val sumaCostoAnimales = { (unCosto: Double) => unCosto + costoAnimales }

  implicit class FExt[A, B](f: A => B) {
    def <*[C](g: C => A) = {
      f compose g
    }
  }

lazy val calcularCostoViaje = (sumaCostoAnimales <* sumaCostoSustanciasPeligrosas <* sumaCostoVideo <* sumaCostoGPS <* sumaReduccionDeInsumos <* sumaCostoRevisionTecnica <* avionConPeaje <* sumaCostoRefrigeracion <* sumaPrecioPeajes <* multiplicaCostoTransporte)(costoDeEnvios)
  
  def costoTransporte(sucursalOrigen: Sucursal, sucursalDestino: Sucursal): Double = {
    val calc = new CalculadorDistancia
	this match {
      case transporte: Avion => this.costoPorKm * calc.distanciaAereaEntre(sucursalOrigen, sucursalDestino)
      case transporte: Camion => this.costoPorKm * calc.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
      case transporte: Furgoneta => this.costoPorKm * calc.distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
    }
  }

  def precioPeajes(): Int = {
    val calc = new CalculadorDistancia
    (calc.cantidadPeajesEntre(sucursalOrigen,sucursalDestino) * this.valorPeaje)
  }

  def multiplicador(): Double = {
    1
  }

  def agregarEnvio(envio: Envio): Transporte = {
    this.enviosAsignados = this.enviosAsignados ++ Set(envio)
    if (enviosAsignados.size == 1) {
      sucursalDestino = envio.sucursalDestino
      fechaEnvio = envio.fecha
    }

    this
  }

  def tieneSeguimientoGPS(): Boolean = {
    !this.serviciosExtra.find((s: ServicioExtra) => s.soyGPS).isEmpty
  }

  def tieneSeguimientoVideo(): Boolean = {
    !this.serviciosExtra.find((s: ServicioExtra) => s.soyVideo).isEmpty
  }

  // Falta definir la mutua exclusion
  def puedeLlevarAnimales(): Boolean = {
    !this.serviciosExtra.find((s: ServicioExtra) => s.soyInfraestructuraAnimales).isEmpty
  }

  def puedeLlevarSustancias(): Boolean = {
    !this.serviciosExtra.find((s: ServicioExtra) => s.soyInfraestructuraSustancias).isEmpty
  }

  def impuestoAvion(): Double = {
    0
  }

  def costoDeRefrigeracion(): Double = {
    0
  }

  def ultimaSemanaDelMes(): Boolean = {
    var cal: Calendar = Calendar.getInstance()
    cal.setTime(fechaEnvio)

    (cal.get(Calendar.MONTH)) match {
      case 1 => cal.get(Calendar.WEEK_OF_MONTH) == 4
      case _ => cal.get(Calendar.WEEK_OF_MONTH) == 5
    }
  }

  def pasadoElDia20(): Boolean = {
    var cal: Calendar = Calendar.getInstance()
    cal.setTime(fechaEnvio)
    cal.get(Calendar.DAY_OF_MONTH) > 20
  }

  def costoRevisionTecnica(costoDeTransporte: Double): Double = {
    0
  }

  def costoGPS(): Double = {
    serviciosExtra.count(_.soyGPS) match {
      case 0 => 0
      case _ => distanciaEntreSucursales() * 2 * 0.5
    }
  }

  def costoVideo(): Double = {
    serviciosExtra.count(_.soyVideo) match {
      case 0 => 0
      case _ => distanciaEntreSucursales() * 2 * 3.74
    }
  }

  def distanciaEntreSucursales(): Double = {
    0
  }

  def costoSustanciasPeligrosas(): Double = {
    enviosAsignados.count(_.caracteristicas.exists(_.soyInfraestructuraSustancias)) match {
      case 0 => 0.0
      case _ => 600.0
    }
  }

  def costoAnimales(): Double = {
    val dist = distanciaEntreSucursales
    if (enviosAsignados.exists(_.caracteristicas.exists(_.soyInfraestructuraAnimales)))
      if (dist < 100)
        50
      else if (dist < 200)
        86
      else
        137
    else
      0
  }

  def reduccionInsumos(costoDeTransporte: Double): Double = {
    0
  }

  def regresarASucursal() ={
   sucursalOrigen.agregarTransporte(this) 
   sucursalDestino = null
  }
  
  def calcularGananciaBruta() : Double = {
    enviosAsignados.toList.map(_.precio).sum
  }
  
  def calcularGananciaNeta() : Double = {
    calcularGananciaBruta - calcularCostoViaje
  }  
  
  def calcularTiempoViaje() : Double = {
    distanciaEntreSucursales / velocidad
  }
}


