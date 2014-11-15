package tadp.tp.argentinaexpresshibrido

class Camion(override val serviciosExtra: Set[ServicioExtra], sucursalOrigen: Sucursal)
  extends Terrestre(serviciosExtra, sucursalOrigen) {

  override val volumenDeCarga: Int = 45
  override val costoPorKm: Int = 100
  override val velocidad: Int = 60
  override val valorPeaje: Int = 12

  override def puedeCargarRefrigerados() = {
    true
  }

   val vDeEvioSobreCarga = (this.volumenEnvios.toDouble / this.volumenDeCarga.toDouble)
    
  override def multiplicador(): Double = {
    ((this.volumenDeCarga * volOcupadoMulti >= this.volumenEnvios)
      && !(sucursalOrigen.esCasaCentral || sucursalDestino.esCasaCentral)) match {
        case true => 1 + vDeEvioSobreCarga
        case _ => 1
      }
  }



  override def costoRevisionTecnica(costoDeTransporte: Double): Double = {
    costoDeTransporte * 0.02
  }

  val sumaVsobreCarga = 3 * enviosAsignados.filter(_.isInstanceOf[Urgente]).map(_.volumen).sum / volumenDeCarga

  override def costoSustanciasPeligrosas(): Double = {
    enviosAsignados.count(_.caracteristicas.exists(_.soyInfraestructuraSustancias)) match {
      case 0 => 0
      case _ => 600 + sumaVsobreCarga
    }
  }
}