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

  def vDeEvioSobreCarga = (this.volumenEnvios.toDouble / this.volumenDeCarga.toDouble)
    
  override def multiplicador(): Double = {
    if ((this.volumenDeCarga * volOcupadoMulti >= this.volumenEnvios) && 
        !(sucursalOrigen.esCasaCentral || sucursalDestino.esCasaCentral))
    		1 + vDeEvioSobreCarga
    	else
    		1
  }



  override def costoRevisionTecnica(costoDeTransporte: Double): Double = {
    costoDeTransporte * 0.02
  }

  def  sumaVsobreCarga = 3 * enviosAsignados.filter(_.isInstanceOf[Urgente]).toList.map(_.volumen).sum / volumenDeCarga

  override def costoSustanciasPeligrosas(): Double = {
    if(enviosAsignados.exists(_.caracteristicas.exists(_.soyInfraestructuraSustancias)))
      600 + sumaVsobreCarga
      else
        0
  }
}