package tadp.tp.argentinaexpresshibrido

class Camion (override val serviciosExtra : Set[ServicioExtra], sucursalOrigen: Sucursal)
extends Terrestre (serviciosExtra, sucursalOrigen){
  
  override val volumenDeCarga : Int = 45
  override val costoPorKm : Int = 100
  override val velocidad : Int = 60
  override val valorPeaje : Int = 12

  override def puedeCargarRefrigerados() ={
    true
  } 
  
  override def multiplicador():Double ={
	if( (this.volumenDeCarga*volOcupadoMulti >= this.volumenEnvios)
	    && !(sucursalOrigen.esCasaCentral || sucursalDestino.esCasaCentral)) 
     1+(this.volumenEnvios.toDouble/this.volumenDeCarga.toDouble)
   else
     1  
  } 
  
  override def costoRevisionTecnica(costoDeTransporte: Double): Double ={
     costoDeTransporte * 0.02
   }
  
  override def costoSustanciasPeligrosas(): Double ={
    if(enviosAsignados.exists(_.caracteristicas.exists(_.soyInfraestructuraSustancias)))
      600 + (3 * enviosAsignados.filter(_.isInstanceOf[Urgente]).map(_.volumen).sum / volumenDeCarga )
    else
      0    
  }
 }