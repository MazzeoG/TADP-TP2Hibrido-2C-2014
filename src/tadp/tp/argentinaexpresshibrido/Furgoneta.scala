package tadp.tp.argentinaexpresshibrido

class Furgoneta (override val serviciosExtra : Set[ServicioExtra], sucursalOrigen: Sucursal)
extends Terrestre (serviciosExtra, sucursalOrigen) {
  override val volumenDeCarga : Int = 9
  override val costoPorKm : Int = 40
  override val velocidad : Int = 80
  override val valorPeaje : Int = 6
  


  def costo(envio:Envio){
    var costoFinal = 
    envio match {
      case envio: Fragil => (precioPeajes()+ 5)*multiplicador
      case _ => (precioPeajes())*multiplicador
    }
   }

  override def multiplicador():Double ={
	if( (this.volumenDeCarga*volOcupadoMulti >= this.volumenEnvios)
	    && (cantUrgentes >= 3)) 
	 2
   else
     1  
  }
  
  def cantUrgentes(): Int ={
    enviosAsignados.filter(_.isInstanceOf[Urgente]).size 
  }
}