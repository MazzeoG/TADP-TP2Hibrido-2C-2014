package tadp.tp.argentinaexpresshibrido

class Avion (override val serviciosExtra : Set[ServicioExtra], sucursalOrigen: Sucursal)
extends Aereo (serviciosExtra, sucursalOrigen){
  
  override val volumenDeCarga : Int = 200
  override val costoPorKm : Int = 500
  override val velocidad : Int = 500
  override val valorPeaje: Int = 0
  
  def entraEnAvion(envio : Envio) : Boolean ={
    val calc = new CalculadorDistancia
    calc.distanciaAereaEntre(envio.sucursalOrigen , envio.sucursalDestino ) > 1000
  }
  
  override def impuestoAvion() : Double = {
    0.1
  }
  
  override def multiplicador():Double ={
    var volumenMultiplicador=this.volumenDeCarga*volOcupadoMulti;
    if (volumenMultiplicador >= this.volumenEnvios)
	  3
	else
	  1 	
  }

  override def reduccionInsumos(costoDeTransporte: Double): Double = {
    costoDeTransporte * 0.2
  }

  override def puedeCargar(envio:Envio) : Boolean ={
    super.puedeCargar(envio) && entraEnAvion(envio)
  }
}