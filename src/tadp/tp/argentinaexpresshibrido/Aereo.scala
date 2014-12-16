package tadp.tp.argentinaexpresshibrido

abstract class Aereo (override val serviciosExtra : Set[ServicioExtra], sucursalOrigen: Sucursal)
extends Transporte (serviciosExtra, sucursalOrigen){

  override def distanciaEntreSucursales(): Double ={
    val calc = new CalculadorDistancia
    calc.distanciaAereaEntre(sucursalOrigen, sucursalDestino)
  }
}