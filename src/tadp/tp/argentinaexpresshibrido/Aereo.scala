package tadp.tp.argentinaexpresshibrido

case class Aereo (override val serviciosExtra : Set[ServicioExtra], sucursalOrigen: Sucursal)
extends Transporte (serviciosExtra, sucursalOrigen){

  override def distanciaEntreSucursales(): Double ={
    distanciaAereaEntre(sucursalOrigen, sucursalDestino)
  }
}