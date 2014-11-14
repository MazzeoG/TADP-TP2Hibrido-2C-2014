package tadp.tp.argentinaexpresshibrido

abstract class Terrestre (override val serviciosExtra : Set[ServicioExtra], sucursalOrigen: Sucursal)
extends Transporte (serviciosExtra, sucursalOrigen){

  override def costoDeRefrigeracion(): Double ={
    this.enviosAsignados.map(e => e.valorRefrigeracion).sum
  }
  
  override def distanciaEntreSucursales(): Double ={
    distanciaTerrestreEntre(sucursalOrigen, sucursalDestino)
  }
}