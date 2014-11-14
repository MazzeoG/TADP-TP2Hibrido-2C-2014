package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat

import java.util.{Calendar, Date}

case class Refrigeracion (override val caracteristicas : Set[ServicioExtra],
    override val sucursalOrigen :Sucursal,
    override val sucursalDestino :Sucursal,
    override val volumen :Int,
    override val fecha :Date)
extends Envio (caracteristicas, sucursalOrigen, sucursalDestino, volumen, fecha){
	override def precio()={210}
	override def costoBase()={70}
	override val valorRefrigeracion : Int = 5;
}