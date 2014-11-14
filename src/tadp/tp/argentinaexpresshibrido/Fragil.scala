package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat

import java.util.{Calendar, Date}

class Fragil (override val caracteristicas : Set[ServicioExtra],
    override val sucursalOrigen :Sucursal,
    override val sucursalDestino :Sucursal,
    override val volumen :Int,
    override val fecha :Date)
extends Envio (caracteristicas, sucursalOrigen, sucursalDestino, volumen, fecha){
  override def precio()={120}
  override def costoBase()={18}
}