package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat

import java.util.{Calendar, Date}

class Urgente (override val caracteristicas : Set[ServicioExtra],
    override val sucursalOrigen :Sucursal,
    override val sucursalDestino :Sucursal,
    override val volumen :Int,
    override val fecha :Date)
extends Envio (caracteristicas, sucursalOrigen, sucursalDestino, volumen, fecha){
	override def precio()={110}
	override def costoBase()={20}
	
	override def esCargablePor(transporte: Transporte) : Boolean = {
	  transporte.puedeCargarUrgentes
	}

  override def puedeEnviarseCon (envio: Envio): Boolean =  envio.puedeEnviarseConUrgentes
  
  override def puedeEnviarseConFragiles : Boolean = false
  override def puedeEnviarseConNormal : Boolean = false
  override def puedeEnviarseConRefrigeracion : Boolean = false
  override def puedeEnviarseConUrgentes : Boolean = true
}