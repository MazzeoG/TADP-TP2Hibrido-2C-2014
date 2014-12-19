package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

class Envio (val caracteristicas : Set[ServicioExtra],
			val sucursalOrigen :Sucursal,
			val sucursalDestino :Sucursal,
			val volumen :Int,
			val fecha :Date)
	{
  
	def valorRefrigeracion : Int = 0;
  
	def precio():Int = {0}
	
	def costo():Int = {0}
	
	def costoBase():Int = {0}
	
	def calcularCostoEnvio () : Double = {
	  this.costoBase() 
    }
 
	def ultimaSemanaDelMes(): Boolean = {
	  var cal: Calendar = Calendar.getInstance()
	  cal.setTime(this.fecha)

	  (cal.get(Calendar.MONTH)) match
	  {
	  	case 1=> cal.get(Calendar.WEEK_OF_MONTH) == 4
	  	case _=> cal.get(Calendar.WEEK_OF_MONTH) == 5
	  }
	 }
	
	def pasadoElDia20(): Boolean = {
	  var cal: Calendar = Calendar.getInstance()
	  cal.setTime(this.fecha)
	  
	  cal.get(Calendar.DAY_OF_MONTH) > 20
	}
	
	def esCargablePor(transporte: Transporte) : Boolean = {
	  true
	}
	
	def puedeEnviarseCon (envio :Envio): Boolean = true
	
	def puedeEnviarseConFragiles : Boolean = true
	def puedeEnviarseConNormal : Boolean = true
	def puedeEnviarseConRefrigeracion : Boolean = true
	def puedeEnviarseConUrgentes : Boolean = true
}