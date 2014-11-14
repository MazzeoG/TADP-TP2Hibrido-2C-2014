package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

class Envio (val caracteristicas : Set[ServicioExtra],
			val sucursalOrigen :Sucursal,
			val sucursalDestino :Sucursal,
			val volumen :Int,
			val fecha :Date)
	extends CalculadorDistancia{
  
	val valorRefrigeracion : Int = 0;
  
	def precio():Int = {0}
	
	def costo():Int = {0}
	
	def costoBase():Int = {0}
	
	def calcularCostoEnvio () : Double = {
	  this.costoBase()
/*	  
	  // Los aviones que vayan a Casa Central pasado el dia 20, aprovechan para volver a la sucursal de origen
	  //enviandoles insumos necesarios para el funcionamiento de la empresa, de esta forma el costo de ese envio se
	  //vera reducido en un 20 %.
	  
	  if(this.sucursalDestino.esCasaCentral() && this.pasadoElDia20()){
	    costoParcial -= costoDeTransporte * 2/10
	  }
		
	  costoParcial
*/	  
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
}