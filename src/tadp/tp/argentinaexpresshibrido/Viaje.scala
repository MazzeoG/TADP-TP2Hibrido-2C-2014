package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

class Viaje (val sucursalOrigen : Sucursal, val sucursalDestino : Sucursal,	
			 val transporte : Transporte  , val pedidos : Set[Envio]      ,
			 val costo : Double           , val ganancia : Double         ,
			 val fecha : Date             , val tipoEnvio : String        ,
			 val tiempoHr : Double
	) extends Estadisticas {
  
}
