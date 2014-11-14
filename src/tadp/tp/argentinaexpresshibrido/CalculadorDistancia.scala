package tadp.tp.argentinaexpresshibrido

trait CalculadorDistancia {
	def distanciaTerrestreEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double = {
	  if (sucursal1.pais == sucursal2.pais) 
		500
	  else
	    1500
	}
	def distanciaAereaEntre(sucursal1: Sucursal, sucursal2: Sucursal): Double = {
	  if (sucursal1.pais == sucursal2.pais) 
		500
	  else
	    1500		
	}
	
	def cantidadPeajesEntre(sucursal1: Sucursal, sucursal2: Sucursal): Int = {
	  if (sucursal1.pais == sucursal2.pais) 
		0
	  else
	    5
	}
}