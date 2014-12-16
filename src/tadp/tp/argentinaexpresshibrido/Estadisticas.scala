package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

class Estadisticas extends prettyPrinter{
  def filtrarPorSucursalDestino(viajes: Set[Viaje], sucursalBuscada : Sucursal): Set[Viaje] ={
    viajes.filter(_.sucursalDestino == sucursalBuscada)
  }
  
  def filtrarPorSucursalOrigen(viajes: Set[Viaje], sucursalBuscada : Sucursal): Set[Viaje] ={
    viajes.filter(_.sucursalOrigen == sucursalBuscada)
  }
  
  def filtrarPorFecha(viajes: Set[Viaje], fechaBuscada : Date): Set[Viaje] ={
    viajes.filter(_.fecha == fechaBuscada)
  }

  def filtrarPorRangoFecha(viajes: Set[Viaje], fechaIni : Date, fechaFin : Date): Set[Viaje] ={
    viajes.filter(v => v.fecha.after(fechaIni) && v.fecha.before(fechaFin) )
  }
    
  def filtrarPorTipoDeEnvio(viajes: Set[Viaje], tipo : String): Set[Viaje] ={
    viajes.filter(_.tipoEnvio == tipo)
  }  

  def filtrarPorTipoDeTransporte(viajes: Set[Viaje], tipo : String): Set[Viaje] ={
    viajes.filter(v => nombreClase(v.transporte) == tipo)
  } 
  
  def tiempoPromedioDeViajes(viajes: Set[Viaje]) : Double = {
    if(!viajes.isEmpty)
    	viajes.toList.map(_.tiempoHr).sum / viajes.size
    else
    	0
  }  
  
  def costoPromedioDeViajes(viajes: Set[Viaje]) : Double = {
    if(!viajes.isEmpty)
    	viajes.toList.map(_.costo).sum / viajes.size
    else
    	0
  }

  def gananciaPromedioDeViajes(viajes: Set[Viaje]) : Double = {
    if(!viajes.isEmpty)
    	viajes.toList.map(_.ganancia).sum / viajes.size
    else
    	0
  }
  
  def cantidadDeEnvios(viajes: Set[Viaje]) : Int ={
    viajes.toList.map(_.pedidos.size).sum
  }
  
  def cantidadDeViajes(viajes: Set[Viaje]) : Int ={
    viajes.size
  }
  
  def calcularFacturacionTotal(viajes: Set[Viaje]) : Double ={
    viajes.toList.map(_.ganancia).sum
  }
  
  def viajesPorTipoTransporte(viajes: Set[Viaje]) ={
    val viajesCamion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Camion")
    val viajesFurgoneta : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Furgoneta")
    val viajesAvion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Avion")
    
    println("VIAJES POR TRANSPORTE")
    println("--------------------------")
    println("Transporte   -    Cantidad")
    println("Camion       :    " + viajesCamion.size)
    println("Furgoneta    :    " + viajesFurgoneta.size)
    println("Avion        :    " + viajesAvion.size)
    println("--------------------------\n")
  }
  
  def enviosPorTipoTransporte(viajes: Set[Viaje]) ={
    val viajesCamion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Camion")
    val viajesFurgoneta : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Furgoneta")
    val viajesAvion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Avion")
    
    println("ENVIOS POR TRANSPORTE")
    println("--------------------------")
    println("Transporte   -    Cantidad")
    println("Camion       :    " + cantidadDeEnvios(viajesCamion))
    println("Furgoneta    :    " + cantidadDeEnvios(viajesFurgoneta))
    println("Avion        :    " + cantidadDeEnvios(viajesAvion))
    println("--------------------------\n")
  }
  
  def tiempoPromedioPorTransporte(viajes: Set[Viaje]) ={
    val viajesCamion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Camion")
    val viajesFurgoneta : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Furgoneta")
    val viajesAvion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Avion")
    
    println("TIEMPO PROMEDIO DE VIAJE")
    println("---------------------------")
    println("Transporte   -   Tiempo(Hr)")
    println("Camion       :   " + tiempoPromedioDeViajes(viajesCamion))
    println("Furgoneta    :   " + tiempoPromedioDeViajes(viajesFurgoneta))
    println("Avion        :   " + tiempoPromedioDeViajes(viajesAvion))
    println("---------------------------\n")
  }  
  
  def costoPromedioPorTransporte(viajes: Set[Viaje]) ={
    val viajesCamion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Camion")
    val viajesFurgoneta : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Furgoneta")
    val viajesAvion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Avion")
    
    println("COSTO PROMEDIO DE VIAJE")
    println("---------------------------")
    println("Transporte   -   Costo($)")
    println("Camion       :   " + costoPromedioDeViajes(viajesCamion))
    println("Furgoneta    :   " + costoPromedioDeViajes(viajesFurgoneta))
    println("Avion        :   " + costoPromedioDeViajes(viajesAvion))
    println("---------------------------\n")
  }    
  
  def gananciaPromedioPorTransporte(viajes: Set[Viaje]) ={
    val viajesCamion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Camion")
    val viajesFurgoneta : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Furgoneta")
    val viajesAvion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Avion")
    
    println("GANANCIA PROMEDIO DE VIAJE")
    println("---------------------------")
    println("Transporte   -  Ganancia($)")
    println("Camion       :   " + gananciaPromedioDeViajes(viajesCamion))
    println("Furgoneta    :   " + gananciaPromedioDeViajes(viajesFurgoneta))
    println("Avion        :   " + gananciaPromedioDeViajes(viajesAvion))
    println("---------------------------\n")
  }   
  
  def facturacionTotalPorRangoFecha(viajes: Set[Viaje], fechaIni : Date, fechaFin : Date) ={
    val viajesFecha : Set[Viaje] = filtrarPorRangoFecha(viajes, fechaIni, fechaFin)
    
    println("FACTURACION POR RANGO DE FECHA")
    println("-----------------------")
    println("Fecha inicio: " + printDate(fechaIni))
    println("Fecha Fin   : " + printDate(fechaFin))
    println("Facturacion : " + calcularFacturacionTotal(viajesFecha))
    println("-----------------------\n")
  }
  
  def facturacionTotalPorFecha(viajes: Set[Viaje], fecha : Date) ={
    val viajesFecha : Set[Viaje] = filtrarPorFecha(viajes, fecha)
    
    println("FACTURACION POR FECHA")
    println("-----------------------")
    println("Fecha       : " + printDate(fecha))
    println("Facturacion : " + calcularFacturacionTotal(viajesFecha))
    println("-----------------------\n")
  }
  
  def facturacionCompaniaPorSucursal(sucursales : Set[Sucursal]) = {
	
    println("FACTURACION TOTAL POR SUCURSAL")
	println("-----------------------------")
	println("Sucursal    -     Facturacion")
    sucursales.foreach({s =>
		println(s.toString() + " -> " + calcularFacturacionTotal(s.viajesRealizados))
	})
	println("-----------------------------\n")
  }
  
}


trait prettyPrinter {
  def nombreClase(obj : Object): String = {
    var nombre : String = obj.getClass().toString()
    nombre = nombre.substring(nombre.lastIndexOf('.')+1)  
    nombre
  } 
  
  def printDate(fecha: Date): String = {
    fecha.getDate().toString + "/" + fecha.getMonth().toString + "/" + fecha.getYear().toString
  }
}