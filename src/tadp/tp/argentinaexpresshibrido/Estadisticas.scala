package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

class Tupla (nombre:String, cantidad:Unit){
}

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
  
  // Comparacion entre distintos tipos de Transporte
  def EstadisticasPorTransporte(viajes: Set[Viaje], f2: Set[Viaje] => _, titulo: String, subt: String) ={
    
    val viajesCamion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Camion")
    val viajesFurgoneta : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Furgoneta")
    val viajesAvion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Avion")
    
    println(titulo)
    println("--------------------------")
    println("Transporte   -    " + subt)
    println("Camion       :    " + f2(viajesCamion))
    println("Furgoneta    :    " + f2(viajesFurgoneta))
    println("Avion        :    " + f2(viajesAvion))
    println("--------------------------\n")
    val tuplaCamion: Tupla=new Tupla("camion", f2(viajesCamion))
    val tuplaFurgoneta: Tupla=new Tupla("furgoneta", f2(viajesFurgoneta))
    val tuplaAvion: Tupla=new Tupla("avion", f2(viajesAvion))
    var datos:Set[Tupla]=Set()
    //datos += tuplaCamion + tuplaFurgoneta + tuplaAvion
   }
  
  //Ejemplos de distintos filtros posibles
  def enviosPorTipoTransporte(viajes: Set[Viaje]) = {
    EstadisticasPorTransporte(viajes,cantidadDeEnvios,"ENVIOS POR TRANSPORTE","Cantidad")
  }
  
  def tiempoPromedioPorTransporte(viajes: Set[Viaje]) = {
    EstadisticasPorTransporte(viajes,tiempoPromedioDeViajes,"TIEMPO PROMEDIO DE VIAJE","Tiempo(Hr)")
  }
  
   def costoPromedioPorTransporte(viajes: Set[Viaje]) = {
    EstadisticasPorTransporte(viajes,costoPromedioDeViajes,"COSTO PROMEDIO DE VIAJE","Costo($)")
  }
   
  def gananciaPromedioPorTransporte(viajes: Set[Viaje]) = {
    EstadisticasPorTransporte(viajes,gananciaPromedioDeViajes,"GANANCIA PROMEDIO DE VIAJE","Ganancia($)")
  }
  
  def viajesPorTipoTransporte(viajes: Set[Viaje]) = {
    EstadisticasPorTransporte(viajes,cantidadDeViajes,"VIAJES POR TRANSPORTE","Cantidad")
  }
  
  def EstadisticasPorEnvio(viajes: Set[Viaje], f2: Set[Viaje] => _, titulo: String, subt: String) ={
    
    val viajesNormal : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Normal")
    val viajesUrgente : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Urgente")
    val viajesRefrigeracion : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Refrigeracion")
    val viajesFragil : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Fragil")
    
    println(titulo)
    println("--------------------------")
    println("Envio   -    " + subt)
    println("Normal       :    " + f2(viajesNormal))
    println("Urgente    :    " + f2(viajesUrgente))
    println("Refrigeracion        :    " + f2(viajesRefrigeracion))
    println("Fragil        :    " + f2(viajesFragil))
    println("--------------------------\n")
    val tuplaNormal: Tupla=new Tupla("camion", f2(viajesNormal))
    val tuplaUrgente: Tupla=new Tupla("furgoneta", f2(viajesUrgente))
    val tuplaRefrigeracion: Tupla=new Tupla("avion", f2(viajesRefrigeracion))
    val tuplaFragil: Tupla=new Tupla("avion", f2(viajesFragil))
    var datos:Set[Tupla]=Set()
    //datos += tuplaNormal + tuplaUrgente + tuplaRefrigeracion + tuplaFragil
    
  }
  
  // Comparacion entre distintas Sucursales
  def EstadisticasPorSucursal(sucursales : Set[Sucursal],viajes: Set[Viaje], f2: Set[Viaje] => _) ={
    sucursales.foreach({s =>
		f2(s.viajesRealizados)
    })
  }
  /*def viajesPorTipoTransporte(viajes: Set[Viaje]) ={
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
  }*/ //Reemplazado con metodos que implementan orden superior
  
  /*def enviosPorTipoTransporte(viajes: Set[Viaje]) ={
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
  }*/ //Reemplazado con metodos que implementan orden superior
  
  /*def tiempoPromedioPorTransporte(viajes: Set[Viaje]) ={
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
  } */ //Reemplazado con metodos que implementan orden superior
  
  
   
  /* def costoPromedioPorTransporte(viajes: Set[Viaje]) ={
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
  } */ //Reemplazado con metodos que implementan orden superior
     
   
  /* def gananciaPromedioPorTransporte(viajes: Set[Viaje]) ={
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
  }  */ //Reemplazado con metodos que implementan orden superior 
  
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