package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

case class Tupla (nombre:String, cantidad:String){
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

  //funcion identidad para el rango de fechas
  
  def sinFiltroFechas(viajes: Set[Viaje], fechaIni : Date, fechaFin : Date): Set[Viaje] ={
    viajes
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
  
 
  //funcion identidad para las estadisticas
  def estadisticaIdentidad(viajes: Set[Viaje], comparacion: (Set[Viaje]) => _,filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date) 
  ={
    var viajesFiltrados: Set[Viaje] = filtroFecha(viajes,fechaIni,fechaFin)
    comparacion(viajesFiltrados)
  }
  
  // Comparacion entre distintos tipos de Transporte
  def estadisticasPorTransporteSinFiltros(
      viajes: Set[Viaje], comparacion: (Set[Viaje]) => _,filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date)
  :Set[Tupla] 
  ={
    estadisticasPorTransporte(viajes,comparacion,estadisticaIdentidad,filtroFecha,fechaIni,fechaFin)
    
   }
  
  def estadisticasPorTransporte(
      viajes: Set[Viaje], comparacion: (Set[Viaje]) => _,filtro: (Set[Viaje],Set[Viaje]=> _,(Set[Viaje],Date,Date) => Set[Viaje],Date,Date) => _,
      filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje], fechaIni : Date, fechaFin : Date)
  :Set[Tupla] 
  ={
    
    val viajesCamion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Camion")
    val viajesFurgoneta : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Furgoneta")
    val viajesAvion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Avion")
    
    val tuplaCamion: Tupla =new Tupla("camion", filtro(viajesCamion,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaFurgoneta: Tupla=new Tupla("furgoneta", filtro(viajesFurgoneta,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaAvion: Tupla=new Tupla("avion", filtro(viajesAvion,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    var datos:Set[Tupla]=Set()
    datos += tuplaCamion 
    datos += tuplaFurgoneta 
    datos += tuplaAvion
    datos
   }
  
  //Ejemplos de distintos filtros posibles
  def enviosPorTipoTransporte(viajes: Set[Viaje]) :Set[Tupla]= {
    var datos:Set[Tupla]=Set()
    datos = estadisticasPorTransporte(viajes,cantidadDeEnvios,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("ENVIOS POR TRANSPORTE","Cantidad")
    datos
  }
  
  def tiempoPromedioPorTransporte(viajes: Set[Viaje]) :Set[Tupla]= {
   var datos:Set[Tupla]=Set()
    datos= estadisticasPorTransporte(viajes,tiempoPromedioDeViajes,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("TIEMPO PROMEDIO DE VIAJE","Tiempo(Hr)")
    datos
  }
  
   def costoPromedioPorTransporte(viajes: Set[Viaje]) :Set[Tupla]= {
    var datos:Set[Tupla]=Set()
    datos= estadisticasPorTransporte(viajes,costoPromedioDeViajes,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("COSTO PROMEDIO DE VIAJE","Costo($)")
    datos
   }
   
  def gananciaPromedioPorTransporte(viajes: Set[Viaje]):Set[Tupla] = {
    var datos:Set[Tupla]=Set()
    datos= estadisticasPorTransporte(viajes,gananciaPromedioDeViajes,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("GANANCIA PROMEDIO DE VIAJE","Ganancia($)")
    datos
  }
  
  def viajesPorTipoTransporte(viajes: Set[Viaje])  :Set[Tupla]= {
    var datos:Set[Tupla]=Set()
    datos= estadisticasPorTransporte(viajes,cantidadDeViajes,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("VIAJES POR TRANSPORTE","Cantidad")
    datos
  }
  
  def estadisticasPorEnvioSinFiltros(
      viajes: Set[Viaje], comparacion: Set[Viaje] => _,filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date) 
  ={
    
    estadisticasPorEnvio(viajes,comparacion,estadisticaIdentidad,filtroFecha,fechaIni,fechaFin)
    
  }
  
  def estadisticasPorEnvio(
      viajes: Set[Viaje], comparacion: Set[Viaje] => _,
      filtro: (Set[Viaje],Set[Viaje]=> _,(Set[Viaje],Date,Date) => Set[Viaje],Date,Date) => _,
      filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date) 
  ={
    
    val viajesNormal : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Normal")
    val viajesUrgente : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Urgente")
    val viajesRefrigeracion : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Refrigeracion")
    val viajesFragil : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Fragil")
    
    val tuplaNormal: Tupla=new Tupla("camion", filtro(viajesNormal,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaUrgente: Tupla=new Tupla("furgoneta", filtro(viajesUrgente,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaRefrigeracion: Tupla=new Tupla("avion", filtro(viajesRefrigeracion,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaFragil: Tupla=new Tupla("avion", filtro(viajesFragil,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    var datos:Set[Tupla]=Set()
    datos += tuplaNormal 
    datos += tuplaUrgente 
    datos += tuplaRefrigeracion 
    datos += tuplaFragil
    datos
    
  }
  
  // Comparacion entre distintas Sucursales
  // Comparacion entre distintas Sucursales
  def estadisticasPorSucursal(sucursales : Set[Sucursal], filtro: (Set[Viaje],Set[Viaje]=> _,(Set[Viaje],Date,Date) => Set[Viaje],Date,Date) => _,
      comparacion: Set[Viaje] => _, filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje], fechaIni : Date, fechaFin : Date) 
  ={
    sucursales.foreach({s =>
		filtro(s.viajesRealizados,comparacion,filtroFecha,fechaIni,fechaFin)
    })
  }
  
  def facturacionCompaniaPorSucursal(sucursales : Set[Sucursal]) = {
    estadisticasPorSucursal(sucursales,estadisticaIdentidad,calcularFacturacionTotal,sinFiltroFechas,new Date, new Date)
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
  
  /*def facturacionTotalPorRangoFecha(viajes: Set[Viaje], fechaIni : Date, fechaFin : Date) ={
    val viajesFecha : Set[Viaje] = filtrarPorRangoFecha(viajes, fechaIni, fechaFin)
    
    println("FACTURACION POR RANGO DE FECHA")
    println("-----------------------")
    println("Fecha inicio: " + printDate(fechaIni))
    println("Fecha Fin   : " + printDate(fechaFin))
    println("Facturacion : " + calcularFacturacionTotal(viajesFecha))
    println("-----------------------\n")
  }*/
  
  /*def facturacionTotalPorFecha(viajes: Set[Viaje], fecha : Date) ={
    val viajesFecha : Set[Viaje] = filtrarPorFecha(viajes, fecha)
    
    println("FACTURACION POR FECHA")
    println("-----------------------")
    println("Fecha       : " + printDate(fecha))
    println("Facturacion : " + calcularFacturacionTotal(viajesFecha))
    println("-----------------------\n")
  }*/
  
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