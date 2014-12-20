package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
/*
case class Tupla (nombre:String, cantidad:String){
}

case class Tupla2 (nombre:String, tuplas:Set[Tupla]){
}
*/
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
  
  def tiempoPromedioDeViajes(viajes: Set[Viaje]) : String = {
   
    	 filtroMap(viajes, (_.tiempoHr))
  }  
  
  def costoPromedioDeViajes(viajes: Set[Viaje]) : String = {
 
    	 filtroMap(viajes, (_.costo))
  }

  def gananciaPromedioDeViajes(viajes: Set[Viaje]) : String = {
        filtroMap(viajes, (_.ganancia))
  }
  
  def filtroMap(viajes: Set[Viaje], f :(Viaje => Double)):String =
  {
    if(!viajes.isEmpty)
    	(viajes.toList.map(f).sum / viajes.size).toString
    else
    	0.toString
  }
  
  def cantidadDeViajes(viajes: Set[Viaje]) : String ={
    viajes.size.toString
  }
  
  def cantidadDeEnvios(viajes: Set[Viaje]) : String ={
        filtroSimpleMap(viajes, (_.pedidos.size))
  }
    
  def calcularFacturacionTotal(viajes: Set[Viaje]) : String ={
        filtroSimpleMap(viajes, (_.ganancia))
  }
  
  def filtroSimpleMap(viajes: Set[Viaje], f :(Viaje => Double)):String =
  {
    viajes.toList.map(f).sum.toString
  }
  
 
  //funcion identidad para las estadisticas
  def estadisticaIdentidad(viajes: Set[Viaje], comparacion: (Set[Viaje]) => String,filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date): Set[(String,String)]
  ={
    var viajesFiltrados: Set[Viaje] = filtroFecha(viajes,fechaIni,fechaFin)
    var datos:Set[(String,String)]=Set()
    datos += ("sin filtro" -> comparacion(viajesFiltrados))
    datos
  }
  

   //funcion identidad para las estadisticas   //asi no me estarpia tirando error, pero no la chequee con todo cambiado a string, string
  def estadisticaIdentidad3(viajes: Set[Viaje], comparacion: (Set[Viaje]) => String,filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date): Set[(String, String)]
  ={
    var viajesFiltrados: Set[Viaje] = filtroFecha(viajes,fechaIni,fechaFin)
    var datos:Set[(String, String)]=Set()
    val tupla:(String, String)=("sin filtro",comparacion(viajesFiltrados))
    datos += tupla
    datos
  }
  
  def estadisticaIdentidadPrima(viajes: Set[Viaje], comparacion: (Set[Viaje]) => String,filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date): String
  ={
    var viajesFiltrados: Set[Viaje] = filtroFecha(viajes,fechaIni,fechaFin)
    comparacion(viajesFiltrados)
  }
  
  def estadisticaTransporte(
      viajes: Set[Viaje], comparacion: (Set[Viaje]) => String,filtro: (Set[Viaje],Set[Viaje]=> String,(Set[Viaje],Date,Date) => Set[Viaje],Date,Date) => _,
      filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],fechaIni : Date, fechaFin : Date) :Set[(String, String)] 
  ={
    val viajesCamion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Camion")
    val viajesFurgoneta : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Furgoneta")
    val viajesAvion : Set[Viaje] = filtrarPorTipoDeTransporte(viajes, "Avion")
    
    val tuplaCamion=("camion", filtro(viajesCamion,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaFurgoneta=("furgoneta", filtro(viajesFurgoneta,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaAvion=("avion", filtro(viajesAvion,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    var datos:Set[(String, String)]=Set()
    datos += tuplaCamion 
    datos += tuplaFurgoneta 
    datos += tuplaAvion
    datos
  }
  
  
  // Comparacion entre distintos tipos de Transporte
  def estadisticasPorTransporteSinFiltros(
      viajes: Set[Viaje], comparacion: (Set[Viaje]) => String,filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date)
  :Set[(String, String)] 
  ={
    estadisticaTransporte(viajes,comparacion,estadisticaIdentidadPrima,filtroFecha,fechaIni,fechaFin)
   }
  
  def estadisticasPorTransporte(
      viajes: Set[Viaje], comparacion: (Set[Viaje]) => String,filtro: (Set[Viaje],Set[Viaje]=> String,(Set[Viaje],Date,Date) => Set[Viaje],Date,Date) => Set[(String,String)],
      filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje], fechaIni : Date, fechaFin : Date)
  :Set[(String, String)]
  ={
    
    estadisticaTransporte(viajes,comparacion,filtro,filtroFecha,fechaIni,fechaFin)
   }
  
  //Ejemplos de distintos filtros posibles
  def enviosPorTipoTransporte(viajes: Set[Viaje]) :Set[(String, String)]= {
    var datos:Set[(String, String)]=Set()
    datos = estadisticasPorTransporte(viajes,cantidadDeEnvios,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("ENVIOS POR TRANSPORTE","Cantidad")
    datos
  }
  
  def tiempoPromedioPorTransporte(viajes: Set[Viaje]) :Set[(String, String)]= {
   var datos:Set[(String, String)]=Set()
    datos= estadisticasPorTransporte(viajes,tiempoPromedioDeViajes,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("TIEMPO PROMEDIO DE VIAJE","Tiempo(Hr)")
    datos
  }
  
   def costoPromedioPorTransporte(viajes: Set[Viaje]) :Set[(String, String)]= {
    var datos:Set[(String, String)]=Set()
    datos= estadisticasPorTransporte(viajes,costoPromedioDeViajes,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("COSTO PROMEDIO DE VIAJE","Costo($)")
    datos
   }
   
  def gananciaPromedioPorTransporte(viajes: Set[Viaje]):Set[(String, String)] = {
    var datos:Set[(String, String)]=Set()
    datos= estadisticasPorTransporte(viajes,gananciaPromedioDeViajes,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("GANANCIA PROMEDIO DE VIAJE","Ganancia($)")
    datos
  }
  
  def viajesPorTipoTransporte(viajes: Set[Viaje])  :Set[(String, String)]= {
    var datos:Set[(String, String)]=Set()
    datos= estadisticasPorTransporte(viajes,cantidadDeViajes,estadisticaIdentidad,sinFiltroFechas,new Date,new Date) //("VIAJES POR TRANSPORTE","Cantidad")
    datos
  }
  
  def estadisticaEnvio(viajes: Set[Viaje], comparacion: Set[Viaje] => String,
      filtro: (Set[Viaje],Set[Viaje]=> String,(Set[Viaje],Date,Date) => Set[Viaje],Date,Date) => _,
      filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date) :Set[(String, String)] 
  ={
    val viajesNormal : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Normal")
    val viajesUrgente : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Urgente")
    val viajesRefrigeracion : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Refrigeracion")
    val viajesFragil : Set[Viaje] = filtrarPorTipoDeEnvio(viajes, "Fragil")
    
    val tuplaNormal=("camion", filtro(viajesNormal,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaUrgente=("furgoneta", filtro(viajesUrgente,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaRefrigeracion=("avion", filtro(viajesRefrigeracion,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    val tuplaFragil=("avion", filtro(viajesFragil,comparacion,filtroFecha,fechaIni,fechaFin).toString)
    var datos:Set[(String, String)]=Set()
    datos += tuplaNormal 
    datos += tuplaUrgente 
    datos += tuplaRefrigeracion 
    datos += tuplaFragil
    datos
  }
  
  def estadisticasPorEnvioSinFiltros(
      viajes: Set[Viaje], comparacion: Set[Viaje] => String,filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date) :Set[(String, String)]
  ={
    estadisticaEnvio(viajes,comparacion,estadisticaIdentidadPrima,filtroFecha,fechaIni,fechaFin)
  }
  
  def estadisticasPorEnvio(
      viajes: Set[Viaje], comparacion: Set[Viaje] => String,
      filtro: (Set[Viaje],Set[Viaje]=> String,(Set[Viaje],Date,Date) => Set[Viaje],Date,Date) => Set[(String,String)],
      filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje],
      fechaIni : Date, fechaFin : Date) :Set[(String, String)]
  ={
    
    estadisticaEnvio(viajes,comparacion,filtro,filtroFecha,fechaIni,fechaFin)
    
  }
  
  // Comparacion entre distintas Sucursales
  // Comparacion entre distintas Sucursales
  def estadisticasPorSucursal(sucursales : Set[Sucursal], filtro: (Set[Viaje],Set[Viaje]=> String,(Set[Viaje],Date,Date) => Set[Viaje],Date,Date) => Set[(String,String)],
      comparacion: Set[Viaje] => String, filtroFecha: (Set[Viaje],Date,Date) => Set[Viaje], fechaIni : Date, fechaFin : Date) :Set[(String, Set[(String,String)])]
  ={
    var datos2:Set[(String, Set[(String,String)])]=Set()
    var datos:Set[(String,String)]=Set()
    sucursales.foreach({s =>
      var viajesSucursal = s.viajesRealizados 
      datos = datos ++ filtro(viajesSucursal,comparacion,filtroFecha,fechaIni,fechaFin)
      datos2 += (s.pais -> datos)
      datos = Set()
    })
    datos2
  }
  
  def facturacionCompaniaPorSucursal(sucursales : Set[Sucursal]) = {
    estadisticasPorSucursal(sucursales,estadisticaIdentidad,calcularFacturacionTotal,sinFiltroFechas,new Date, new Date)
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