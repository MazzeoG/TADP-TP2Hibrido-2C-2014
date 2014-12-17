package tadp.tp.argentinaexpresshibrido

import org.junit.Test
import org.junit.Assert._
import org.junit.Before
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

class TransporteTest {
  
  @Before	
  	val sucursalArg = new Sucursal(Set(),1000,"Argentina")
  	val sucursalChi = new Sucursal(Set(),500,"Chile")
  	val camion1 = new Camion(Set(), sucursalArg)
  
  @Test
  def `el transporte debe tener espacio disponible` = {
    var unEnvioGrande = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,7))
    var unEnvioChico = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
   
    assertTrue(camion1.puedeCargar(unEnvioChico))
    assertFalse(camion1.puedeCargar(unEnvioGrande))    
  }

  @Test
  def `los pedidos se agregan a un transporte disponible` = {
    var unEnvioChico = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
   
    sucursalArg.asignarEnvioATransporte(unEnvioChico)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioChico))	  
  }
  
  @Test
  def `las caracteristicas deben ser las mismas` = {
    var unEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
    var otroEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
    var unEnvioUrgente = new Fragil(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvioNormal)
    sucursalArg.asignarEnvioATransporte(otroEnvioNormal)
    sucursalArg.asignarEnvioATransporte(unEnvioUrgente)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioNormal))
    assertEquals(true,camion1.enviosAsignados.contains(otroEnvioNormal))
    assertEquals(false,camion1.enviosAsignados.contains(unEnvioUrgente))
  }  

  
  @Test
  def `todos los pedidos deben tener el mismo destino` = {
    val laCasaCentral = new CasaCentral(Set(),1500,"EEUU")
    var unEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
    var otroEnvioNormal = new Normal(Set(),sucursalArg,laCasaCentral,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvioNormal)
    sucursalArg.asignarEnvioATransporte(otroEnvioNormal)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioNormal))
    assertEquals(false,camion1.enviosAsignados.contains(otroEnvioNormal))    
  }
  
 
  @Test
  def `la sucursal debe tener espacio disponible` = {
    val sucursalChi = new Sucursal(Set(),30,"Chile")
    var unEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
    var otroEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,25,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvioNormal)
    sucursalArg.asignarEnvioATransporte(otroEnvioNormal)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioNormal))
    assertEquals(false,camion1.enviosAsignados.contains(otroEnvioNormal))
  }   

   
  @Test
  def `los aviones solo viajan a mas de 1000 km` = {
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")
    val avion1 = new Avion(Set(), sucursalArg)
    
    var unEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
    var otroEnvioNormal = new Normal(Set(),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
  
    sucursalArg.asignarEnvioATransporte(otroEnvioNormal)
    sucursalArg.asignarEnvioATransporte(unEnvioNormal)
    
    assertEquals(true,avion1.enviosAsignados.contains(unEnvioNormal))
    assertEquals(false,avion1.enviosAsignados.contains(otroEnvioNormal))
  }
  
  @Test
  def `un transporte que transporte animales` = {
    val infraAnimales = new InfraestructuraAnimales
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")
    val camion1 = new Camion(Set(infraAnimales),sucursalArg)
    val camion2 = new Camion(Set(), sucursalArg2)
    
    var unEnvioAnimal = new Normal(Set(infraAnimales),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    var otroEnvioAnimal = new Normal(Set(infraAnimales),sucursalArg2,sucursalArg,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvioAnimal)
    sucursalArg2.asignarEnvioATransporte(otroEnvioAnimal)

    assertEquals(true,camion1.enviosAsignados.contains(unEnvioAnimal))   
    assertEquals(false,camion2.enviosAsignados.contains(otroEnvioAnimal))
  }  

  @Test
  def `un transporte que transporte sustancias peligrosas` = {
	val infraSustancias = new InfraestructuraSustancias
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")
	val camion1 = new Camion(Set(infraSustancias),sucursalArg)
    val camion2 = new Camion(Set(),sucursalArg2)
    
    var unEnvioPeligroso = new Normal(Set(infraSustancias),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    var otroEnvioPeligroso = new Normal(Set(infraSustancias),sucursalArg2,sucursalArg,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvioPeligroso)
    sucursalArg2.asignarEnvioATransporte(otroEnvioPeligroso)

    assertEquals(true,camion1.enviosAsignados.contains(unEnvioPeligroso))   
    assertEquals(false,camion2.enviosAsignados.contains(otroEnvioPeligroso))    
  } 

  @Test
  def `las sucursales reciben un envio` = {
    var unEnvioChico = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
   
    sucursalArg.asignarEnvioATransporte(unEnvioChico)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioChico))
    assertEquals(false,sucursalChi.envios.contains(unEnvioChico))
    
    sucursalArg.mandarTransporte(camion1)
    assertEquals(false,camion1.enviosAsignados.contains(unEnvioChico))
    assertEquals(true,sucursalChi.envios.contains(unEnvioChico))
    assertEquals(Set(),camion1.enviosAsignados)
    assertEquals(null,camion1.sucursalDestino)
  }
  
  @Test
  def `un envio es retirado de la Sucursal` = {
    var unEnvioChico = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
   
    sucursalArg.asignarEnvioATransporte(unEnvioChico)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioChico))
    assertEquals(false,sucursalChi.envios.contains(unEnvioChico))
    
    sucursalArg.mandarTransporte(camion1)
    assertEquals(false,camion1.enviosAsignados.contains(unEnvioChico))
    assertEquals(true,sucursalChi.envios.contains(unEnvioChico))
    assertEquals(Set(),camion1.enviosAsignados)
    assertEquals(null,camion1.sucursalDestino)
    
    sucursalChi.retirarEnvio(unEnvioChico)
    
    assertEquals(false,sucursalChi.envios.contains(unEnvioChico))    
  }
}


 class PrecioYCostoTest {

  @Before	
  	val sucursalArg = new Sucursal(Set(),1000,"Argentina")
  	val sucursalChi = new Sucursal(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
  
  @Test
  def `calculo de costo base` = {
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    var unEnvio = new Normal(Set(),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $10
    // No hay mas modificadores
    assertEquals(50010, //expected
    			 camion1.calcularCostoViaje, //actual
    			 0.01) //Delta
  }

  @Test
  def `calculo con peajes` = {
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 1500 km = $150000
    // CostoBase = $10
    // Peajes = $12 x 5 = $60
    // No hay mas modificadores
    assertEquals(150070, //expected
    			 camion1.calcularCostoViaje,
    			 0.01) //Delta    
  }  
  
  @Test
  def `calculo con refrigeracion` = {
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    var unEnvio = new Refrigeracion(Set(),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $70
    // Refrigeracion = $5
    // No hay mas modificadores
    assertEquals(50075, //expected
    			 camion1.calcularCostoViaje,
    			 0.01) //Delta    
  }
  
  @Test
  def `los aviones pagan impuestos entre paises` = {
    val avion1 = new Avion(Set(), sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $500/km x 1500 km = $750000
    // CostoBase = $10
    // Impuesto avion = $750000 * 0.1 = $75000
    // No hay mas modificadores
    assertEquals(825010, //expected
    			 avion1.calcularCostoViaje, //actual
    			 0.01) //Delta    
  }  
 
  @Test
  def `un camion a CC en la ultima semana` = {
    val casaCentral = new CasaCentral(Set(),500,"Chile")
    var unEnvio = new Normal(Set(),sucursalArg,casaCentral,10,new Date(2014,11,30)) //Año,Mes,Dia
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    val asd = camion1.ultimaSemanaDelMes
    // CostoTransporte = $100/km x 1500 km = $150000
    // CostoBase = $10
    // Peajes = $12 x 5 = $60
    // CostoReparacion = $150000 * 0.02 = $3000
    // No hay mas modificadores
    assertEquals(153070, //expected
    			 camion1.calcularCostoViaje,
    			 0.01) //Delta    
  }  
  
  @Test
  def `los aviones a CC pasado el 20 de mes` = {
    val casaCentral = new CasaCentral(Set(),500,"Chile")
    val avion1 = new Avion(Set(), sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,casaCentral,100,new Date(2014,11,21)) //Año,Mes,Dia
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    
    // CostoTransporte = $500/km x 1500 km = $750000
    // CostoBase = $10
    // Impuestos = $750000 * 0.1 = $75000
    // Reduccion Insumos = $750000 * (-0.2) = -$150000
    // No hay mas modificadores
    assertEquals(675010, //expected
    			 avion1.calcularCostoViaje,
    			 0.01) //Delta    
    
  }  
 
  @Test
  def `los transportes con menos del 20% del volumen afectan el costo` = {
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    var unEnvio = new Normal(Set(),sucursalArg,sucursalArg2,9,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $10
    // Multiplicador = $50000 * (1+ 9/45) = $10000
    // No hay mas modificadores
    assertEquals(60010, //expected
    			 camion1.calcularCostoViaje, //actual
    			 0.01) //Delta
  }  
  
  @Test
  def `el GPS agrega costo` = {
    val unGPS = new SeguimientoGPS
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    sucursalArg.transporte = Set()
    val camionGps = new Camion(Set(unGPS), sucursalArg)
    
    var unEnvio = new Normal(Set(),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $10
    // GPS = $500
    // No hay mas modificadores
    assertEquals(50510, //expected
    			 camionGps.calcularCostoViaje, //actual
    			 0.01) //Delta    
  }  
  
  @Test
  def `el Video agrega costo` = {
    val unVideo = new SeguimientoVideo
    sucursalArg.transporte = Set()   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camionVideo = new Camion(Set(unVideo), sucursalArg)
    
    var unEnvio = new Normal(Set(),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $10
    // Video = $3740
    // No hay mas modificadores
    assertEquals(53750, //expected
    			 camionVideo.calcularCostoViaje, //actual
    			 0.01) //Delta      
  }  
   
  @Test
  def `costo si viajan sustancias peligrosas` = {
    val infraSustancias = new InfraestructuraSustancias
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(infraSustancias), sucursalArg)
    var unEnvio = new Normal(Set(infraSustancias),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $10
    // Sustancias = $600
    // No hay mas modificadores
    assertEquals(50610, //expected
    			 camion1.calcularCostoViaje, //actual
    			 0.01) //Delta    
  }
  
  @Test
  def `costo si viajan animales` = {
    val infraAnimales = new InfraestructuraAnimales
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(infraAnimales), sucursalArg)
    var unEnvio = new Normal(Set(infraAnimales),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $10
    // Animales = $137 porque distancia >= 200 km
    // No hay mas modificadores
    assertEquals(50147, //expected
    			 camion1.calcularCostoViaje, //actual
    			 0.01) //Delta   
  }  
  
  @Test
  def `sustancias peligrosas y paquetes urgentes a la vez` = {
    val infraSustancias = new InfraestructuraSustancias
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(infraSustancias), sucursalArg)
    var unEnvio = new Urgente(Set(infraSustancias),sucursalArg,sucursalArg2,10,new Date(2014,11,7))
    var otroEnvio = new Urgente(Set(infraSustancias),sucursalArg,sucursalArg2,5,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    sucursalArg.asignarEnvioATransporte(otroEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $40
    // Sustancias base= $600
    // Sustancias extra camion con urgentes = $3 * 15/45 = $1
    // No hay mas modificadores
    assertEquals(50641, //expected
    			 camion1.calcularCostoViaje, //actual
    			 0.01) //Delta         
  }   
}


class EstadisticasTest {

  @Before	
  	val sucursalArg = new Sucursal(Set(),1000,"Argentina")
  	val sucursalChi = new Sucursal(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
  	val stats = new Estadisticas
  	
  @Test
  def `Cantidad de viajes por transporte` = {
       
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")  
    val furgoneta1 = new Furgoneta(Set(), sucursalArg)
    val avion1 = new Avion(Set(),sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,40,new Date(2014,11,7))
    var otroEnvio = new Normal(Set(),sucursalArg,sucursalArg2,8,new Date(2014,11,7))
    var unEnvio2 = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,7)) 
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    sucursalArg.asignarEnvioATransporte(unEnvio2)
    sucursalArg.asignarEnvioATransporte(otroEnvio)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvio))
    assertEquals(true,furgoneta1.enviosAsignados.contains(otroEnvio))
    assertEquals(true,avion1.enviosAsignados.contains(unEnvio2))
        
    sucursalArg.mandarTransporte(camion1)
    sucursalArg.mandarTransporte(furgoneta1)
    sucursalArg.mandarTransporte(avion1)
    
    stats.viajesPorTipoTransporte(sucursalArg.viajesRealizados) // Imprime estadistica en consola
  }
  
  @Test
  def `Cantidad de envios por transporte` = {
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")  
    val furgoneta1 = new Furgoneta(Set(), sucursalArg)
    val avion1 = new Avion(Set(),sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,45,new Date(2014,11,7))
    var otroEnvio = new Normal(Set(),sucursalArg,sucursalArg2,4,new Date(2014,11,7))
    var unEnvio2 = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,7))
    var unEnvio3 = new Normal(Set(),sucursalArg,sucursalArg2,3,new Date(2014,11,7))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    sucursalArg.asignarEnvioATransporte(unEnvio2)
    sucursalArg.asignarEnvioATransporte(unEnvio3)
    sucursalArg.asignarEnvioATransporte(otroEnvio)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvio))
    assertEquals(true,furgoneta1.enviosAsignados.contains(otroEnvio))
    assertEquals(true,furgoneta1.enviosAsignados.contains(unEnvio3))
    assertEquals(true,avion1.enviosAsignados.contains(unEnvio2))
        
    sucursalArg.mandarTransporte(camion1)
    sucursalArg.mandarTransporte(furgoneta1)
    sucursalArg.mandarTransporte(avion1)
    
    stats.enviosPorTipoTransporte(sucursalArg.viajesRealizados) // Imprime estadistica en consola
  }
  
  @Test
  def `Facturacion Total Por Rango de Fecha` = {
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")  
    val furgoneta1 = new Furgoneta(Set(), sucursalArg)
    val avion1 = new Avion(Set(),sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,40,new Date(2014,11,7))
    var otroEnvio = new Normal(Set(),sucursalArg,sucursalArg2,8,new Date(2014,11,14))
    var unEnvio2 = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,21)) 
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    sucursalArg.asignarEnvioATransporte(unEnvio2)
    sucursalArg.asignarEnvioATransporte(otroEnvio)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvio))
    assertEquals(true,furgoneta1.enviosAsignados.contains(otroEnvio))
    assertEquals(true,avion1.enviosAsignados.contains(unEnvio2))
        
    sucursalArg.mandarTransporte(camion1)
    sucursalArg.mandarTransporte(furgoneta1)
    sucursalArg.mandarTransporte(avion1)
    
    stats.facturacionTotalPorRangoFecha(sucursalArg.viajesRealizados, new Date(2014,11,1), new Date(2014,11,5)) // Imprime estadistica en consola
    stats.facturacionTotalPorRangoFecha(sucursalArg.viajesRealizados, new Date(2014,11,6), new Date(2014,11,11)) // Imprime estadistica en consola
    stats.facturacionTotalPorRangoFecha(sucursalArg.viajesRealizados, new Date(2014,11,12), new Date(2014,11,22)) // Imprime estadistica en consola
  }  
  
  @Test
  def `Facturacion Total de una fecha` = {
       
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")  
    val furgoneta1 = new Furgoneta(Set(), sucursalArg)
    val avion1 = new Avion(Set(),sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,40,new Date(2014,11,7))
    var otroEnvio = new Normal(Set(),sucursalArg,sucursalArg2,8,new Date(2014,11,14))
    var unEnvio2 = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,21)) 
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    sucursalArg.asignarEnvioATransporte(unEnvio2)
    sucursalArg.asignarEnvioATransporte(otroEnvio)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvio))
    assertEquals(true,furgoneta1.enviosAsignados.contains(otroEnvio))
    assertEquals(true,avion1.enviosAsignados.contains(unEnvio2))
        
    sucursalArg.mandarTransporte(camion1)
    sucursalArg.mandarTransporte(furgoneta1)
    sucursalArg.mandarTransporte(avion1)
    
    stats.facturacionTotalPorFecha(sucursalArg.viajesRealizados, new Date(2014,11,7)) // Imprime estadistica en consola
  }

  @Test
  def `Facturacion Total Por Sucursal` = {
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")  
    val furgoneta1 = new Furgoneta(Set(), sucursalArg)
    val avion1 = new Avion(Set(),sucursalArg)
    val camion2 = new Camion(Set(), sucursalArg2)
    val furgoneta2 = new Furgoneta(Set(), sucursalArg2)
    val avion2 = new Avion(Set(),sucursalArg2)
    val camion3 = new Camion(Set(), sucursalChi)
    val furgoneta3 = new Furgoneta(Set(), sucursalChi)
    val avion3 = new Avion(Set(),sucursalChi)
    
    var unEnvio0 = new Normal(Set(),sucursalArg,sucursalChi,40,new Date(2014,11,7))
    var unEnvio1 = new Normal(Set(),sucursalChi,sucursalArg2,8,new Date(2014,11,14))
    var unEnvio2 = new Normal(Set(),sucursalArg2,sucursalChi,100,new Date(2014,11,21)) 
    var unEnvio3 = new Normal(Set(),sucursalArg,sucursalArg2,3,new Date(2014,11,7))    
    
    sucursalArg.asignarEnvioATransporte(unEnvio0)
    sucursalChi.asignarEnvioATransporte(unEnvio1)
    sucursalArg2.asignarEnvioATransporte(unEnvio2)
    sucursalArg.asignarEnvioATransporte(unEnvio3)
        
    sucursalArg.mandarTransporte(camion1)
    sucursalArg.mandarTransporte(furgoneta1)
    sucursalArg.mandarTransporte(avion1)

    sucursalArg2.mandarTransporte(camion2)
    sucursalArg2.mandarTransporte(furgoneta2)
    sucursalArg2.mandarTransporte(avion2)
    
    sucursalChi.mandarTransporte(camion3)
    sucursalChi.mandarTransporte(furgoneta3)
    sucursalChi.mandarTransporte(avion3)
    
    stats.facturacionCompaniaPorSucursal(Set(sucursalArg,sucursalArg2, sucursalChi)) // Imprime estadistica en consola
  } 
  
  @Test
  def `Tiempo promedio por transporte` = {
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")  
    val furgoneta1 = new Furgoneta(Set(), sucursalArg)
    val avion1 = new Avion(Set(),sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,40,new Date(2014,11,7))
    var otroEnvio = new Normal(Set(),sucursalArg,sucursalArg2,8,new Date(2014,11,14))
    var unEnvio2 = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,21)) 
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    sucursalArg.asignarEnvioATransporte(unEnvio2)
    sucursalArg.asignarEnvioATransporte(otroEnvio)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvio))
    assertEquals(true,furgoneta1.enviosAsignados.contains(otroEnvio))
    assertEquals(true,avion1.enviosAsignados.contains(unEnvio2))
        
    sucursalArg.mandarTransporte(camion1)
    sucursalArg.mandarTransporte(furgoneta1)
    sucursalArg.mandarTransporte(avion1)
    
    stats.tiempoPromedioPorTransporte(sucursalArg.viajesRealizados) // Imprime estadistica en consola
  }
  
 @Test
  def `Ganancia promedio por transporte` = {
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")  
    val furgoneta1 = new Furgoneta(Set(), sucursalArg)
    val avion1 = new Avion(Set(),sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,40,new Date(2014,11,7))
    var otroEnvio = new Normal(Set(),sucursalArg,sucursalArg2,8,new Date(2014,11,14))
    var unEnvio2 = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,21)) 
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    sucursalArg.asignarEnvioATransporte(unEnvio2)
    sucursalArg.asignarEnvioATransporte(otroEnvio)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvio))
    assertEquals(true,furgoneta1.enviosAsignados.contains(otroEnvio))
    assertEquals(true,avion1.enviosAsignados.contains(unEnvio2))
        
    sucursalArg.mandarTransporte(camion1)
    sucursalArg.mandarTransporte(furgoneta1)
    sucursalArg.mandarTransporte(avion1)
    
    stats.gananciaPromedioPorTransporte(sucursalArg.viajesRealizados) // Imprime estadistica en consola
  }
  @Test
  def `Costo promedio por transporte` = {
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")  
    val furgoneta1 = new Furgoneta(Set(), sucursalArg)
    val avion1 = new Avion(Set(),sucursalArg)
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,40,new Date(2014,11,7))
    var otroEnvio = new Normal(Set(),sucursalArg,sucursalArg2,8,new Date(2014,11,14))
    var unEnvio2 = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(2014,11,21)) 
    
    sucursalArg.asignarEnvioATransporte(unEnvio)
    sucursalArg.asignarEnvioATransporte(unEnvio2)
    sucursalArg.asignarEnvioATransporte(otroEnvio)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvio))
    assertEquals(true,furgoneta1.enviosAsignados.contains(otroEnvio))
    assertEquals(true,avion1.enviosAsignados.contains(unEnvio2))
        
    sucursalArg.mandarTransporte(camion1)
    sucursalArg.mandarTransporte(furgoneta1)
    sucursalArg.mandarTransporte(avion1)
    
    stats.costoPromedioPorTransporte(sucursalArg.viajesRealizados) // Imprime estadistica en consola
  }  
}
