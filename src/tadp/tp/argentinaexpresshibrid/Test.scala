package tadp.tp.argentinaexpresshibrido
import org.junit.Test
import org.junit.Assert._

import java.text.SimpleDateFormat

import java.util.{Calendar, Date}

class TransporteTest {
  
/*  Copy Paste Cheatsheet
    val avion1 = new Avion(Set())
    val furgoneta1 = new Furgoneta(Set())
    val camion1 = new Camion(Set())
    val avion2 = new Avion(Set())
    val furgoneta2 = new Furgoneta(Set())
    val camion2 = new Camion(Set())
    val sucursalArg = new Sucursal(Set(avion1,furgoneta1,camion1),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(avion2,furgoneta2,camion2),500,"Chile")
    val laCasaCentral = new CasaCentral(Set(),1500,"EEUU")
    val unGPS = new SeguimientoGPS
    val unVideo = new SeguimientoVideo
    val infraAnimales = new InfraestructuraAnimales
	val infraSustancias = new InfraestructuraSustancias
*/
  

  @Test
  def `el transporte debe tener espacio disponible` = {
    
    var sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    var sucursalChi = new Sucursal(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)

    
    var unEnvioGrande = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(7,11,2014))
    var unEnvioChico = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
   
    assertEquals(true, camion1.puedeCargar(unEnvioChico))
    assertEquals(false, camion1.puedeCargar(unEnvioGrande))    
  }

  @Test
  def `los pedidos se agregan a un transporte disponible` = {
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvioChico = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
   
    sucursalArg.asignarEnvioATransporte(unEnvioChico)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioChico))	  
  }
  
  @Test
  def `las caracteristicas deben ser las mismas` = {
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
    var otroEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
    var unEnvioUrgente = new Fragil(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
    
    sucursalArg.asignarEnvioATransporte(unEnvioNormal)
    sucursalArg.asignarEnvioATransporte(otroEnvioNormal)
    sucursalArg.asignarEnvioATransporte(unEnvioUrgente)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioNormal))
    assertEquals(true,camion1.enviosAsignados.contains(otroEnvioNormal))
    assertEquals(false,camion1.enviosAsignados.contains(unEnvioUrgente))
  }  

  
  @Test
  def `todos los pedidos deben tener el mismo destino` = {
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(),500,"Chile")
    val laCasaCentral = new CasaCentral(Set(),1500,"EEUU")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
    var otroEnvioNormal = new Normal(Set(),sucursalArg,laCasaCentral,10,new Date(7,11,2014))
    
    sucursalArg.asignarEnvioATransporte(unEnvioNormal)
    sucursalArg.asignarEnvioATransporte(otroEnvioNormal)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioNormal))
    assertEquals(false,camion1.enviosAsignados.contains(otroEnvioNormal))    
  }
  
 
  @Test
  def `la sucursal debe tener espacio disponible` = {
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(),30,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
    var otroEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,25,new Date(7,11,2014))
    
    sucursalArg.asignarEnvioATransporte(unEnvioNormal)
    sucursalArg.asignarEnvioATransporte(otroEnvioNormal)
    assertEquals(true,camion1.enviosAsignados.contains(unEnvioNormal))
    assertEquals(false,camion1.enviosAsignados.contains(otroEnvioNormal))
  }   

   
  @Test
  def `los aviones solo viajan a mas de 1000 km` = {
    
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")
    val sucursalChi = new Sucursal(Set(),500,"Chile")
    val avion1 = new Avion(Set(), sucursalArg)
    
    var unEnvioNormal = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
    var otroEnvioNormal = new Normal(Set(),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
  
    sucursalArg.asignarEnvioATransporte(otroEnvioNormal)
    sucursalArg.asignarEnvioATransporte(unEnvioNormal)
    

    assertEquals(true,avion1.enviosAsignados.contains(unEnvioNormal))
    assertEquals(false,avion1.enviosAsignados.contains(otroEnvioNormal))
  }
  
  @Test
  def `un transporte que transporte animales` = {
    val infraAnimales = new InfraestructuraAnimales
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")
    val camion1 = new Camion(Set(infraAnimales),sucursalArg)
    val camion2 = new Camion(Set(), sucursalArg2)
    
    var unEnvioAnimal = new Normal(Set(infraAnimales),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
    var otroEnvioAnimal = new Normal(Set(infraAnimales),sucursalArg2,sucursalArg,10,new Date(7,11,2014))
    
    sucursalArg.asignarEnvioATransporte(unEnvioAnimal)
    sucursalArg2.asignarEnvioATransporte(otroEnvioAnimal)

    assertEquals(true,camion1.enviosAsignados.contains(unEnvioAnimal))   
    assertEquals(false,camion2.enviosAsignados.contains(otroEnvioAnimal))
  }  

  @Test
  def `un transporte que transporte sustancias peligrosas` = {
	val infraSustancias = new InfraestructuraSustancias
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),1000,"Argentina")
	val camion1 = new Camion(Set(infraSustancias),sucursalArg)
    val camion2 = new Camion(Set(),sucursalArg2)
    
    var unEnvioPeligroso = new Normal(Set(infraSustancias),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
    var otroEnvioPeligroso = new Normal(Set(infraSustancias),sucursalArg2,sucursalArg,10,new Date(7,11,2014))
    
    sucursalArg.asignarEnvioATransporte(unEnvioPeligroso)
    sucursalArg2.asignarEnvioATransporte(otroEnvioPeligroso)

    assertEquals(true,camion1.enviosAsignados.contains(unEnvioPeligroso))   
    assertEquals(false,camion2.enviosAsignados.contains(otroEnvioPeligroso))    
  } 

  @Test
  def `las sucursales reciben un envio` = {
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvioChico = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
   
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
    
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvioChico = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
   
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
   
  @Test
  def `calculo de costo base` = {
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvio = new Normal(Set(),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
    
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
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,10,new Date(7,11,2014))
    
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
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(), sucursalArg)
    var unEnvio = new Refrigeracion(Set(),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
    
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
	val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalChi = new Sucursal(Set(),500,"Chile")
    val avion1 = new Avion(Set(), sucursalArg)
    
    var unEnvio = new Normal(Set(),sucursalArg,sucursalChi,100,new Date(7,11,2014))
    
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
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val casaCentral = new CasaCentral(Set(),500,"Chile")
    val camion1 = new Camion(Set(), sucursalArg)
    
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
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
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
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(), sucursalArg)
    
    var unEnvio = new Normal(Set(),sucursalArg,sucursalArg2,9,new Date(7,11,2014))
    
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
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(unGPS), sucursalArg)
    
    var unEnvio = new Normal(Set(),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $10
    // GPS = $500
    // No hay mas modificadores
    assertEquals(50510, //expected
    			 camion1.calcularCostoViaje, //actual
    			 0.01) //Delta    
  }  
  
  @Test
  def `el Video agrega costo` = {
    val unVideo = new SeguimientoVideo
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(unVideo), sucursalArg)
    
    var unEnvio = new Normal(Set(),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $10
    // Video = $3740
    // No hay mas modificadores
    assertEquals(53750, //expected
    			 camion1.calcularCostoViaje, //actual
    			 0.01) //Delta      
  }  
   
  @Test
  def `costo si viajan sustancias peligrosas` = {
    val infraSustancias = new InfraestructuraSustancias
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(infraSustancias), sucursalArg)
    
    var unEnvio = new Normal(Set(infraSustancias),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
    
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
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(infraAnimales), sucursalArg)
    
    var unEnvio = new Normal(Set(infraAnimales),sucursalArg,sucursalArg2,10,new Date(7,11,2014))
    
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
    val sucursalArg = new Sucursal(Set(),1000,"Argentina")   
    val sucursalArg2 = new Sucursal(Set(),500,"Argentina")
    val camion1 = new Camion(Set(infraSustancias), sucursalArg)
    
    var unEnvio = new Urgente(Set(infraSustancias),sucursalArg,sucursalArg2,15,new Date(7,11,2014))
    
    sucursalArg.asignarEnvioATransporte(unEnvio)

    // CostoTransporte = $100/km x 500 km = $50000
    // CostoBase = $20
    // Sustancias base= $600
    // Sustancias extra camion con urgentes = $3 * 15/45 = $1
    // No hay mas modificadores
    assertEquals(50621, //expected
    			 camion1.calcularCostoViaje, //actual
    			 0.01) //Delta        
  }  
  
  @Test
  def `calculo de la ganancia bruta` = {
    
  }  
}

/*
class EstadisticasTest {
  
  @Test
  def `asd` = {
    
  }
  
}
*/