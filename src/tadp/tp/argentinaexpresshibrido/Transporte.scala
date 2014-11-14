package tadp.tp.argentinaexpresshibrido

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

abstract class Transporte (val serviciosExtra : Set[ServicioExtra], var sucursalOrigen: Sucursal)
    extends CalculadorDistancia{
  val volumenDeCarga : Int
  val costoPorKm : Int
  val velocidad : Int
  var sucursalDestino: Sucursal = null
  var enviosAsignados: Set[Envio] = Set()
  var fechaEnvio :Date = null
  val valorPeaje: Int
  val volOcupadoMulti: Double = 0.2 
  // Inicializacion de los transportes
  if (sucursalOrigen != null)
	  sucursalOrigen.agregarTransporte(this)
  
  
  
  //Esta repetido! volumenDisponible()
  def espacioDisponible():Int={
    this.volumenDeCarga - this.volumenEnvios
  }

  def puedeCargarUrgentes() ={
    true
  }
   def puedeCargarFragiles() ={
    false
  }
  def puedeCargarRefrigerados() ={
    false
  }
  
  def sinEnviosAsignados : Boolean = {
    this.enviosAsignados.isEmpty
  }
  
  def volumenEnvios() :Int = {
//     var volumenOcupado:Int = 0;
//     this.enviosAsignados.foreach((e:Envio) =>volumenOcupado+= e.volumen)
//     volumenOcupado
     this.enviosAsignados.map((e:Envio) => e.volumen).sum
  }
  
  def volumenDisponible() :Int = {
    this.volumenDeCarga - this.volumenEnvios
  }
  
  //Funcion utilizada para validar que un transporte pueda cargar un envio
  def puedeCargar(envio:Envio) : Boolean ={
    var cargable : Boolean = coincideDestino(envio) && entraEnTransporte(envio) && entraEnAvion(envio) && infraestructuraNecesaria(envio)
    envio match {
  case envio :Fragil => cargable = cargable && puedeCargarFragiles
  case envio :Urgente => cargable = cargable && puedeCargarUrgentes
  case envio :Refrigeracion => cargable = cargable && puedeCargarRefrigerados
  case _ =>
    }
    cargable
  }
  
  def coincideDestino(envio:Envio) : Boolean = {
   
    enviosAsignados.size match
    {
    	case 0 => true
    	case _ => this.enviosAsignados.forall((e:Envio) => e.sucursalDestino==envio.sucursalDestino);
    }
  }

  //Si el transporte cuyo envio esta siendo cargado es un avion, valida que la distancia sea mayor a 1000
  def entraEnDestino(envio:Envio): Boolean ={
    envio.sucursalDestino.volumenDisponible >= envio.volumen
  }
  
  def entraEnTransporte(envio:Envio) : Boolean ={
    this.volumenDisponible >= envio.volumen
  }
  
  def entraEnAvion(envio : Envio) : Boolean ={
    true
  }
  
  def infraestructuraNecesaria(envio: Envio) : Boolean ={
    envio.caracteristicas.forall(carac => this.serviciosExtra.contains(carac)) 
  }
  
  
   val costoDeEnvios = this.enviosAsignados.map((e:Envio) => e.costoBase()).sum
  
  val sumaPrecioPeajes  =  { (unCosto : Int ) => unCosto + precioPeajes}
  
  val sumaCostoRefrigeracion  = { (unCosto : Int ) => unCosto + costoDeRefrigeracion }
  
  val avionConPeaje = { (unCosto : Double ) => unCosto + (sucursalOrigen.pais == this.sucursalDestino.pais 
      									match {
      											case true => 0
      											case _ => costoDeTransporte * impuestoAvion
  												}) }
    
  val costoDeTransporte	 = this.costoTransporte(sucursalOrigen , sucursalDestino)
  
  val sumaCostoRevisionTecnica  = { (unCosto : Double ) => unCosto + (this.sucursalDestino.esCasaCentral() && this.ultimaSemanaDelMes()
  													match {
  													case true => costoRevisionTecnica(costoDeTransporte)
  													case _ => 0
  													}) }
  									
  val sumaReduccionDeInsumos  = { (unCosto : Double ) => unCosto  - (pasadoElDia20 && sucursalDestino.esCasaCentral
		  												match {
		  												case true => reduccionInsumos(costoDeTransporte)
		  												case _ => 0
    													})}  
  
  
 
    													
  val multiplicaCostoTransporte  = { (unCosto : Double ) => unCosto +  costoDeTransporte*multiplicador} 
 
  
  val sumaCostoGPS  = { (unCosto : Double ) => unCosto + costoGPS} 
  val sumaCostoVideo  = { (unCosto : Double ) => unCosto + costoVideo} 
  val sumaCostoSustanciasPeligrosas  = { (unCosto : Double ) => unCosto + costoSustanciasPeligrosas} 
  val sumaCostoAnimales  = { (unCosto : Double ) => unCosto + costoAnimales} 

  implicit class FExt[A, B](f: A => B) {
    def <*[C](g: C => A) = {
      f compose g
    }
  }
  
  val calcularCostoViaje =  (sumaCostoAnimales <* sumaCostoSustanciasPeligrosas <* sumaCostoVideo <* sumaCostoGPS <* sumaReduccionDeInsumos<* sumaCostoRevisionTecnica <* avionConPeaje <* sumaCostoRefrigeracion <* sumaPrecioPeajes)(costoDeEnvios)
   def costoTransporte(sucursalOrigen: Sucursal, sucursalDestino: Sucursal) : Double = {
    this match {
      case transporte : Avion => this.costoPorKm * this.distanciaAereaEntre(sucursalOrigen , sucursalDestino)
      case transporte : Camion => this.costoPorKm * this.distanciaTerrestreEntre(sucursalOrigen , sucursalDestino)
      case transporte : Furgoneta => this.costoPorKm * this.distanciaTerrestreEntre(sucursalOrigen , sucursalDestino)
    }
  }
  
  def precioPeajes():Int={
    (cantidadPeajesEntre(sucursalOrigen,sucursalDestino) * this.valorPeaje)
  }
  
  def multiplicador():Double= {
	1
  }
  
  def agregarEnvio(envio : Envio): Transporte = {
    this.enviosAsignados = this.enviosAsignados ++ Set(envio)
    if (enviosAsignados.size == 1) {
      sucursalDestino = envio.sucursalDestino
      fechaEnvio = envio.fecha
    }
    	
    this
  }
  
  def tieneSeguimientoGPS(): Boolean = {
    !this.serviciosExtra.find((s: ServicioExtra) => s.soyGPS).isEmpty
  }
  
  def tieneSeguimientoVideo(): Boolean = {
    !this.serviciosExtra.find((s: ServicioExtra) => s.soyVideo).isEmpty
  }
  
   // Falta definir la mutua exclusion
  def puedeLlevarAnimales() : Boolean = {
    !this.serviciosExtra.find((s: ServicioExtra) => s.soyInfraestructuraAnimales).isEmpty
  }
  
  def puedeLlevarSustancias() : Boolean = {
    !this.serviciosExtra.find((s: ServicioExtra) => s.soyInfraestructuraSustancias).isEmpty
  }
  
  def impuestoAvion() : Double = {
    0
  }
  
  def costoDeRefrigeracion(): Double ={
    0
  }

  def ultimaSemanaDelMes(): Boolean = {
    var cal: Calendar = Calendar.getInstance()
	cal.setTime(fechaEnvio)
	
      (cal.get(Calendar.MONTH)) match
	  {
	  	case 1 => cal.get(Calendar.WEEK_OF_MONTH) == 4
	  	case _ => cal.get(Calendar.WEEK_OF_MONTH) == 5
	  }
  }
	
  def pasadoElDia20(): Boolean = {
	var cal: Calendar = Calendar.getInstance()
	cal.setTime(fechaEnvio)
	cal.get(Calendar.DAY_OF_MONTH) > 20
  }
  
  def costoRevisionTecnica(costoDeTransporte: Double): Double ={
    0
  }
  
  def costoGPS(): Double ={
    serviciosExtra.exists(_.soyGPS) match{
      case true => distanciaEntreSucursales()*2*0.5
      case _ => 0
    }
    
  }
  
  def costoVideo(): Double ={
    serviciosExtra.exists(_.soyVideo) match{
      case true => distanciaEntreSucursales()*2*3.74
      case _ => 0
    }
  }  

  def distanciaEntreSucursales(): Double ={
    0
  }
  
  def costoSustanciasPeligrosas(): Double ={
    if(enviosAsignados.exists(_.caracteristicas.exists(_.soyInfraestructuraSustancias)))
      600.0
    else
      0.0
  }
  
    def costoAnimales(): Double ={
    val dist = distanciaEntreSucursales 
    if(enviosAsignados.exists(_.caracteristicas.exists(_.soyInfraestructuraAnimales)))
      if (dist < 100)
       	50
      else if (dist < 200)
       	86
      else
       	137
    else
    	0
  }
    
  def reduccionInsumos(costoDeTransporte: Double): Double = {
    0
  }
  
  def regresarASucursal() ={
   sucursalOrigen.agregarTransporte(this) 
   sucursalDestino = null
  }
}


