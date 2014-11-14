package tadp.tp.argentinaexpresshibrido

class Sucursal (var transporte : Set[Transporte], val volumenTotal : Int, val pais : String) extends CalculadorDistancia{

  var envios : Set[Envio] = Set()
  var volumen:Int = 0
   
  
  def volumenDisponible():Int={
    (this.volumenTotal) - (this.volumenEnviosEnSucursal);
    }
  
  def agregarTransporte(tran : Transporte) ={
    //this.transporte = this.transporte ++ Set(tran)
    this.transporte += tran
  }
  
  def quitarTransporte(tran : Transporte) ={
    this.transporte -= tran
  }
  
  def volumenEnviosEnSucursal() : Int ={
	(this.transporte.map((t:Transporte) => t.volumenEnvios).sum) + (this.envios.map((e:Envio) => e.volumen).sum)
  }

  def volumenEnviosASucursal(destino : Sucursal) : Int ={
	this.transporte.filter((t: Transporte)=> t.sucursalDestino == destino).map((t:Transporte) => t.volumenEnvios).sum
  } 
  
  def asignarEnvioATransporte(envio: Envio): Option[Transporte] = {
    var transporteAsignado : Option[Transporte] = None;

    transporteAsignado = transporte.find((t: Transporte) => t.puedeCargar(envio))
    
    if (!transporteAsignado.isEmpty && entraPedido(envio)) 
    	transporteAsignado.foreach(_.agregarEnvio(envio))
    
    transporteAsignado
  }
  
  def entraPedido(envio:Envio): Boolean = { 
    envio.sucursalDestino.volumenDisponible >= (envio.volumen + this.volumenEnviosASucursal(envio.sucursalDestino))
  }
  
  def esCasaCentral(): Boolean = {
    false
  }
  
  
  //2. Indicar los eventos relacionados a la salida y la llegada de los transportes de una sucursal a otra, es decir:
  //a) El transporte sale hacia la sucursal destino.
  //b) El transporte llega a la sucursal destino. Se considera que inmediatamente emprende el regreso.
  //c) El transporte llega nuevamente a la sucursal origen.
  //Se considera que los transportes solo llevan envios en su camino de ida, es decir, vuelven vacios.
  
  def mandarTransporte(tran : Transporte) = {
	  //El transporte esta en la sucursal y tiene pedidos para enviar?
	  if (transporte.contains(tran) && (tran.sucursalDestino != null) && (tran.sucursalOrigen != null) && (!tran.enviosAsignados.isEmpty)) {
	    quitarTransporte(tran)
	    tran.sucursalDestino.recibirEnvio(Some(tran))
	  }

//    if (!transporteAsignado.isEmpty){
//      this.transporte -- transporteAsignado
//      envio.sucursalDestino.recibirEnvio(envio,transporteAsignado)
 //   }
  }
  
  def recibirEnvio(tran: Option[Transporte]) = {
    tran.foreach({t: Transporte =>
      t.enviosAsignados.foreach(this.envios += _ ) // Pasamos los pedidos a la sucursal
      t.enviosAsignados = Set() // Vaciamos los pedidos del transporte
      t.regresarASucursal // Retorna a la sucursal de origen
    })
      
  }

  
  def regresoTransporte(transporteAsignado: Option[Transporte]) = {
    this.transporte ++ transporteAsignado
  }
  
  //3. Un paquete se retira de la sucursal destino
  
  def retirarEnvio(envio: Envio) ={
    this.envios -- Set(envio)
  }
  
}