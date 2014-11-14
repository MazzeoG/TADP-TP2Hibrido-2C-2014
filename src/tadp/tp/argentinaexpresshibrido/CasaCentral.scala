package tadp.tp.argentinaexpresshibrido

import scala.collection.immutable.Set

class CasaCentral(transporte : Set[Transporte],override val volumenTotal : Int, override val pais: String) 
extends Sucursal(transporte,volumenTotal,pais) {
  
  override def esCasaCentral(): Boolean = {
    true
  }

}