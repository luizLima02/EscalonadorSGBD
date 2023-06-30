import scala.compiletime.ops.any
import scala.compiletime.ops.string
//tipos auxiliares
case class NumPos(num: String, pos: Int)

//tipos fixos
enum Estado:
  case ATIVA, CONCLUÍDA, ABORTADA, ESPERANDO

enum Escopo:
  case OBJETO, PREDICADO

enum Duracao:
  case CURTA, LONGA

enum TipoBlock:
  case LEITURA, ESCRITA

enum NivelIsol:
  case READ_UNCOMMIT, READ_COMMIT, READ_REPEAT, SERIALIZE

//traits
trait locksMan {
  // Insere um bloqueio de Leitura na LockTable sobre o item D para a transação Tr, se for possível.
  def RL(Tr: Int, D: String): Boolean

  // Insere um bloqueio de Escrita na LockTable sobre o item D para a transação Tr, se for possível.
  def WL(Tr: Int, D: String): Boolean

  // Apaga o bloqueio da transação Tr sobre o item D na LockTable.
  def UL(Tr: Int, D: String): Unit
}
//funcao auxiliar/ adicionar lista
def add_lista[A](lista:List[A], valor:A): List[A] = {
  lista match
    case head :: next => head::(add_lista[A](next, valor))
    case Nil => valor::Nil
}
//funcao auxiliar lista para linha separada por / e com uma quebra de linha no final
def compos_lista[A](lista:List[A]):String = {
  lista match
    case head :: next => head.toString() + "/ " + compos_lista[A](next)
    case Nil => "\n"
}
//grafo wait-for
class Wait_For(size: Int) {
  // variaveis
  var tamanho: Int = size
  var data: Array[Array[Int]] = Array.ofDim[Int](size, size)
  // funcoes
  // inicializa o grafo
  def init() = {
    for (i <- 0 until size) do {
      for (j <- 0 until size) do {
        data(i)(j) = 0
      }
    }
  }

  def add_Aresta(tr1: Int, tr2: Int) = {
      this.data(tr1-1)(tr2-1) = 1
  }

  def remove_Aresta(tr1: Int, tr2: Int) = {
      this.data(tr1-1)(tr2-1) = 0
  }

  def draw_grafo(): String = {
    var graf: String = ""
    for (i <- 0 until size) do {
      graf = graf + "T" + (i+1).toString() + ": "
      for (j <- 0 until size) do {
        if (data(i)(j) == 1) {
          graf = graf + (j+1).toString() + " "
        }
      }
      graf = graf + "\n"
    }
    graf
  }
}

//trManager
class TrManager {
//ids
  var itemID: List[Int] = Nil
//timestamps
  var TrIds: List[Int] = Nil
//status
  var Status: List[Estado] = Nil

  //add item
  //print TrMananger
  def printEstadoA():String = {
    var s = ""
    s = s + "Tr Manager:\n"
    s = s +"itemID: " + compos_lista(this.itemID)
    s = s +"TrId: " + compos_lista(this.TrIds)
    s = s +"Status: " + compos_lista(this.Status)
    s
  }
}

class LockTable(nv_isol:NivelIsol) extends locksMan {
  val isolamento:NivelIsol = nv_isol
  // idItem
  var idItems: List[String] = Nil
  // TrId
  var TrIds: List[Int] = Nil
  // escopo
  var escopos: List[Escopo] = Nil
  // duracao
  var duracoes: List[Duracao] = Nil
  //tipo
  var tipos: List[TipoBlock] = Nil

  def printEstadoA():String = {
    var s = ""
    s = s + "LOCKTABLE:\n"
    s = s +"IdItem: " + compos_lista(this.idItems)
    s = s +"TrId: " + compos_lista(this.TrIds)
    s = s +"Escopo: " + compos_lista(this.escopos)
    s = s +"Duracao: " + compos_lista(this.duracoes)
    s = s +"Tipo: " + compos_lista(this.tipos)
    s
  }
  //tenta adicionar bloqueios de leitura na tabela
  def RL(Tr: Int, D: String): Boolean = {
    //verifica se tem isolamento incompativel na tabela tipos e retorna true se tiver
    def verificaIncomp(idItem:List[String], tipo:List[TipoBlock]): Boolean = {
      idItem match
        case head :: next => tipo match
                              case cab :: prox => if(head.equalsIgnoreCase(D) && cab == TipoBlock.ESCRITA){
                                return true
                              }else{
                                verificaIncomp(next, prox)
                              }
                              case Nil => false
        case Nil => false
      
    }
    val validar = verificaIncomp(this.idItems, this.tipos)
    
    if(validar == true){ //se tiver -> retorna false (nao consegiu inserir bloqueio)
      false 
    }else{ //nao tiver -> adiciona o bloqueio
    //adicionar bloqueio
      //verifica o nivel de isolamento
      this.isolamento match
        case NivelIsol.READ_UNCOMMIT => true//nao adiciona bloqueio

        case NivelIsol.READ_COMMIT =>  this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.CURTA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.LEITURA)
                                       //adiciona bloqueio com duracao curta 

        case NivelIsol.READ_REPEAT =>  this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.LEITURA)
                                       //longa duracao/objeto

        case NivelIsol.SERIALIZE =>    this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.LEITURA)
                                       //longa duracao
      //retorna true
      true
    }
  }
  def WL(Tr: Int, D: String): Boolean = {true}

  def UL(Tr: Int, D: String): Unit = {}
}

//salva o estado de um momento especifico do escalonador
class Momento(grafo:String, tabelaBloq:String, mananger:String){
  //grafo
  //lockTable
  //trMananger
  //lista de espera
}

//funcoes auxliares
def sizeLista(lista:List[Any]):Int = {
  lista match
    case head :: next => 1 + (sizeLista(next)) 
    case Nil => 0
}

//funcoes de formatacao de scheduler
def montarBT(s: String): NumPos = {
  var i = 0
  var f = 1
  var m = s.substring(i, f)
  // acha a primeira chave (
  if (m.equalsIgnoreCase("(")) {
    i = i + 1
    f = f + 1
    // acha a ultima chave )
    var k = s.substring(i, f)
    var ite = s.substring(i + 1, f + 1)
    while (ite.equalsIgnoreCase(")") == false && f < s.length()) {
      k = k + ite
      f = f + 1
      i = i + 1
      ite = s.substring(i + 1, f + 1)
    }
    NumPos("BT-" + k, f + 3)
  } else {
    NumPos(" ", -1)
  }
}

def montarRW(s: String, mode: String): NumPos = {
  var i = 1
  var f = 2
  // achar o fim do numero
  while (s.substring(f, f + 1).equalsIgnoreCase("(") == false) {
    f = f + 1
  }
  var number = s.substring(i, f)
  i = f
  f = f + 1
  var m = s.substring(i, f)
  // acha a primeira chave (
  if (m.equalsIgnoreCase("(")) {
    i = i + 1
    f = f + 1
    // acha a ultima chave )
    var k = s.substring(i, f)
    var ite = s.substring(i + 1, f + 1)
    while (ite.equalsIgnoreCase(")") == false && f < s.length()) {
      k = k + ite
      f = f + 1
      i = i + 1
      ite = s.substring(i + 1, f + 1)
    }
    NumPos(mode + "-" + number + "-" + k, f + 1)
  } else {
    NumPos(" ", -1)
  }
}

def montarC(s: String): NumPos = {
  // C(xy)
  var i = 1
  var f = 2
  var m = s.substring(i, f)
  // acha a primeira chave (
  if (m.equalsIgnoreCase("(")) {
    i = i + 1
    f = f + 1
    // acha a ultima chave )
    var k = s.substring(i, f)
    var ite = s.substring(i + 1, f + 1)
    while (ite.equalsIgnoreCase(")") == false && f < s.length()) {
      k = k + ite
      f = f + 1
      i = i + 1
      ite = s.substring(i + 1, f + 1)
    }
    NumPos("C-" + k, f + 1)
  } else {
    NumPos(" ", -1)
  }
}

def readString(st: String): List[String] = {
  if (st.length() > 0) {
    // inicio
    val m = st.substring(0, 2)
    if (m.equalsIgnoreCase("BT")) {
      // inicio
      val p = montarBT(st.substring(2, st.length()))
      if (p.pos > 0) {
        val stRet = p.num :: readString(st.substring(p.pos))
        stRet
      } else {
        Nil
      }
      // fim
    } else if (
      (st
        .substring(0, 1))
        .equalsIgnoreCase("r") || (st.substring(0, 1)).equalsIgnoreCase("w")
    ) {
      // inicio
      val p = montarRW(st, st.substring(0, 1))
      if (p.pos > 0) {
        val stRet = p.num :: readString(st.substring(p.pos))
        stRet
      } else {
        Nil
      }
      // fim
    } else if ((st.substring(0, 1)).equalsIgnoreCase("C")) {
      // inicio
      val p = montarC(st)
      if (p.pos > 0) {
        val stRet = p.num :: readString(st.substring(p.pos))
        stRet
      } else {
        Nil
      }
      // fim
    } else {
      Nil
    }
    // fim
  } else {
    Nil
  }
}

def mostraLista(l: List[String]): Unit = {
  l match
    case h :: t =>
      println(h)
      mostraLista(t)
    case Nil => println("")

}

object Main extends App {
  
  var teste = "BT(1)r1(x)BT(2)w2(x)r2(y)r1(y)C(1)r2(z)C(2)"
  var ttt = readString(teste)
  mostraLista(ttt)
  
  val lock:LockTable = new LockTable(NivelIsol.READ_COMMIT)
  lock.RL(1, "x")
  lock.RL(1, "y")
  lock.RL(2, "x")
  //println(k.toString())
  val k = lock.printEstadoA()
  println(k)
  /*
  val k = sizeLista(ttt)
  print(k.toString()+"\n")
  
  var grafo:Wait_For = new Wait_For(5)
  grafo.init()
  grafo.add_Aresta(1,2)
  grafo.add_Aresta(2,3)
  grafo.add_Aresta(1,3)
  println(grafo.draw_grafo())
  */
}
/*
BT(101)r1(x)BT(2)w2(x)r2(y)r11(y)C(1)r2(z)C(22)
 */
