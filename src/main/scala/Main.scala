import scala.compiletime.ops.any
import scala.compiletime.ops.string
import scala.collection.immutable.Range.Int

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

//tipos auxiliares
case class NumPos(num: String, pos: Int)
//wait item
case class ItemWait(var waiting:List[Int])

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
//funcao auxiliar/ remover lista passando valor
def remove_lista[A](lista:List[A], valor:A): List[A] = {
  lista match
    case head :: next => if((valor.toString()).equalsIgnoreCase(head.toString())){
                          next
                        }else{
                          head::remove_lista(next, valor)
                        }
    case Nil => Nil
}
//funcao auxiliar/ remover lista passando indice
def remove_listaPos[A](lista:List[A], posF:Int ,posI:Int = 0): List[A] = {
  if(posF > posI){
    lista match
      case head :: next => head::remove_listaPos(next, posF, posI + 1)
      case Nil => Nil
  }else{
    lista match
      case head :: next => next
      case Nil => Nil    
  }
}
//funcao auxiliar lista para linha separada por / e com uma quebra de linha no final
def compos_lista[A](lista:List[A], divisor:String = " / " ):String = {
  lista match
    case head :: next => head.toString() + divisor + compos_lista[A](next, divisor)
    case Nil => ""
}
//funcao auxiliar lista pega a posicao do valor passado ou valor negativo se nao tiver
def getListaPos[A](lista:List[A], valor:A):Int = {
  lista match
    case head :: next => if(head == valor){
      0
    }else{
      if(getListaPos(next, valor) == -1){-1}else{1 + getListaPos(next, valor)}
    }
    case Nil => -1
}
def getListaPosValue[A](lista:List[A], posF:Int ,posI:Int = 0):A = {
  if(posF > posI){
    lista match
      case head :: next => getListaPosValue(next, posF, posI + 1)
      case Nil => lista.last
  }else{
    lista match
      case head :: next => head
      case Nil => lista.last
  }
}
//muda a posicao especificada da lista se ela nao estiver vazia
def changeListaPos[A](lista:List[A], valor:A, posF:Int ,posI:Int = 0):List[A] = {
  if(posF > posI){
    lista match
      case head :: next => head::changeListaPos(next,valor, posF, posI + 1)
      case Nil => Nil
  }else{
    lista match
      case head :: next => valor::next
      case Nil => Nil    
  }
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

//wait item
class Wait_Item{
  var items:List[String] = Nil
  var waiting:List[ItemWait] = Nil
  //adiciona um item e um tr que o espera

  def add_wait(item:String, tr:Int) = {
    val k = getListaPos(this.items, item)
    if(k == -1){
      this.items = add_lista(this.items, item)
      this.waiting  = add_lista(this.waiting, ItemWait(tr::Nil))
    }else{
      var valor = getListaPosValue(this.waiting, k)
      valor.waiting match
        case h::n => valor.waiting = add_lista(valor.waiting, tr)
        case Nil  => 
      this.waiting = changeListaPos(this.waiting, valor, k)
    }
  }
  def remove_wait(item:String) = {
    val k = getListaPos(this.items, item)
    if(k == -1){
  
    }else{
      var valor = getListaPosValue(this.waiting, k)
      valor.waiting match
        case h::n => valor.waiting = remove_listaPos(valor.waiting, 0)
        case Nil  => 
      valor.waiting match
        case h::n => this.waiting = changeListaPos(this.waiting, valor, k)
        case Nil  => this.items = remove_listaPos(this.items, k)
                     this.waiting = remove_listaPos(this.waiting, k)
      
    }
  }

  def printEstadoAtual():String = {
     var s = ""
     def percorrerLista(item:List[String], waiting:List[ItemWait]):String = {
      item match
        case itemH :: itemN => waiting match
                    case waitingH :: waitingN =>  itemH + ", [ " + compos_lista(waitingH.waiting, ", ") + " ]\n" + percorrerLista(itemN, waitingN)
                    case Nil => ""
        case Nil => ""
     } 
     s = percorrerLista(this.items, this.waiting)
     s
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
  def add_Item(itemId:Int, trId:Int, estado:Estado) = {
    this.itemID = add_lista(this.itemID, itemId)
    this.TrIds  = add_lista(this.TrIds, trId)
    this.Status = add_lista(this.Status, estado)
  }
  //atualizar item
  def att_Item(trId:Int, newEstado:Estado) = {
    val k = getListaPos(this.TrIds, trId)
    if(k != -1){
      this.Status = changeListaPos(this.Status, newEstado, k)
    }
  }
  //print TrMananger
  def printEstadoA():String = {
    var s = ""
    s = s + "Tr Manager:\n"
    s = s +"itemID: " + compos_lista(this.itemID) + "\n"
    s = s +"TrId: " + compos_lista(this.TrIds) + "\n"
    s = s +"Status: " + compos_lista(this.Status) + "\n"
    s
  }
}

class LockTable(nv_isol:NivelIsol) extends locksMan {
  //isolamento
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
  //estado do bloqueio
  var ativo: List[Boolean] = Nil
  //retorna o estado da lista de bloqueio como string
  def printEstadoA():String = {
    var s = ""
    s = s + "LOCKTABLE:\n"
    s = s +"IdItem: " + compos_lista(this.idItems) + "\n"
    s = s +"TrId: " + compos_lista(this.TrIds) + "\n"
    s = s +"Escopo: " + compos_lista(this.escopos) + "\n"
    s = s +"Duracao: " + compos_lista(this.duracoes) + "\n"
    s = s +"Tipo: " + compos_lista(this.tipos) + "\n"
    s = s +"Ativo: " + compos_lista(this.ativo) + "\n"
    s
  }
  //tenta adicionar bloqueios de leitura na tabela
  def RL(Tr: Int, D: String): Boolean = {
    //verifica se tem isolamento incompativel na tabela tipos e retorna true se tiver
    def verificaIncomp(idItem:List[String],trId:List[Int], tipo:List[TipoBlock]): Boolean = {
      idItem match
        case idItemH :: idItemN => trId match
          case trIdH :: trIdN => tipo match
             case tipoH :: tipoN => if(idItemH.equalsIgnoreCase(D) && tipoH == TipoBlock.LEITURA && trIdH == Tr){
                                      return true
                                    }else if(idItemH.equalsIgnoreCase(D) && tipoH == TipoBlock.ESCRITA && trIdH != Tr){
                                      return true
                                    }else{
                                      verificaIncomp(idItemN, trIdN, tipoN)
                                    }
             case Nil => false
          case Nil => false
        case Nil => false
      
    }
    val validar = verificaIncomp(this.idItems, this.TrIds, this.tipos)
    if(validar == true){ //se tiver -> retorna false (nao consegiu inserir bloqueio)
      false 
    }else{ //nao tiver -> adiciona o bloqueio
    //adicionar bloqueio
      //verifica o nivel de isolamento
      this.isolamento match
        case NivelIsol.READ_UNCOMMIT => true //nao adiciona bloqueio

        case NivelIsol.READ_COMMIT =>  this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.CURTA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.LEITURA)
                                       this.ativo    = add_lista(this.ativo, true)
                                       //adiciona bloqueio com duracao curta 

        case NivelIsol.READ_REPEAT =>  this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.LEITURA)
                                       this.ativo    = add_lista(this.ativo, true)
                                       //longa duracao/objeto

        case NivelIsol.SERIALIZE =>    this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.LEITURA)
                                       this.ativo    = add_lista(this.ativo, true)
                                       //longa duracao
      //retorna true
      true
    }
  }
  def WL(Tr: Int, D: String): Boolean = {
     //verifica se tem isolamento incompativel na tabela tipos e retorna true se tiver
    def verificaIncomp(idItem:List[String],trId:List[Int], tipo:List[TipoBlock], active:List[Boolean]): Boolean = {
      active match
        case activeH :: activeN => idItem match
          case idItemH :: idItemN => trId match
            case trIdH :: trIdN => tipo match
              case tipoH :: tipoN => if(idItemH.equalsIgnoreCase(D) && tipoH == TipoBlock.LEITURA && trIdH != Tr && activeH == true){
                                        return true
                                      }else if(idItemH.equalsIgnoreCase(D) && tipoH == TipoBlock.ESCRITA && trIdH != Tr && activeH == true){
                                        return true
                                      }else if(idItemH.equalsIgnoreCase(D) && tipoH == TipoBlock.ESCRITA && trIdH == Tr && activeH == false){
                                        return true
                                      }else{
                                        verificaIncomp(idItemN, trIdN, tipoN, activeN)
                                      }
              case Nil => false
            case Nil => false
          case Nil => false
        case Nil => false
    }
    val validar = verificaIncomp(this.idItems, this.TrIds, this.tipos, this.ativo)
    if(validar == true){ //se tiver -> retorna false (nao consegiu inserir bloqueio)
      false 
    }else{ //nao tiver -> adiciona o bloqueio
    //adicionar bloqueio
      //verifica o nivel de isolamento
        this.isolamento match
        case NivelIsol.READ_UNCOMMIT =>this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.CURTA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.ESCRITA)
                                       this.ativo    = add_lista(this.ativo, true)
                                       //adiciona bloqueio com duracao curta 

        case NivelIsol.READ_COMMIT =>  this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.ESCRITA)
                                       this.ativo    = add_lista(this.ativo, true)
                                       //adiciona bloqueio com duracao curta 

        case NivelIsol.READ_REPEAT =>  this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.ESCRITA)
                                       this.ativo    = add_lista(this.ativo, true)
                                       //longa duracao/objeto

        case NivelIsol.SERIALIZE =>    this.idItems = add_lista[String](this.idItems, D)
                                       this.TrIds = add_lista[Int](this.TrIds, Tr)
                                       this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
                                       this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
                                       this.tipos    = add_lista[TipoBlock](this.tipos , TipoBlock.ESCRITA)
                                       this.ativo    = add_lista(this.ativo, true)
                                       //longa duracao
      //retorna true
        true
    }
  }
  def UL(Tr: Int, D: String): Unit = {
      //acha o indice
      def findIndice(trId:List[Int], idItem:List[String]):Int = {
        trId  match
          case trIdH :: trIdN => idItem match
            case idItemH :: idItemN =>if(trIdH == Tr && idItemH.equalsIgnoreCase(D)){
                0
            }else{
              if(findIndice(trIdN, idItemN) == -1){-1}else{1 + findIndice(trIdN, idItemN)}
            }
            case Nil => -1
          case Nil => -1
      }
      var k = findIndice(this.TrIds, this.idItems)
      if(k != -1){
        this.ativo = changeListaPos(this.ativo, false, k)
      }
  }
}

//salva o estado de um momento especifico do escalonador
class Momento(operacaoAtual:String ,grafo:Wait_For, tabelaBloq:LockTable, mananger:TrManager, waitItem:Wait_Item){
  //operacao atual
  val opAtual = operacaoAtual
  //grafo
  val grafoStr = grafo.draw_grafo() 
  //lockTable
  val lockStr  = tabelaBloq.printEstadoA()
  //trMananger
  val trmStr   = mananger.printEstadoA()
  //lista de espera
  val waitStr  = waitItem.printEstadoAtual()

  def printMomento() ={
    println("Operacao Atual: " + opAtual)
    println("-")
    println(grafoStr)
    println("-")
    println(lockStr)
    println("-")
    println(trmStr)
    println("-")
    println(waitStr)
    println("-")
  }
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

class Escalonador(scheduler:list[String]){
  var lastadd = 1
  //grafo
  var grafo_W = 0
  //lista de espera
  var lista_espera = 0
  //locktable
  var lockTable = 0
  //trMananger
  var trMananger = 0
  ////////////////
  //list[string] scheduler
  var schedulerSerial = ""
  //list[momentos]
  var momentos = 0
  /////////////////
  def mostrarMomento(momentos:List[Momento], indiceF:Int, indiceI:Int = 0) = {}
  //processar(Item:string)
  def processarItem(Item:string) = {}
  //escalonarScheduler(lista[string])
  def escalonarScheduler(scheduler:list[String]) = {}
}

object Main extends App {
  
  var t = new Wait_Item
  t.add_wait("x", 1)
  t.add_wait("x", 3)
  t.add_wait("x", 5)
  t.add_wait("y", 1)
  t.add_wait("z", 3)
  t.add_wait("x", 15)
  println(t.printEstadoAtual())
  t.remove_wait("y")
  println(t.printEstadoAtual())
  //var teste = "BT(1)r1(x)BT(2)w2(x)r2(y)r1(y)C(1)r2(z)C(2)"
  //var ttt = readString(teste)
  //mostraLista(ttt)
  
  //val lock:LockTable = new LockTable(NivelIsol.READ_COMMIT)
  //lock.RL(1, "x")
  //lock.WL(1, "y")
  //var k = lock.printEstadoA()
  //println(k)
  //lock.UL(1, "x")
  //lock.RL(1, "x")
  //println(k.toString())
  //k = lock.printEstadoA()
  //println(k)
  //println(j)
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
