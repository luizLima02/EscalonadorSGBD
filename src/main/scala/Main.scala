import scala.compiletime.ops.any
import scala.io.StdIn.readLine
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
case class ItemWait(var waiting: List[Int])

//funcao auxiliar/ adicionar lista
def add_lista[A](lista: List[A], valor: A): List[A] = {
  lista match
    case head :: next => head :: (add_lista[A](next, valor))
    case Nil          => valor :: Nil
}
def contem[A](lista: List[A], valor: A): Boolean = {
  lista match
    case head :: next =>
      if (head.toString().equalsIgnoreCase(valor.toString()) == false) {
        (contem[A](next, valor))
      } else { true }
    case Nil => false
}
//funcao auxiliar/ remover lista passando valor
def remove_lista[A](lista: List[A], valor: A): List[A] = {
  lista match
    case head :: next =>
      if ((valor.toString()).equalsIgnoreCase(head.toString())) {
        next
      } else {
        head :: remove_lista(next, valor)
      }
    case Nil => Nil
}
//funcao auxiliar/ remover lista passando indice
def remove_listaPos[A](lista: List[A], posF: Int, posI: Int = 0): List[A] = {
  if (posF > posI) {
    lista match
      case head :: next => head :: remove_listaPos(next, posF, posI + 1)
      case Nil          => Nil
  } else {
    lista match
      case head :: next => next
      case Nil          => Nil
  }
}
//funcao auxiliar lista para linha separada por / e com uma quebra de linha no final
def compos_lista[A](lista: List[A], divisor: String = " / "): String = {
  lista match
    case head :: next =>
      head.toString() + divisor + compos_lista[A](next, divisor)
    case Nil => ""
}
//funcao auxiliar lista pega a posicao do valor passado ou valor negativo se nao tiver
def getListaPos[A](lista: List[A], valor: A): Int = {
  lista match
    case head :: next =>
      if (head == valor) {
        0
      } else {
        if (getListaPos(next, valor) == -1) { -1 }
        else { 1 + getListaPos(next, valor) }
      }
    case Nil => -1
}
def getListaPosValue[A](lista: List[A], posF: Int, posI: Int = 0): A = {
  if (posF > posI) {
    lista match
      case head :: next => getListaPosValue(next, posF, posI + 1)
      case Nil          => lista.last
  } else {
    lista match
      case head :: next => head
      case Nil          => lista.last
  }
}
//muda a posicao especificada da lista se ela nao estiver vazia
def changeListaPos[A](
    lista: List[A],
    valor: A,
    posF: Int,
    posI: Int = 0
): List[A] = {
  if (posF > posI) {
    lista match
      case head :: next => head :: changeListaPos(next, valor, posF, posI + 1)
      case Nil          => Nil
  } else {
    lista match
      case head :: next => valor :: next
      case Nil          => Nil
  }
}


//funcoes auxliares
def sizeLista(lista: List[Any]): Int = {
  lista match
    case head :: next => 1 + (sizeLista(next))
    case Nil          => 0
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

def achaNum(Item: String): Int = {
  val k = Item.split("-")
  val r = k(1).toInt
  r
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

def transacoesQnt(scheduler: List[String]): Int = {
  scheduler match
    case head :: next =>
      if (head.substring(0, 1).equalsIgnoreCase("b")) {
        1 + transacoesQnt(next)
      } else { transacoesQnt(next) }
    case Nil => 0
}

def listaToArray(lista: List[String]): Array[String] = {
  var data: Array[String] = Array.ofDim[String](lista.length)
  for (i <- 0 until lista.length) do {
    data(i) = lista(i)
  }
  data
}

def re_ordenar(lista: Array[String], i: Int, f: Int) = {
  val valor = lista(f)
  for (k <- f to (i + 1) by -1) {
    lista(k) = lista(k - 1)
  }
  lista(i) = valor
}


//traits
trait locksMan {
  // Insere um bloqueio de Leitura na LockTable sobre o item D para a transação Tr, se for possível.
  def RL(Tr: Int, D: String): Int

  // Insere um bloqueio de Escrita na LockTable sobre o item D para a transação Tr, se for possível.
  def WL(Tr: Int, D: String): Int

  // Apaga o bloqueio da transação Tr sobre o item D na LockTable.
  def UL(Tr: Int, D: String): Unit
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

  def temCiclo(matriz: Array[Array[Int]]): Boolean = {
    val numVertices = matriz.length
    val visitado = Array.fill(numVertices)(false)
    val pilha = Array.fill(numVertices)(false)
  
    def dfs(vertice: Int): Boolean = {
      visitado(vertice) = true
      pilha(vertice) = true
  
      for (vizinho <- 0 until numVertices) {
        if (matriz(vertice)(vizinho) == 1) {
          if (!visitado(vizinho)) {
            if (dfs(vizinho)){true}
          } else if (pilha(vizinho)) {
            true
          }
        }
      }
  
      pilha(vertice) = false
      false
    }
  
    for (vertice <- 0 until numVertices) {
      if (!visitado(vertice) && dfs(vertice)) {
        true
      }
    }
  
    false
  }

  def add_Aresta(tr1: Int, tr2: Int) = {
    this.data(tr1 - 1)(tr2 - 1) = 1
  }

  def remove_Aresta(tr1: Int, tr2: Int) = {
    this.data(tr1 - 1)(tr2 - 1) = 0
  }

  def delete_Ltr(tr1: Int) = {
    if((tr1-1) >= 0){
      for (i <- 0 until size) do {
          this.remove_Aresta(tr1, i+1)
     }
    }
  }

  def delete_tr(tr1: Int) = {
    for (i <- 0 until size) do {
      for (j <- 0 until size) do {
        if (i == tr1 || j == tr1) {
          data(i)(j) = 0
        }
      }
    }
  }



  def draw_grafo(): String = {
    var graf: String = "Grafo:\n"
    for (i <- 0 until size) do {
      graf = graf + "T" + (i + 1).toString() + ": "
      for (j <- 0 until size) do {
        if (data(i)(j) == 1) {
          graf = graf + "T" + (j + 1).toString() + " "
        }
      }
      graf = graf + "\n"
    }
    graf
  }
}
//wait item
class Wait_Item {
  var items: List[String] = Nil
  var waiting: List[ItemWait] = Nil
  // adiciona um item e um tr que o espera

def add_wait(item: String, tr: Int) = {
    val k = getListaPos(this.items, item)
    if (k == -1) {
      this.items = add_lista(this.items, item)
      this.waiting = add_lista(this.waiting, ItemWait(tr :: Nil))
    } else {
      var valor = getListaPosValue(this.waiting, k)
      if (contem(valor.waiting, tr) == false) {
        valor.waiting match
          case h :: n => valor.waiting = add_lista(valor.waiting, tr)
          case Nil    =>
        this.waiting = changeListaPos(this.waiting, valor, k)
      }
    }
  }
def remove_wait(item: String): Int = {
    val k = getListaPos(this.items, item)
    if (k == -1) {
      -1
    } else {
      var valor = getListaPosValue(this.waiting, k)
      var remov = valor.waiting(0)
      valor.waiting match
        case h :: n => valor.waiting = remove_listaPos(valor.waiting, 0)
        case Nil    =>
      valor.waiting match
        case h :: n => this.waiting = changeListaPos(this.waiting, valor, k)
        case Nil =>
          this.items = remove_listaPos(this.items, k)
          this.waiting = remove_listaPos(this.waiting, k)
      remov
    }
  }

def remove_waiter(item: String, tr: Int) = {
    val k = getListaPos(this.items, item)
    if (k == -1) {} else {
      var valor = getListaPosValue(this.waiting, k)
      valor.waiting match
        case h :: n =>
          val posicao = getListaPos(valor.waiting, tr)
          if (posicao == -1) {} else {
            valor.waiting = remove_listaPos(valor.waiting, posicao)
          } // valor.waiting = remove_listaPos(valor.waiting, 0)
        case Nil =>
      valor.waiting match
        case h :: n => this.waiting = changeListaPos(this.waiting, valor, k)
        case Nil =>
          this.items = remove_listaPos(this.items, k)
          this.waiting = remove_listaPos(this.waiting, k)
    }
  }
def printEstadoAtual(): String = {
    var s = "Wait Item:\n"
    def percorrerLista(item: List[String], waiting: List[ItemWait]): String = {
      item match
        case itemH :: itemN =>
          waiting match
            case waitingH :: waitingN =>
              itemH + ", [ " + compos_lista(
                waitingH.waiting,
                ", "
              ) + " ]\n" + percorrerLista(itemN, waitingN)
            case Nil => ""
        case Nil => ""
    }
    s = s + percorrerLista(this.items, this.waiting)
    s
  }
}
//trManager
class TrManager {
//ids BT(22)BT(1)r22(x)
  var itemID: List[Int] = Nil
//timestamps
  var TrIds: List[Int] = Nil
//status
  var Status: List[Estado] = Nil

  // add item
def add_Item(itemId: Int, trId: Int, estado: Estado) = {
    this.itemID = add_lista(this.itemID, itemId)
    this.TrIds = add_lista(this.TrIds, trId)
    this.Status = add_lista(this.Status, estado)
  }
  // atualizar item
def att_Item(trId: Int, newEstado: Estado) = {
    val k = getListaPos(this.TrIds, trId)
    if (k != -1) {
      this.Status = changeListaPos(this.Status, newEstado, k)
    }
  }

def ativar() = {
    for (k <- 0 until this.Status.length) {
      if (this.Status(k) == Estado.ABORTADA) {
        this.Status = changeListaPos(this.Status, Estado.ATIVA, k)
      }
    }
  }
  // print TrMananger
def printEstadoA(): String = {
    var s = ""
    s = s + "Tr Manager:\n"
    s = s + "itemID: " + compos_lista(this.itemID) + "\n"
    s = s + "TrId: " + compos_lista(this.TrIds) + "\n"
    s = s + "Status: " + compos_lista(this.Status) + "\n"
    s
  }
}

class LockTable(nv_isol: NivelIsol) extends locksMan {
  // isolamento
  val isolamento: NivelIsol = nv_isol
  // idItem
  var idItems: List[String] = Nil
  // TrId
  var TrIds: List[Int] = Nil
  // escopo
  var escopos: List[Escopo] = Nil
  // duracao
  var duracoes: List[Duracao] = Nil
  // tipo
  var tipos: List[TipoBlock] = Nil
  // estado do bloqueio
  var ativo: List[Boolean] = Nil
  // retorna o estado da lista de bloqueio como string
  def printEstadoA(): String = {
    var s = ""
    s = s + "LOCKTABLE:\n"
    s = s + "IdItem: " + compos_lista(this.idItems) + "\n"
    s = s + "TrId: " + compos_lista(this.TrIds) + "\n"
    s = s + "Escopo: " + compos_lista(this.escopos) + "\n"
    s = s + "Duracao: " + compos_lista(this.duracoes) + "\n"
    s = s + "Tipo: " + compos_lista(this.tipos) + "\n"
    s = s + "Ativo: " + compos_lista(this.ativo) + "\n"
    s
  }
  // tenta adicionar bloqueios de leitura na tabela
  def RL(Tr: Int, D: String): Int = {
    // verifica se tem isolamento incompativel na tabela tipos e retorna true se tiver
    // println(Tr)
    def verificaIncomp(
        idItem: List[String],
        trId: List[Int],
        tipo: List[TipoBlock],
        active: List[Boolean]
    ): Int = {
      active match
        case activeH :: activeN =>
          idItem match
            case idItemH :: idItemN =>
              trId match
                case trIdH :: trIdN =>
                  tipo match
                    case tipoH :: tipoN =>
                      if (
                        idItemH.equalsIgnoreCase(
                          D
                        ) && tipoH == TipoBlock.ESCRITA && trIdH != Tr && activeH == true
                      ) {
                        -trIdH
                      } else if (
                        idItemH.equalsIgnoreCase(
                          D
                        ) && tipoH == TipoBlock.LEITURA && trIdH == Tr && activeH == false
                      ) {
                        5
                      } else {
                        verificaIncomp(idItemN, trIdN, tipoN, activeN)
                      }
                    case Nil => 0
                case Nil => 0
            case Nil => 0
        case Nil => 0
    }
    val validar =
      verificaIncomp(this.idItems, this.TrIds, this.tipos, this.ativo)
    // println(validar)
    if (validar == 5) {
      1
    } else if (validar == 0) { // nao tiver -> adiciona o bloqueio
      // adicionar bloqueio
      // verifica o nivel de isolamento
      this.isolamento match
        case NivelIsol.READ_UNCOMMIT => // nao adiciona bloqueio
        case NivelIsol.READ_COMMIT =>
          this.idItems = add_lista[String](this.idItems, D)
          this.TrIds = add_lista[Int](this.TrIds, Tr)
          this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
          this.duracoes = add_lista[Duracao](this.duracoes, Duracao.CURTA)
          this.tipos = add_lista[TipoBlock](this.tipos, TipoBlock.LEITURA)
          this.ativo = add_lista(this.ativo, true)
        // adiciona bloqueio com duracao curta

        case NivelIsol.READ_REPEAT =>
          this.idItems = add_lista[String](this.idItems, D)
          this.TrIds = add_lista[Int](this.TrIds, Tr)
          this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
          this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
          this.tipos = add_lista[TipoBlock](this.tipos, TipoBlock.LEITURA)
          this.ativo = add_lista(this.ativo, true)
        // longa duracao/objeto

        case NivelIsol.SERIALIZE =>
          this.idItems = add_lista[String](this.idItems, D)
          this.TrIds = add_lista[Int](this.TrIds, Tr)
          this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
          this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
          this.tipos = add_lista[TipoBlock](this.tipos, TipoBlock.LEITURA)
          this.ativo = add_lista(this.ativo, true)
        // longa duracao
      // retorna true
      1
    } else {
      validar
    }
  }
  def WL(Tr: Int, D: String): Int = {
    // verifica se tem isolamento incompativel na tabela tipos e retorna true se tiver
    def verificaIncomp(
        idItem: List[String],
        trId: List[Int],
        tipo: List[TipoBlock],
        active: List[Boolean]
    ): Int = {
      active match
        case activeH :: activeN =>
          idItem match
            case idItemH :: idItemN =>
              trId match
                case trIdH :: trIdN =>
                  tipo match
                    case tipoH :: tipoN =>
                      if (
                        idItemH.equalsIgnoreCase(
                          D
                        ) && tipoH == TipoBlock.LEITURA && trIdH != Tr && activeH == true
                      ) {
                        -trIdH
                      } else if (
                        idItemH.equalsIgnoreCase(
                          D
                        ) && tipoH == TipoBlock.ESCRITA && trIdH != Tr && activeH == true
                      ) {
                        -trIdH
                      } else if (
                        idItemH.equalsIgnoreCase(
                          D
                        ) && tipoH == TipoBlock.ESCRITA && trIdH == Tr && activeH == false
                      ) {
                        5
                      } else {
                        verificaIncomp(idItemN, trIdN, tipoN, activeN)
                      }
                    case Nil => 0
                case Nil => 0
            case Nil => 0
        case Nil => 0
    }
    val validar =
      verificaIncomp(this.idItems, this.TrIds, this.tipos, this.ativo)
    // println(validar)
    if (validar == 5) {
      1
    } else if (validar == 0) { // nao tiver -> adiciona o bloqueio
      // adicionar bloqueio
      // verifica o nivel de isolamento
      this.isolamento match
        case NivelIsol.READ_UNCOMMIT =>
          this.idItems = add_lista[String](this.idItems, D)
          this.TrIds = add_lista[Int](this.TrIds, Tr)
          this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
          this.duracoes = add_lista[Duracao](this.duracoes, Duracao.CURTA)
          this.tipos = add_lista[TipoBlock](this.tipos, TipoBlock.ESCRITA)
          this.ativo = add_lista(this.ativo, true)
        // adiciona bloqueio com duracao curta

        case NivelIsol.READ_COMMIT =>
          this.idItems = add_lista[String](this.idItems, D)
          this.TrIds = add_lista[Int](this.TrIds, Tr)
          this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
          this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
          this.tipos = add_lista[TipoBlock](this.tipos, TipoBlock.ESCRITA)
          this.ativo = add_lista(this.ativo, true)
        // adiciona bloqueio com duracao curta

        case NivelIsol.READ_REPEAT =>
          this.idItems = add_lista[String](this.idItems, D)
          this.TrIds = add_lista[Int](this.TrIds, Tr)
          this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
          this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
          this.tipos = add_lista[TipoBlock](this.tipos, TipoBlock.ESCRITA)
          this.ativo = add_lista(this.ativo, true)
        // longa duracao/objeto

        case NivelIsol.SERIALIZE =>
          this.idItems = add_lista[String](this.idItems, D)
          this.TrIds = add_lista[Int](this.TrIds, Tr)
          this.escopos = add_lista[Escopo](this.escopos, Escopo.OBJETO)
          this.duracoes = add_lista[Duracao](this.duracoes, Duracao.LONGA)
          this.tipos = add_lista[TipoBlock](this.tipos, TipoBlock.ESCRITA)
          this.ativo = add_lista(this.ativo, true)
        // longa duracao
      // retorna true
      1
    } else {
      validar
    }
  }
  def UL(Tr: Int, D: String): Unit = {
    // acha o indice
    def findIndice(trId: List[Int], idItem: List[String]): Int = {
      trId match
        case trIdH :: trIdN =>
          idItem match
            case idItemH :: idItemN =>
              if (trIdH == Tr && idItemH.equalsIgnoreCase(D)) {
                0
              } else {
                if (findIndice(trIdN, idItemN) == -1) { -1 }
                else { 1 + findIndice(trIdN, idItemN) }
              }
            case Nil => -1
        case Nil => -1
    }
    var k = findIndice(this.TrIds, this.idItems)
    if (k != -1) {
      this.ativo = changeListaPos(this.ativo, false, k)
    }
  }

  def ULA(Tr: Int): Unit = {
    for (j <- 0 until this.TrIds.length) {
      if (this.TrIds(j) == Tr && this.ativo(j) == true) {
        this.ativo = changeListaPos(this.ativo, false, j)
      }
    }
  }
}

//salva o estado de um momento especifico do escalonador
class Momento(
    operacaoAtual: String,
    grafo: Wait_For,
    tabelaBloq: LockTable,
    mananger: TrManager,
    waitItem: Wait_Item
) {
  // operacao atual
  val opAtual = operacaoAtual
  // grafo
  val grafoStr = grafo.draw_grafo()
  // lockTable
  val lockStr = tabelaBloq.printEstadoA()
  // trMananger
  val trmStr = mananger.printEstadoA()
  // lista de espera
  val waitStr = waitItem.printEstadoAtual()

  def printMomento() = {
    println("Operacao Atual: " + opAtual)
    println("-")
    print(grafoStr)
    println("-")
    print(lockStr)
    println("-")
    print(trmStr)
    println("-")
    print(waitStr)
    println("-")
  }
}

class Escalonador(isolamento: NivelIsol, sch: List[String]) {
  var scheduler = listaToArray(sch)
  var lastadd = 1
  var posAtual = 0
  var breakP = 0
  // grafo
  var grafo_W = new Wait_For(transacoesQnt(sch))
  // lista de espera
  var lista_espera = new Wait_Item
  // locktable
  var lockTable = new LockTable(isolamento)
  // trMananger
  var trMananger = new TrManager
  ////////////////
  // list[string] scheduler
  var schedulerSerial = ""
  // list[momentos]
  var momentos: List[Momento] = Nil
  // abortar
  // funcoes momento
  def criarMomento(opAtual: String) = {
    val m = new Momento(
      opAtual,
      this.grafo_W,
      this.lockTable,
      this.trMananger,
      this.lista_espera
    )
    this.momentos = add_lista(this.momentos, m)
  }
  /////////////////
def mostrarMomento(
      indiceF: Int,
      indiceI: Int = 0,
      momentos: List[Momento] = this.momentos
  ): Unit = {
    if (indiceF > indiceI) {
      momentos match
        case head :: next => mostrarMomento(indiceF, indiceI + 1, next)
        case Nil          => ()
    } else {
      momentos match
        case head :: next => head.printMomento()
        case Nil          => ()
    }
  }
  // apagar bloqueios
  def apagarBloqueios(Tr: Int) = {
    val k = getListaPos(this.lockTable.TrIds, Tr)
    if (k != -1) {
      while (k != -1) {
        this.lockTable.TrIds = remove_listaPos(this.lockTable.TrIds, k)
        this.lockTable.idItems = remove_listaPos(this.lockTable.idItems, k)
        this.lockTable.escopos = remove_listaPos(this.lockTable.escopos, k)
        this.lockTable.duracoes = remove_listaPos(this.lockTable.duracoes, k)
        this.lockTable.tipos = remove_listaPos(this.lockTable.tipos, k)
        this.lockTable.ativo = remove_listaPos(this.lockTable.ativo, k)
      }
    }
  }
  def abortar(tr: Int) = {
    val posicao = getListaPos(this.trMananger.TrIds, tr)
    //val trAp = getListaPosValue(this.trMananger.TrIds, posicao)
    // apaga as lockTable de tr
    apagarBloqueios(tr)
    // muda o estado de tr para abortado
    this.trMananger.att_Item(
      getListaPosValue(this.trMananger.TrIds, posicao),
      Estado.ABORTADA
    )
    // apaga as arestas de tr para qualquer outra e de qualquer outra para tr
    this.grafo_W.delete_tr(tr)
    // reseta a posicao atual
    this.posAtual = 0
  }
  // processar(Item:string)
  def processarItem(Item: String): Int = {
    val it = Item.split("-")
    val operacao = it(0)
    if (operacao.equalsIgnoreCase("BT")) {
      val posicao = getListaPos(this.trMananger.itemID, it(1).toInt)
      if (posicao == -1) {
        trMananger.add_Item(it(1).toInt, this.lastadd, Estado.ATIVA)
        this.schedulerSerial = this.schedulerSerial + s"${it(0)}(${it(1)})"
        criarMomento(s"${it(0)}(${it(1)})")
        this.lastadd = this.lastadd + 1
      }
      1
    } else if (operacao.equalsIgnoreCase("r")) {
      // tente obter bloqueio de leitura
      val posicao = getListaPos(this.trMananger.itemID, it(1).toInt)
      if (posicao == -1) {
        3
      } else {
        val status = getListaPosValue(this.trMananger.Status, posicao)
        status match
          case Estado.ATIVA =>
            val adicionado = this.lockTable.RL(getListaPosValue(this.trMananger.TrIds, posicao), it(2))
            if (adicionado == 1) {
              this.schedulerSerial =
                this.schedulerSerial + s"${it(0)}${it(1)}(${it(2)})"
              criarMomento(s"${it(0)}${it(1)}(${it(2)})")
              // liberar bloqueios de curta duracao
              if (this.lockTable.isolamento == NivelIsol.READ_COMMIT) {
                this.lockTable
                  .UL(getListaPosValue(this.trMananger.TrIds, posicao), it(2))
              }
              1
            } else if (adicionado < 0) {
              if (math.abs(adicionado) < (getListaPosValue(this.trMananger.TrIds, posicao))) {
                //adicionado
                val ret = getListaPosValue(this.trMananger.TrIds,posicao)
                -ret
              } else {
                this.trMananger.att_Item( getListaPosValue(this.trMananger.TrIds, posicao), Estado.ESPERANDO)
                this.lista_espera.add_wait(it(2),getListaPosValue(this.trMananger.TrIds, posicao))
                grafo_W.add_Aresta(getListaPosValue(this.trMananger.TrIds, posicao), math.abs(adicionado))
                criarMomento(s"${it(0)}${it(1)}(${it(2)})")
                2
                // acho a proxima operacao
              }
            } else {
              3
            }
          // tenta obter bloqueio
          case Estado.CONCLUÍDA => 1 // faz nada
          case Estado.ABORTADA  => 1 // faz nada
          case Estado.ESPERANDO =>
            val adicionado = this.lockTable.RL(getListaPosValue(this.trMananger.TrIds, posicao), it(2))
            trMananger.att_Item(getListaPosValue(this.trMananger.TrIds, posicao),Estado.ATIVA)
            if (adicionado == 1) {
              // remover item do wait item
              //this.lista_espera.remove_waiter(it(2), getListaPosValue(this.trMananger.TrIds, posicao))
              val rmt = this.lista_espera.remove_wait(it(2))
              // remover aresta do grafo
              this.grafo_W.delete_Ltr(rmt)
              this.schedulerSerial =
                this.schedulerSerial + s"${it(0)}${it(1)}(${it(2)})"
              criarMomento(s"${it(0)}${it(1)}(${it(2)})")
              // liberar bloqueios de curta duracao
              if (this.lockTable.isolamento == NivelIsol.READ_COMMIT) {
                this.lockTable
                  .UL(getListaPosValue(this.trMananger.TrIds, posicao), it(2))
              }
              1
            } else if (adicionado < 0) {
              if (
                math.abs(adicionado) < (getListaPosValue(
                  this.trMananger.TrIds,
                  posicao
                ))
              ) {
                val ret = getListaPosValue(this.trMananger.TrIds,posicao)
                -ret
              } else {
                this.trMananger.att_Item(
                  getListaPosValue(this.trMananger.TrIds, posicao),
                  Estado.ESPERANDO
                )
                /*this.lista_espera.add_wait(
                  it(2),
                  getListaPosValue(this.trMananger.TrIds, posicao)
                )*/
                grafo_W.add_Aresta(
                  getListaPosValue(this.trMananger.TrIds, posicao),
                  math.abs(adicionado)
                )
                criarMomento(s"${it(0)}${it(1)}(${it(2)})")
                2
                // acho a proxima operacao
              }
            } else {
              3
            }
      }
    } else if (operacao.equalsIgnoreCase("w")) {
      // tente obter bloqueio de escrita
      val posicao = getListaPos(this.trMananger.itemID, it(1).toInt)
      if (posicao == -1) {
        3
      } else {
        val status = getListaPosValue(this.trMananger.Status, posicao)
        status match
          case Estado.ATIVA =>
            val adicionado = this.lockTable
              .WL(getListaPosValue(this.trMananger.TrIds, posicao), it(2))
            if (adicionado == 1) {
              trMananger.att_Item(
                getListaPosValue(this.trMananger.TrIds, posicao),
                Estado.ATIVA
              )
              this.schedulerSerial =
                this.schedulerSerial + s"${it(0)}${it(1)}(${it(2)})"
              criarMomento(s"${it(0)}${it(1)}(${it(2)})")
              // liberar bloqueios de curta duracao
              if (this.lockTable.isolamento == NivelIsol.READ_UNCOMMIT) {
                this.lockTable
                  .UL(getListaPosValue(this.trMananger.TrIds, posicao), it(2))
              }
              1
            } else if (adicionado < 0) {
             
              if (
                math.abs(adicionado) < (getListaPosValue(
                  this.trMananger.TrIds,
                  posicao
                ))
              ) {
                val ret = getListaPosValue(this.trMananger.TrIds,posicao)
                -ret
              } else {
                this.trMananger.att_Item(
                  getListaPosValue(this.trMananger.TrIds, posicao),
                  Estado.ESPERANDO
                )
                this.lista_espera.add_wait(
                  it(2),
                  getListaPosValue(this.trMananger.TrIds, posicao)
                )
                grafo_W.add_Aresta(
                  getListaPosValue(this.trMananger.TrIds, posicao),
                  math.abs(adicionado)
                )
                criarMomento(s"${it(0)}${it(1)}(${it(2)})")
                2
                // acho a proxima operacao
              }
            } else {
              3
            }
          // tenta obter bloqueio
          case Estado.CONCLUÍDA => 1 // faz nada
          case Estado.ABORTADA  => 1 // faz nada
          case Estado.ESPERANDO =>
            val adicionado = this.lockTable.WL(getListaPosValue(this.trMananger.TrIds, posicao), it(2))
            trMananger.att_Item(getListaPosValue(this.trMananger.TrIds, posicao),Estado.ATIVA)
            if (adicionado == 1) {
              // remover item do wait item
              //this.lista_espera.remove_waiter(it(2), getListaPosValue(this.trMananger.TrIds, posicao))
              val rmt = this.lista_espera.remove_wait(it(2))
              // remover aresta do grafo
              this.grafo_W.delete_Ltr(rmt)
              //acha a primeira transacao que tem o tipo 
              this.schedulerSerial = this.schedulerSerial + s"${it(0)}${it(1)}(${it(2)})"
              criarMomento(s"${it(0)}${it(1)}(${it(2)})")
              // liberar bloqueios de curta duracao
              if (this.lockTable.isolamento == NivelIsol.READ_UNCOMMIT) {
                this.lockTable.UL(getListaPosValue(this.trMananger.TrIds, posicao), it(2))
              }
              1
            }else if (adicionado < 0) {
              if (math.abs(adicionado) < (getListaPosValue(this.trMananger.TrIds, posicao))
              ) {
                val ret = getListaPosValue(this.trMananger.TrIds,posicao)
                -ret
              } else {
                this.trMananger.att_Item(
                  getListaPosValue(this.trMananger.TrIds, posicao),
                  Estado.ESPERANDO
                )
                /*this.lista_espera.add_wait(
                  it(2),
                  getListaPosValue(this.trMananger.TrIds, posicao)
                )*/
                grafo_W.add_Aresta(
                  getListaPosValue(this.trMananger.TrIds, posicao),
                  math.abs(adicionado)
                )
                criarMomento(s"${it(0)}${it(1)}(${it(2)})")
                2
                // acho a proxima operacao
              }
            } else {
              3
            }
      }
    } else if (operacao.equalsIgnoreCase("C")) {
      val posicao = getListaPos(this.trMananger.itemID, it(1).toInt)
      if (posicao == -1) {
        3
      } else {
        val status = getListaPosValue(this.trMananger.Status, posicao) 
        status match
          case Estado.ATIVA =>
            // remove os bloqueios de duracao longa
            this.lockTable.ULA(getListaPosValue(this.trMananger.TrIds, posicao))
            trMananger.att_Item(
              getListaPosValue(this.trMananger.TrIds, posicao),
              Estado.CONCLUÍDA
            )
            this.schedulerSerial = this.schedulerSerial + s"${it(0)}(${it(1)})"
            criarMomento(s"${it(0)}(${it(1)})")
            1
          case Estado.CONCLUÍDA => 1 // faz nada
          case Estado.ABORTADA  => 1 // faz nada
          case Estado.ESPERANDO => 1 // faz nada
      }
    } else {
      3
    }
  }

  def concluido(lista: List[Estado]): Boolean = {
    lista match
      case Nil => true
      case h :: n =>
        if (h == Estado.CONCLUÍDA) { concluido(n) }
        else { false }
  }

  var acabou = false
  var vezes = 0
  // escalonarScheduler(lista[string])
  def escalonarScheduler(): Unit = {
    while (acabou == false) {
      if (this.posAtual >= this.scheduler.length) {
        // alterar o estado de todos os schedules abortados para ativos
        this.trMananger.ativar()
        this.posAtual = 0
      }
      //inicia escalonamento
      var k = processarItem(this.scheduler(this.posAtual))
      if (k == 1) {
        this.posAtual = this.posAtual + 1
        acabou = concluido(this.trMananger.Status)
        //tem ciclo?
        if(this.grafo_W.temCiclo(this.grafo_W.data)){acabou = true}
      }
      else if (k == 2) {
        acabou = concluido(this.trMananger.Status)
        // acha transacao diferente
        this.breakP = this.posAtual + 1
        if(this.breakP == this.scheduler.length) {
          acabou = true
        } else {
          while (achaNum(this.scheduler(this.breakP)) == achaNum( this.scheduler(this.posAtual)) && this.breakP < this.scheduler.size) {
            this.breakP = this.breakP + 1
          }
          // verifico se achou
          // achou
          if (achaNum(this.scheduler(this.breakP)) != achaNum(this.scheduler(this.posAtual))) {
            re_ordenar(this.scheduler, this.posAtual, this.breakP)
          } else { // nao e possivel atrasar, termina escalonamento
            acabou = true
          }
        }
      } else if (k == 3) {
        acabou = true
      } else {
        val it = this.scheduler(this.posAtual).split("-")
        this.schedulerSerial = this.schedulerSerial + s"A(${it(1)})"
        abortar(-k)
        acabou = false
        //aborta k
      }
      if(this.vezes >= 300){
        acabou = true
      }
      this.vezes = this.vezes + 1
    }
  }
}

object Main extends App {
  
  println("/////////////////////////////////////// PROGRAMA ////////////////////////////////////////")
  var rodando = true
  var isolamento = NivelIsol.READ_UNCOMMIT
  var msg = "Iniciando programa com isolamento read uncommitted"
  println("Escolha o nivel de Isolamento:")
  println("1 - read uncommitted  2 - read commit  3 - repeatable read  4 - serializable")
  print(": ")
  val isol = readLine()
  if(isol.equalsIgnoreCase("2")){
    msg = "Iniciando programa com isolamento read committed"
    isolamento = NivelIsol.READ_COMMIT
  }else if(isol.equalsIgnoreCase("3")){

    msg = "Iniciando programa com isolamento repeatable read"
    isolamento = NivelIsol.READ_REPEAT
  }else if(isol.equalsIgnoreCase("4")){

    msg = "Iniciando programa com isolamento serializable"
    isolamento = NivelIsol.SERIALIZE
  }
  println("----------------------------------------------")
  println(msg)
  println("Digite o Escalonamento de entrada")
  print(": ")
  val escalonamentoStr = readLine()
  val escal = readString(escalonamentoStr)
  val scal2PL = new Escalonador(isolamento, escal)
  scal2PL.grafo_W.init()
  scal2PL.escalonarScheduler()
  var posA = 0
  while(rodando){
    println("----------------------------------------------")
    scal2PL.mostrarMomento(posA)
    println("Ordem das operacoes: " + scal2PL.schedulerSerial)
    println("a -> momento anterior   d -> proximo momento  s -> sair")
    print(": ")
    val op = readLine()
    if(op.equalsIgnoreCase("a")){
      if(posA > 0){posA = posA - 1}
      print("\u001b[2J")
    }else if(op.equalsIgnoreCase("d")){
      if(posA < scal2PL.momentos.length - 1 ){posA = posA + 1}
      print("\u001b[2J")
    }else if(op.equalsIgnoreCase("s")){
      rodando = false
      println("saindo....")
    }
  }
}

