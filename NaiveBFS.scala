import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import java.net._
import java.io._

case class wikiPage(title:String)

final val wikiUrl = "https://en.wikipedia.org/wiki/"

def getPageContent(link:String):String = {
  val url = new URL(wikiUrl+link)
  val br = new BufferedReader(new InputStreamReader(url.openStream()));
  var sb = new StringBuilder()
  var cont = true
  while(cont){
    val in = br.readLine
    if(in==null)
      cont = false
    else
      sb ++= in
  }
  br.close
  return sb.toString
}

def getPage(link:String):Option[wikiPage] = {
  val start = System.nanoTime()
  try {
    val url = new URL(wikiUrl+link)
    val br = new BufferedReader(new InputStreamReader(url.openStream()));
    for(i <- 0 until 4)
      br.readLine
    val titleLine = br.readLine
    br.close
    val title = titleLine.substring(7, titleLine.indexOf(" - Wiki"))
    println((System.nanoTime-start)/1e6+" ms on "+link)
    return Some(wikiPage(title))
  }
  catch {
    case _ : Throwable => {
      println((System.nanoTime-start)/1e6+" ms on "+link)
      return None
    }
  }
}

def getLinks(pageTxt:String):ListBuffer[String] = {
  var in = pageTxt
  val links:ListBuffer[String] = ListBuffer()
  val linkPattern = "<a href=\"/wiki/"
  var i = in.indexOf(linkPattern)
  while(i != -1) {
    in = in.substring(i+linkPattern.length, in.length)
    val link = (in.substring(0,in.indexOf("\"")))
    if(!link.contains(":") && !link.contains("#"))
      links += link
    i = in.indexOf(linkPattern)
  }
  //links.foreach(println)
  links
}

val toParent:Map[wikiPage, wikiPage] = Map()

def bfs(start:String, end:String):Boolean = {
  val endPage = getPage(end.replace(" ","_")).get
  val startPage = getPage(start.replace(" ","_")).get
  val discovered:Set[wikiPage] = Set()
  val queue:Queue[wikiPage] = Queue()
  discovered += startPage
  queue.enqueue(startPage)
  while(!queue.isEmpty) {
    val currPage = queue.dequeue
    println("investigating "+currPage.title)
    val pageContent = getPageContent(currPage.title.replace(" ","_"))
    if(currPage.title == endPage.title)
      return true
    val links = getLinks(pageContent)
    for(link <- links) {
      val newPage = getPage(link)
      if(!newPage.isEmpty && !discovered(newPage.get)) {
        discovered += newPage.get
        toParent += (newPage.get -> currPage)
        queue.enqueue(newPage.get)
      }
    }
  }
  return false
}

bfs("Silas Strawn", "Lawyer")
