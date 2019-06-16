import java.net._
import java.io._
import scala.collection.mutable.Buffer
import scala.collection.mutable.Set

object WikiPathFinder {

  final val wikiUrl = "https://en.wikipedia.org/wiki/"

  case class Node(page:String, dist:Int, parent:Node, links:Set[String])

  val discovered:Buffer[Node] = Buffer()
  val done:Set[Node] = Set()

  def main(args:Array[String]) {
    //println(bfs(arg(0)),arg(1))
    extractLinks(readURL(args(0))).foreach(println)
  }

  /*//Throws away all text outside of <p> ... </p> tags
  def getParagraph(txt:String):String = {
    val res:Buffer[String] = Buffer()
    var in = txt
    var i = in.indexOf("<p>")
    var j = in.indexOf("</p>")
    while(i != -1 && j != -1) {
      if(j<i) {
          in = in.substring(j+"</p>".length, in.length)
      }
      else {
        val s = in.substring(i+"<p>".length, j)
        res += s
        in = in.substring(j+"</p>".length, in.length)
      }
      i = in.indexOf("<p>")
      j = in.indexOf("</p>")
    }
    return res.reduce(_+_)
  }*/

  def extractLinks(txt:String):Set[String] = {
    var in = txt
    val links:Set[String] = Set()
    val linkPattern = "<a href=\"/wiki/"
    var i = in.indexOf(linkPattern)
    while(i != -1) {
      in = in.substring(i+linkPattern.length, in.length)
      links += (in.substring(0,in.indexOf("\"")))
      i = in.indexOf(linkPattern)
    }
    return links
  }

  def readURL(page:String):String = {
    val url = new URL(wikiUrl+page)
    val br = new BufferedReader(new InputStreamReader(url.openStream()));
    var txt = ""
    var cont = true
    while(cont){
      val in = br.readLine
      if(in==null)
        cont = false
      else
        txt+=in
    }
    br.close
    return txt
  }

  def isValidPage(page:String):Boolean = {

  }

  def constructNodes(parent:Node, links:Set[String]):Buffer[Node] = {
    val res:Buffer[Node] = Buffer()
    for(link <- links) {
      val n = Node(link, parent.dist+1, parent, null)
      res+=n
    }
    return res
  }

  def bfs(src:String, dest:String):Int = {
    val maxDepth = 3
    val sLinks = extractLinks(readURL(src))
    val s = Node(src, 0, null, sLinks)
    done += s

    while(discovered.length != 0) {
      val a = discovered.head
      discovered -= a
      if(a.page.toLowerCase == dest.toLowerCase)
        return a.dist
      if(a.dist > maxDepth) {
        println("Maximum depth reached")
        return -1
      }

    }
    println("Target not found")
    return -1
  }


}
