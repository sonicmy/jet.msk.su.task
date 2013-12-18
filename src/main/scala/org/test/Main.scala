package org.test

import scala.io.Source
import scala.xml._
import javax.servlet.ServletException
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.eclipse.jetty.server.Request
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler.AbstractHandler
import java.util.concurrent.Executors
import java.util.concurrent.Callable

object MainHandler {
  def main(args: Array[String]) {
    val server = new Server(8080)
    server.setHandler(new MainHandler())
    server.start()
    server.join()
  }
}

class MainHandler extends AbstractHandler {
  override def handle(target: String, baseRequest: Request, request: HttpServletRequest, response: HttpServletResponse) {
    def executeYandexRssSearch(phrase: String): Map[String, Int] = {
    	val url = "http://blogs.yandex.ru/search.rss?text="+phrase
     	val xmlResponse = XML.loadString(Source.fromURL(url).mkString)
      		
     	lazy val resp = (xmlResponse \\ "channel" \\ "item" \\ "link").toStream

     	resp.take(10).map{t =>
     		val res = t.text.drop(7).takeWhile('/'!=).split('.')
     		res.init.last+"."+res.last
     		}.groupBy(identity).mapValues(_.size)
     	}

      response.setContentType("text/html;charset=utf-8")
      response.setStatus(HttpServletResponse.SC_OK)
     	baseRequest.setHandled(true)
      
      if(target == "/search") {
      	val phrases = request.getQueryString.split("&").map{ pair => 
      	val key = pair.takeWhile('='!=)
      	if(key == "query")
      		pair.drop(key.length+1)
      	else ""
      	}.filter(_ != "")
				val es = Executors.newFixedThreadPool(10)

      	val futures = for(phrase <- phrases) yield
      	es.submit(new Callable[Map[String, Int]] { 
      		def call():Map[String, Int] = {
      	 		executeYandexRssSearch(phrase)
      	 		}
      	})

				val str = "<html><pre>{<br>" + {
					futures.map(_.get).foldLeft(Map():Map[String, Int])(
  	   	 	(m1,m2) => m1 ++ m2.map {
    	 			case (k,v) => k -> (v + m1.getOrElse(k,0))
     			}
    		).map(x => "\t\t\""+x._1+"\"\t:\t"+x._2).mkString("<br>")
					} +"\n<br>}</pre></html>"
					response.getWriter().println(str)
    	}
  }
}