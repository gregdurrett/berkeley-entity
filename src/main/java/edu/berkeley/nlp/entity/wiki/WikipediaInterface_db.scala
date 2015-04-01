package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.CounterMap
import scalikejdbc._

import scala.collection.mutable

/**
 * Created by matthewfl
 */
class WikipediaInterface_db (conn : String) {


  Class.forName("org.postgresql.Driver")
  val settings = ConnectionPoolSettings(
    initialSize = 1,
    maxSize = 8,
    connectionTimeoutMillis = 3000L,
    validationQuery = "select 1")
  ConnectionPool.add(this, conn, "wiki", "wiki", settings)

  /*val i1 : Option[Int] = using(DB(ConnectionPool.borrow(this))) { db =>
    db localTx { implicit session =>
      SQL("select 5 as i").map(r=>r.get[Int](1)).single.apply()
    }
  }

  println("value of il: "+i1.get)
*/


  def disambigRes(query: Query) = {
    Seq[String]()
  }

  def TitlesGivenSurface = {
    var m = new CounterMap[String, String]()
    using(DB(ConnectionPool.borrow(this))) { db => {
      db localTx { implicit session => {
        SQL("select surface_text, page_title, count(*) as cnt from links inner join page on page_latest = to_id group by surface_text, page_title")
          .fetchSize(5000)
          .foreach(res => {
          m.incrementCount(res.string("surface_text"), res.string("page_title"), res.int("cnt"))
        })
      }}
    }}
    new WikipediaTitleGivenSurfaceDB(m)
  }

  def Redirects = {
    val m = new mutable.HashMap[String,String]()
    using(DB(ConnectionPool.borrow(this))) { db =>
      db localTx { implicit session => {
        SQL(
          """select pf.page_title as from_page, pt.page_title as to_page
             from page pf inner join links on links.from_id = pf.page_latest
             inner join page pt on links.to_id = pt.page_latest
             where pf.page_is_redirect = 1 limit 10000"""
        ).fetchSize(5000)
          .foreach(res => {
          println("loading redirect "+res.string("from_page"))
          m += (res.string("from_page") -> res.string("to_page"))
        })
      }}
    }
    new WikipediaRedirectsDB(m)
  }

  def Links = {
    // TODO:
    val ind = new Indexer[String]()

    null.asInstanceOf[WikipediaLinkDB]
  }

  def Aux = {
    null.asInstanceOf[WikipediaAuxDB]
  }

}


object WikipediaInterface_db {

  // database connection string
  val conn = "jdbc:postgresql://10.7.0.17/wiki"

  // most stuff should come out of the db
  val wikipediaPath = ""

  val categoryDBInputPath = ""
  val categoryDBOutputPath = ""

  val outputPath = ""

  def main(args : Array[String]): Unit = {

    LightRunner.initializeOutput(WikipediaInterface_db.getClass);
    LightRunner.populateScala(WikipediaInterface_db.getClass, args);


    var db = new WikipediaInterface_db(conn)

    val catDB = if(!categoryDBInputPath.isEmpty) {
      GUtil.load(categoryDBInputPath).asInstanceOf[WikipediaCategoryDB]
    } else {
      // this is really slow to make the cat database, you should want to avoid this
      assert(false)
      null.asInstanceOf[WikipediaCategoryDB]
    }

    val wi = new WikipediaInterface(db.TitlesGivenSurface, db.Redirects, catDB, db.Links, db.Aux)

    GUtil.save(wi, outputPath)

    if (categoryDBOutputPath != "") {
      GUtil.save(catDB, categoryDBOutputPath);
    }
    LightRunner.finalizeOutput();

    // going to punt on the links db, as it appears that it is not being used

  }
}