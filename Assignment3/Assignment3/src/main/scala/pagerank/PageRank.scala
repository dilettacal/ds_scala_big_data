package pagerank

import org.apache.spark.rdd.RDD
import pagerank.models.Page

import scala.annotation.tailrec

object PageRank {

  /**
    * Computes the pageRank based on the given links.
    *
    * @param links the links
    * @param t     teleportation
    * @param delta minimum value for difference
    * @return pageRanks
    **/
  def computePageRank(links: RDD[(String, Set[String])], t: Double = 0.15, delta: Double = 0.01): RDD[(String, Double)] = {
    val n = links.count()
    val tNorm = t / n
    val ranks = links.mapValues(_ => 1.0 / n)

    @tailrec
    def inner(ranks: RDD[(String, Double)], links: RDD[(String, Set[String])], tNorm: Double, t: Double, delta: Double)
    : RDD[(String, Double)] = {

      //compute the contributions
      val contributions = computeContributions(ranks, links)

      //combine same keys and apply teleportation and damping factors after
      val newRanks = computeNewRanksFromContributions(contributions, tNorm, t)

      val diff = computeDifference(ranks, newRanks)
      println("Diff: " +diff)
      //done, difference is small enough
      if (diff < delta)
        newRanks
      else
        inner(newRanks, links, tNorm, t, delta)



    }

    inner(ranks, links, tNorm, t, delta)
  }

  /**
    * Computes the contributions from the given ranks and links.
    *
    * See the tests and the description in the assignment sheet for more information.
    *
    * HINTs:
    * join and flatMap might be useful
    *
    * - What happens if a page is never linked to? ex. {A->B, B->B}
    * make sure that (A,0) is also in the result, so A doesn't get lost
    *
    * - also pay attention that A->{} should be treated as A->{A} and contribute (A,1)
    */
  def computeContributions(ranks: RDD[(String, Double)], links: RDD[(String, Set[String])]): RDD[(String, Double)] = {

    //Empty Set() values are set to the corresponding key value
    val adaptedLinks = links.map(x => {
      if(x._2.isEmpty)
        (x._1, Set(x._1))
      else (x._1, x._2)
    })

    val contributions = adaptedLinks.join(ranks).flatMap{
      case (pageid, (urls, rank)) => {
          val size = urls.size

          if(size==0) urls.map(dest => (dest, 1.0))
        //not useful: size < nodesNumber && missingLinksToNode.toList.contains((pageid,0.0)) ||
          if(urls.contains(pageid) == false) {
           // urls.map(dest => (dest, rank/size))
            urls.map(dest => (dest, rank/size)).union( urls.map(_ => (pageid,0.0)))
          }
          else urls.map(dest => (dest, rank/size))
      }
    }
    contributions
  }
  /**
    *
    * Computes the new ranks from the contributions
    * The difference is computed the following way in pseudocode:
    *
    * foreach key:
    * - sum its values
    * multiply the values obtained in the previous step by (1-t) and add tNorm
    *
    **/
  def computeNewRanksFromContributions(contributions: RDD[(String, Double)], tNorm: Double, t: Double): RDD[(String, Double)] = {
    contributions.reduceByKey(_ + _).mapValues(tNorm + (1-t)*_)
  }

  /**
    *
    * Computes the difference between the old and new ranks.
    * The difference is computed the following way in pseudocode:
    *
    * foreach key:
    * - obtain the absolute value/modulus of its value from ranks subtracted its value in newRanks
    * sum the values
    *
    **/
  def computeDifference(ranks: RDD[(String, Double)], newRanks: RDD[(String, Double)]): Double = {
  //  List((a,(0.5,0.6)), (b,(0.7,0.85)), (c,(0.2,0.8)))
    println("Compute difference for: \n" + ranks.collect.toList+"-"+ newRanks.collect.toList)
    println(ranks.values.subtract(newRanks.values).collect().toList)
    //values after join are a tuple of old and new ranks: (0.2, 0.8)
    ranks.join(newRanks).mapValues(x => math.abs(x._1-x._2)).values.sum()
  }

  /**
    * Extracts all links from the given page RDD. This is a 3 step process:
    *
    * 1. Project the pages in the form of Title -> Set(links.titles)
    * 2. Project all other links in the form link.title -> Set()
    * 3. Merge the 2 using the rdd union operation followed by a reduceByKey
    *
    * This results in all pages, who have links to be a pair of pageTitle -> Set (linkTitle, linkTitle..)
    * and all pages, who don't have any links in the form of pageTitle -> Set()
    *
    * For some examples see the test cases
    */
  def extractLinksFromPages(pages: RDD[Page]): RDD[(String, Set[String])] = {

    val pageTitlesInRDD = pages.map(page => page.title)
    val withoutDanglingNodes = pages.map(page => (page.title, (page.links.map(l => l.title).toSet)))
    val internalLinks = pages.flatMap(page => page.links).map(link => link.title).distinct()
    val allNodes = pageTitlesInRDD.union(internalLinks).distinct()

    //We need to link dangling nodes to Set() and unify them with withoutDanglingNodes
    val danglingNodes = internalLinks.subtract(pageTitlesInRDD).map(el => (el, Set.empty[String])).distinct()
    val result =  withoutDanglingNodes.union(danglingNodes).distinct().sortBy(x => x._1)
    result

  }

}