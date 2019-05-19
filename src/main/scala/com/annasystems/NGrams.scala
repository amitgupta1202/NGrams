package com.annasystems

import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.util.Try

object NGrams extends App {

  //Tests
  assert(processInput("2,the") == "lamb,0.375;teacher,0.250;children,0.125;eager,0.125;rule,0.125")
  assert(processInput("3,the lamb") == "love,0.333;was,0.333;you,0.333")

  //console input
  println(processInput(scala.io.StdIn.readLine()))


  def processInput(input: String): String = {
    val text =
      """Mary had a little lamb its fleece was white as snow;
        |And everywhere that Mary went, the lamb was sure to go.
        |It followed her to school one day, which was against the rule;
        |It made the children laugh and play, to see a lamb at school.
        |And so the teacher turned it out, but still it lingered near,
        |And waited patiently about till Mary did appear.
        |\"Why does the lamb love Mary so?\" the eager children cry;
        |"Why, Mary loves the lamb, you know\" the teacher did reply.\"""".stripMargin

    val inputs = input.split(",")
    val nGramSize = inputs(0).toInt
    val words = inputs(1).split(" ").map(Word)
    val indexedNGram = indexNGram(text, nGramSize)
    val maybePrediction = predict(indexedNGram, words)
    val predictionWithWeights = maybePrediction.map(calculatePredictionWeight).getOrElse(List.empty)
    predictionWithWeights.map(w => f"${w.word.underlying},${w.weight}%1.3f").mkString(";")
  }

  def indexNGram(text: String, nGramSize: Int): NGramMap = {
    def extractWords(text: String): Array[Word] = text.split("[\\s@&.?$+-,;]+").collect {
      case w if !w.trim.isEmpty => Word(w)
    }

    def indexNGramSequence(nGramMap: NGramMap, words: Seq[Word]): Unit = words.foldLeft(nGramMap) { (map, word) =>
      map.putIfAbsent(word, NextNGramAndCounter(new AtomicInteger(0), new TrieMap()))
      val counter = map(word)
      counter.count.incrementAndGet()
      counter.nGramMap
    }

    val words = extractWords(text)
    val nGramMap: NGramMap = new TrieMap[Word, NextNGramAndCounter]()
    val startFrom = nGramSize - 1

    (startFrom until words.length).foreach(idx => indexNGramSequence(nGramMap, (idx - startFrom to idx).map(words)))
    nGramMap
  }

  def predict(nGramMap: NGramMap, previousWords: Seq[Word]): Option[NextNGramAndCounter] = {
    @tailrec
    def doIt(ngc: NextNGramAndCounter, pw: Seq[Word]): NextNGramAndCounter =
      if (pw.isEmpty) ngc
      else doIt(ngc.nGramMap(pw.head), pw.drop(1))

    Try(doIt(NextNGramAndCounter(new AtomicInteger(0), nGramMap), previousWords)).toOption
  }

  def calculatePredictionWeight(nextNGramAndCounter: NextNGramAndCounter): List[PredictionWeight] =
    nextNGramAndCounter.nGramMap.map {
      case (word, NextNGramAndCounter(count, _)) => PredictionWeight(word, count.doubleValue() / nextNGramAndCounter.count.doubleValue())
    }.toList.sortBy(p => (-p.weight, p.word.underlying))

  case class Word(underlying: String) extends AnyVal

  case class NextNGramAndCounter(count: AtomicInteger, nGramMap: NGramMap)

  case class PredictionWeight(word: Word, weight: Double)

  type NGramMap = TrieMap[Word, NextNGramAndCounter]
}



