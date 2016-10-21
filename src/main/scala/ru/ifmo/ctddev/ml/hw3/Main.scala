package ru.ifmo.ctddev.ml.hw3

import java.io.{ File, FileInputStream }

object Main {

  def main(args: Array[String]): Unit = {
    val foldedMails = for (partNumber <- 1 to 10) yield {
      val file = new File(getClass.getResource(s"/bayes/part$partNumber").getPath)
      val mails = for (mailFile <- file.listFiles()) yield {
        val mail = Mail.parse(new FileInputStream(mailFile))
        val verdict = mailFile.getName.contains("spmsg")
        MailWithVerdict(mail, verdict)
      }
      mails.toSeq
    }
    val results = for (partNumber <- 0 until 10) yield {
      val testSet = foldedMails(partNumber)
      val trainSet = foldedMails.patch(partNumber, Nil, 1).reduce(_ ++ _)
      val algorithm = BayesClassifier.train(trainSet)
      val error = testSet.map {
        case MailWithVerdict(mail, verdict) =>
          if (algorithm(mail) != verdict) 1 else 0
      }.sum
      error / testSet.length
    }
    println(results)
  }
}
