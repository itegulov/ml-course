package ru.ifmo.ctddev.ml.hw3

import java.io.{ FileInputStream, File }

import ru.ifmo.ctddev.ml.Metric

object Main {

  def fillTo3(seq: Seq[Long]): Seq[Long] = {
    if (seq.size % 3 != 0) {
      fillTo3(seq :+ 0L)
    } else {
      seq
    }
  }

  def triplizeMail(mail: MailWithVerdict): MailWithVerdict = mail match {
    case MailWithVerdict(Mail(subject, body), verdict) =>
      val newSubject = fillTo3(subject).grouped(3).map { case Seq(f, s, t) => f << 21 | s << 42 | t }.toSeq
      val newBody = fillTo3(body).grouped(3).map { case Seq(f, s, t) => f << 21 | s << 42 | t }.toSeq
      MailWithVerdict(Mail(newSubject, newBody), verdict)
  }

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
    val allMails = foldedMails.flatten
    val (bad, good) = allMails.partition(_.verdict)
    val result = (0 until 10).flatMap { partNumber =>
      val testSet = foldedMails(partNumber)
      val trainSet = foldedMails.patch(partNumber, Nil, 1).reduce(_ ++ _)
      val algorithm = BayesClassifier.train(trainSet)
      algorithm(Mail(Seq(), Seq(61, 61, 61)))
      testSet.map(mailWithVerdict => algorithm(mailWithVerdict.mail))
    }
    val falsePositives = allMails.zip(result).collect {
      case (MailWithVerdict(_, false), true) => 1
    }.sum
    val falseNegatives = allMails.zip(result).collect {
      case (MailWithVerdict(_, true), false) => 1
    }.sum
    val debug = allMails.zip(result).collect {
      case mail @ (MailWithVerdict(_, false), true) => mail
    }
    val falsePositivePercent = falsePositives.toDouble / good.size
    val falseNegativePercent = falseNegatives.toDouble / bad.size
    val dataInt = allMails.map(x => if (x.verdict) 1 else 0)
    val resultInt = result.map(x => if (x) 1 else 0)
    val f1Score = Metric.f1Score(dataInt, resultInt)
    val accuracy = resultInt.zip(dataInt).map { case (x, y) => if (x == y) 1 else 0 }.sum.toDouble / dataInt.size
    println(
      s"""
         |=======================================
         |False positive = $falsePositives, $falsePositivePercent
         |False negative = $falseNegatives, $falseNegativePercent
         |F1 score       = $f1Score
         |Accuracy       = $accuracy
         |=======================================
       """.stripMargin
    )
  }
}
