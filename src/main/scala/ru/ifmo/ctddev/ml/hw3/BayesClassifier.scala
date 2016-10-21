package ru.ifmo.ctddev.ml.hw3

/**
  * Created by Aleksei Latyshev on 21.10.2016.
  */
object BayesClassifier {
  def train(mails: Seq[MailWithVerdict]): Mail => Boolean = {
    val words = mails.flatMap(x => x.mail.body ++ x.mail.subject).toSet
    val (bad, good) = mails.partition(_.verdict)
    val badMap = bad.flatMap(x => x.mail.body ++ x.mail.subject)
      .foldLeft(Map.empty[Int, Int])((count, word) => count + (word -> (count.getOrElse(word, 0) + 1)))
    val goodMap = good.flatMap(x => x.mail.body ++ x.mail.subject)
      .foldLeft(Map.empty[Int, Int])((count, word) => count + (word -> (count.getOrElse(word, 0) + 1)))
    (mail) => {
      val ws = mail.body ++ mail.subject
      val (x, y) = ws.map(x => {
        val inBad = badMap.getOrElse(x, bad.size)
        val inGood = goodMap.getOrElse(x, good.size)
        (Math.log((inBad + inGood) * 1.0 / (bad.size + good.size)), Math.log(inBad * 1.0 / bad.size))
      }).reduce {
        (_, _) match {
          case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
        }
      }
      Math.exp(x - y) * (bad.size * 1.0 / (bad.size + good.size)) >= 0.5
    }
  }
}
