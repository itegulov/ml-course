package ru.ifmo.ctddev.ml.hw3

/**
  * Created by Aleksei Latyshev on 21.10.2016.
  */
object BayesClassifier {
  def train(mails : Seq[MailWithVerdict]) : Mail => Boolean = {
    val words = mails.map(x => x.mail.body ++ x.mail.subject).fold(Seq())(_ ++ _).foldLeft(Set.empty[Int])(_ + _)
    val (bad, good) = mails.partition(_.verdict)
    val m = (for (word <- words) yield {
      val inBad = bad.count(x => (x.mail.body ++ x.mail.subject).contains(word))
      val inGood = good.count(x => (x.mail.body ++ x.mail.subject).contains(word))
      (word, (inBad, inGood))
    }).toMap
    (mail) => {
      val ws = mail.body ++ mail.subject
      val (x, y) = ws.map(x => {
        val (inBad, inGood) = m.getOrElse(x, (bad.size, good.size))
        (Math.log((inBad + inGood) * 1.0 / (bad.size + good.size)), Math.log(inBad * 1.0 / bad.size))
      }).reduce { (_, _) match {
        case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
      }}
      Math.exp(y - x) * (bad.size * 1.0 / (bad.size + good.size)) >= 0.5
    }
  }
}
