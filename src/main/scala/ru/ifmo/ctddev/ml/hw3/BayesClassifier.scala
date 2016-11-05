package ru.ifmo.ctddev.ml.hw3

/**
  * Created by Aleksei Latyshev on 21.10.2016.
  */
object BayesClassifier {
  def train(mails: Seq[MailWithVerdict]): Mail => Boolean = {
    val (bad, good) = mails.partition(_.verdict)
    val words = mails.flatMap(x => x.mail.subject ++ x.mail.body).distinct

    val b = bad
      .flatMap(x => (x.mail.subject ++ x.mail.body).distinct)
      .foldLeft(Map.empty[Long, Long])((map, word) => map.updated(word, map.getOrElse(word, 0L) + 1))
      .withDefaultValue(1L)
    val g = good
      .flatMap(x => (x.mail.subject ++ x.mail.body).distinct)
      .foldLeft(Map.empty[Long, Long])((map, word) => map.updated(word, map.getOrElse(word, 0L) + 1))
      .withDefaultValue(1L)

    val allBad = bad.map(x => x.mail.subject.size + x.mail.body.size).sum
    val allGood = good.map(x => x.mail.subject.size + x.mail.body.size).sum

    val spamicity = words
      .filter(word => b.contains(word) && g.contains(word))
      .filter(word => b(word) != 0)
      .map(word => word -> g(word).toDouble / b(word)).toMap
    val considered = words.filter(w => Math.abs(1 - spamicity.getOrElse(w, 0D)) > 0.5).toSet

    (mail) => {
      val mailWords = mail.subject ++ mail.body
      val x = mailWords.collect {
        case w if considered.contains(w) =>
          Math.log(b(w).toDouble / g(w))
      }.sum
      x > 5
    }
  }
}
