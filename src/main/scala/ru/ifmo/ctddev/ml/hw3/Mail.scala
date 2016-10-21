package ru.ifmo.ctddev.ml.hw3

import java.io.InputStream
import java.util.Scanner


case class Mail(subject: Seq[Int], body: Seq[Int])

case class MailWithVerdict(mail: Mail, verdict: Boolean)

object Mail {
  def parse(is: InputStream): Mail = {
    val scanner = new Scanner(is)
    val subjectLine = scanner.nextLine().substring("Subject: ".length)
    scanner.nextLine()
    val bodyLine = scanner.nextLine()
    Mail(subjectLine.split(' ').filterNot(_.isEmpty).map(_.toInt), bodyLine.split(' ').filterNot(_.isEmpty).map(_.toInt))
  }
}
