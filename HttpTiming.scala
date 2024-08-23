//>using dep "org.http4s::http4s-netty-client:0.5.18"
//>using dep "org.http4s::http4s-ember-client:0.23.27"
//>using dep "org.http4s::http4s-blaze-client:0.23.16"

package dev.pawlik.michal

import cats.effect.*
import cats.syntax.all.*
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.netty.client.NettyClientBuilder
import org.http4s.syntax.all.*

import scala.concurrent.duration.*

object HttpTiming extends IOApp {

  import java.util.UUID

  override def run(args: List[String]): IO[ExitCode] = {
    val domain = args.get(0).getOrElse("example.com")
    val warmup = args.get(1).contains("true")
    (nettyHttpClient, blazeHttpClient, emberHttpClient).tupled
      .use { (netty, blaze, ember) =>
        IO.println(s"Running benchmark against $domain ${
            if (warmup) "with warmup" else "without warmup"
          }") *>
          List(
            benchmarkClient("netty", netty, domain, warmup),
            benchmarkClient("blaze", blaze, domain, warmup),
            benchmarkClient("ember", ember, domain, warmup)
          ).sequence.flatMap(list =>
            IO.println(
              list.mkString("\n")
            )
          )
      }
      .as(ExitCode.Success)

  }

  val nettyHttpClient =
    NettyClientBuilder[IO].resource

  val blazeHttpClient =
    BlazeClientBuilder[IO].resource

  val emberHttpClient =
    EmberClientBuilder.default[IO].build

  private def benchmarkClient(
      clientName: String,
      client: Client[IO],
      domain: String,
      shouldWarmup: Boolean = true
  ) =
    for {
      _ <- IO.println(s"[$clientName] Benchmark started")
      _ <-
        if (shouldWarmup)
          IO.println(
            s"[$clientName] Warming up for connections to $domain"
          ) *> clientWarmup(client, domain) *> IO.println(
            s"[$clientName] Warmup complete"
          )
        else IO.println(s"[$clientName] Skipping warmup phase")
      individualMeasurements <-
        List
          .range(0, 30)
          .traverse(_ =>
            measuredRequest(clientName, client, "example.com") <* IO.sleep(
              50.millis
            )
          )

      totalTime = DurationFormatter.format(individualMeasurements.reduce(_ + _))
      maxDuration = DurationFormatter.format(individualMeasurements.max)
      minDuration = DurationFormatter.format(individualMeasurements.min)
      averageDuration = DurationFormatter.format(
        DurationStats.averageDuration(individualMeasurements)
      )
      medianDuration = DurationFormatter.format(
        DurationStats.medianDuration(individualMeasurements)
      )
      _ <- IO.println(
        s"[$clientName] example.com benchmark complete in ${totalTime}"
      )
      _ <- IO.println(
        s"[$clientName] max duration ${maxDuration}"
      )
      _ <- IO.println(
        s"[$clientName] min duration ${minDuration}"
      )
      _ <- IO.println(
        s"[$clientName] average duration ${averageDuration}"
      )
      _ <- IO.println(
        s"[$clientName] median duration ${medianDuration}"
      )
    } yield (
      clientName,
      domain,
      totalTime,
      maxDuration,
      minDuration,
      averageDuration,
      medianDuration
    )

  private def clientWarmup(client: Client[IO], domain: String): IO[Unit] =
    List
      .range(0, 20)
      .traverse_ { url =>
        exampleRequest(client, domain).flatMap(_ => IO.unit)
      }

  private def exampleRequest(
      client: Client[IO],
      domain: String
  ) =
    client
      .get(s"https://$domain/?id=${UUID.randomUUID().toString()}")(_ => IO.unit)

  private def measuredRequest(
      clientName: String,
      client: Client[IO],
      domain: String
  ) =
    Temporal[IO].timed(exampleRequest(client, domain)).map(_._1)
}

object DurationFormatter {
  def format(duration: Duration): String = {
    val ns = duration.toNanos
    val ms = ns / 1000000 // Convert nanoseconds to milliseconds
    val s = ms / 1000 // Convert milliseconds to seconds
    val m = s / 60 // Convert seconds to minutes
    val h = m / 60 // Convert minutes to hours

    val unit =
      if (h > 0) "h"
      else if (m > 0) "m"
      else if (s > 0) "s"
      else if (ms > 0) "ms"
      else "ns"

    val value =
      if (unit == "ns") ms
      else if (unit == "ms") ms
      else if (unit == "s") s
      else if (unit == "m") m
      else h

    f"$value.${ns % 1_000} ${
        if (unit == "ns") "ns"
        else if (unit == "ms") "ms"
        else if (unit == "s") "s"
        else if (unit == "m") "min"
        else "hr"
      }"
  }
}

object DurationStats {
  import java.util.concurrent.TimeUnit
  import scala.math.BigDecimal.RoundingMode
  import scala.math.BigDecimal.RoundingMode.HALF_UP

  def averageDuration(durations: List[FiniteDuration]): FiniteDuration = {
    val totalDurationNs = durations.map(_.toNanos).sum
    val averageNs =
      BigDecimal(totalDurationNs.toDouble) / BigDecimal(durations.length)
    val averageSec = averageNs.setScale(6, RoundingMode.HALF_UP).toLong
    FiniteDuration(averageSec, TimeUnit.NANOSECONDS)
  }

  def medianDuration(durations: Seq[FiniteDuration]) = {
    val (lower, upper) = durations.sortWith(_ < _).splitAt(durations.size / 2)
    if (durations.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }

}
