package io.iohk.ethereum.forkid

import akka.util.ByteString
import cats.Monad
import cats.data.EitherT
import cats.implicits._
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.ByteUtils._
import monix.eval.Task
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.zip.CRC32

sealed trait ForkIdValidationResult
case object Connect extends ForkIdValidationResult
case object ErrRemoteStale extends ForkIdValidationResult
case object ErrLocalIncompatibleOrStale extends ForkIdValidationResult

object ForkIdValidator {

  implicit val unsafeLogger = Slf4jLogger.getLogger[Task]

  // scalastyle:off magic.number
  val maxUInt64 = (BigInt(0x7fffffffffffffffL) << 1) + 1

  def validatePeer[F[_]: Monad: Logger](
      genesisHash: ByteString,
      config: BlockchainConfig
  )(currentHeight: BigInt, remoteForkId: ForkId): F[ForkIdValidationResult] = {
    val forks = ForkId.gatherForks(config)
    validatePeer[F](genesisHash, forks)(currentHeight, remoteForkId)
  }

  def validatePeer[F[_]: Monad: Logger](
      genesisHash: ByteString,
      forks: List[BigInt]
  )(currentHeight: BigInt, remoteId: ForkId): F[ForkIdValidationResult] = {
    val checksums: Vector[BigInt] = calculateCheckchecksums(genesisHash, forks)

    // find the first unpassed fork and it's index
    val (unpassedFork, i) = (forks :+ maxUInt64).zipWithIndex.find { case (fork, _) => currentHeight < fork }.get

    // The checks are left biased -> whenever a result is found we need to short circuit
    val validate: F[Option[ForkIdValidationResult]] = (for {
      _ <- EitherT.liftF(Logger[F].debug(s"Before checkMatchingHashes"))
      matching <- EitherT.fromEither[F](
        checkMatchingHashes(checksums, remoteId, currentHeight, i).toLeft("hashes didn't match")
      )
      _ <- EitherT.liftF(Logger[F].debug(s"checkMatchingHashes result: $matching"))
      _ <- EitherT.liftF(Logger[F].debug(s"Before checkSubset"))
      sub <- EitherT.fromEither[F](checkSubset(checksums, forks, remoteId, i).toLeft("not in subset"))
      _ <- EitherT.liftF(Logger[F].debug(s"checkSubset result: $sub"))
      _ <- EitherT.liftF(Logger[F].debug(s"Before checkSuperset"))
      sup <- EitherT.fromEither[F](checkSuperset(checksums, remoteId, i).toLeft("not in superset"))
      _ <- EitherT.liftF(Logger[F].debug(s"checkSuperset result: $sup"))
      _ <- EitherT.liftF(Logger[F].debug(s"No check succeeded"))
      res <- EitherT.fromEither[F](Either.left[ForkIdValidationResult, Unit](ErrLocalIncompatibleOrStale))
    } yield (res)).value
      .map(_.swap)
      .flatMap(res => Logger[F].debug(s"Validation result is: $res") >> Monad[F].pure(res.toOption))

    Logger[F].debug(s"Validating $remoteId") >>
      Logger[F].debug(s"Forks list: $forks") >>
      Logger[F].debug(s"Unpassed fork $unpassedFork was found at index $i") >>
      validate.map(_.getOrElse(Connect)) // Impossible to say if nodes are compatible, so we need to allow to connect
  }

  private def calculateCheckchecksums(
      genesisHash: ByteString,
      forks: List[BigInt]
  ): Vector[BigInt] = {
    val crc = new CRC32()
    crc.update(genesisHash.asByteBuffer)
    val genesisChecksum = BigInt(crc.getValue())

    genesisChecksum +: (forks.map { fork =>
      crc.update(bigIntToBytes(fork, 8))
      BigInt(crc.getValue())
    }).toVector
  }

  /**
    * 1) If local and remote FORK_HASH matches, compare local head to FORK_NEXT.
    * The two nodes are in the same fork state currently.
    * They might know of differing future forks, but that’s not relevant until the fork triggers (might be postponed, nodes might be updated to match).
    * 1a) A remotely announced but remotely not passed block is already passed locally, disconnect, since the chains are incompatible.
    * 1b) No remotely announced fork; or not yet passed locally, connect.
    */
  private def checkMatchingHashes(
      checksums: Vector[BigInt],
      remoteId: ForkId,
      currentHeight: BigInt,
      i: Int
  ): Option[ForkIdValidationResult] = {
    if (checksums(i) == remoteId.hash) {
      if (remoteId.next.isDefined && currentHeight >= remoteId.next.get) {
        Some(ErrLocalIncompatibleOrStale)
      } else {
        Some(Connect)
      }
    } else { None }
  }

  /**
    * 2) If the remote FORK_HASH is a subset of the local past forks and the remote FORK_NEXT matches with the locally following fork block number, connect.
    * Remote node is currently syncing. It might eventually diverge from us, but at this current point in time we don’t have enough information.
    */
  def checkSubset(
      checksums: Vector[BigInt],
      forks: List[BigInt],
      remoteId: ForkId,
      i: Int
  ): Option[ForkIdValidationResult] =
    checksums
      .zip(forks)
      .take(i)
      .find { case (sum, _) => sum == remoteId.hash }
      .map { case (_, fork) => if (fork == remoteId.next.getOrElse(0)) Connect else ErrRemoteStale }

  /**
    * 3) If the remote FORK_HASH is a superset of the local past forks and can be completed with locally known future forks, connect.
    * Local node is currently syncing. It might eventually diverge from the remote, but at this current point in time we don’t have enough information.
    */
  def checkSuperset(checksums: Vector[BigInt], remoteId: ForkId, i: Int): Option[ForkIdValidationResult] = {
    checksums.drop(i).find(_ == remoteId.hash).map(_ => Connect)
  }

}
