package io.iohk.ethereum.forkid

import java.util.zip.CRC32

import akka.util.ByteString
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.ByteUtils._

sealed trait ForkIdValidationResult
case object Connect extends ForkIdValidationResult
case object ErrRemoteStale extends ForkIdValidationResult
case object ErrLocalIncompatibleOrStale extends ForkIdValidationResult

object ForkIdValidator {

  // scalastyle:off magic.number
  val maxUInt64 = (BigInt(0x7fffffffffffffffL) << 1) + 1

  def validatePeer(
      genesisHash: ByteString,
      config: BlockchainConfig
  )(currentHeight: BigInt, remoteForkId: ForkId): ForkIdValidationResult = {
    val forks = ForkId.gatherForks(config)
    validatePeer(genesisHash, forks)(currentHeight, remoteForkId)
  }

  def validatePeer(
      genesisHash: ByteString,
      forks: List[BigInt]
  )(currentHeight: BigInt, remoteId: ForkId): ForkIdValidationResult = {
    val sums: Vector[BigInt] = calculateChecksums(genesisHash, forks)
    val id = remoteId
    // find the first unpassed fork and it's index
    val (unpassedFork, i) = (forks :+ maxUInt64).zipWithIndex.find { case (fork, _) => currentHeight < fork }.get

    // The checks are left biased -> whenever a result is found we need to short circuit
    val validationResult: Option[ForkIdValidationResult] = (for {
      /**
        * 1) If local and remote FORK_HASH matches, compare local head to FORK_NEXT.
        * The two nodes are in the same fork state currently.
        * They might know of differing future forks, but that’s not relevant until the fork triggers (might be postponed, nodes might be updated to match).
        * 1a) A remotely announced but remotely not passed block is already passed locally, disconnect, since the chains are incompatible.
        * 1b) No remotely announced fork; or not yet passed locally, connect.
        */
      _ <- checkMatchingHashes(sums, remoteId, currentHeight, i).toLeft(())
      /**
        * 2) If the remote FORK_HASH is a subset of the local past forks and the remote FORK_NEXT matches with the locally following fork block number, connect.
        * Remote node is currently syncing. It might eventually diverge from us, but at this current point in time we don’t have enough information.
        */
      _ <- checkSubset(sums, forks, remoteId, i).toLeft(())
      /**
        * 3) If the remote FORK_HASH is a superset of the local past forks and can be completed with locally known future forks, connect.
        * Local node is currently syncing. It might eventually diverge from the remote, but at this current point in time we don’t have enough information.
        */
      _ <- checkSuperset(sums, remoteId, i).toLeft(())
      /**
        * 4) Reject in all other cases.
        */
      _ <- Left(ErrLocalIncompatibleOrStale)
    } yield ()).swap.toOption

    // Hard to say if nodes are compatible, so we need to allow to connect
    validationResult.getOrElse(Connect)
  }

  private def calculateChecksums(
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

  private def checkMatchingHashes(
      sums: Vector[BigInt],
      remoteId: ForkId,
      currentHeight: BigInt,
      i: Int
  ): Option[ForkIdValidationResult] = {
    if (sums(i) == remoteId.hash) {
      if (remoteId.next.isDefined && currentHeight >= remoteId.next.get) {
        Some(ErrLocalIncompatibleOrStale)
      } else {
        Some(Connect)
      }
    } else { None }
  }

  def checkSubset(sums: Vector[BigInt], forks: List[BigInt], remoteId: ForkId, i: Int): Option[ForkIdValidationResult] =
    sums
      .zip(forks)
      .take(i)
      .find { case (sum, _) => sum == remoteId.hash }
      .map { case (_, fork) => if (fork == remoteId.next.getOrElse(0)) Connect else ErrRemoteStale }

  def checkSuperset(sums: Vector[BigInt], remoteId: ForkId, i: Int): Option[ForkIdValidationResult] = {
    sums.drop(i).find(_ == remoteId.hash).map(_ => Connect)
  }

}
