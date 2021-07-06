package io.iohk.ethereum.network.handshaker

import java.util.concurrent.atomic.AtomicReference

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.utils.NodeStatus
import io.iohk.ethereum.utils.BlockchainConfig

case class EtcHandshaker private (
    handshakerState: HandshakerState[PeerInfo],
    handshakerConfiguration: EtcHandshakerConfiguration
) extends Handshaker[PeerInfo] {

  protected def copy(handshakerState: HandshakerState[PeerInfo]): Handshaker[PeerInfo] =
    EtcHandshaker(handshakerState, handshakerConfiguration)

}

object EtcHandshaker {

  def apply(handshakerConfiguration: EtcHandshakerConfiguration): EtcHandshaker = {
    val initialState = EtcHelloExchangeState(handshakerConfiguration)
    EtcHandshaker(initialState, handshakerConfiguration)
  }

}

trait EtcHandshakerConfiguration {
  val nodeStatusHolder: AtomicReference[NodeStatus]
  val blockchain: Blockchain
  val blockchainReader: BlockchainReader
  val appStateStorage: AppStateStorage
  val peerConfiguration: PeerConfiguration
  val forkResolverOpt: Option[ForkResolver]
  val capabilities: List[Capability]
  val blockchainConfig: BlockchainConfig
}
