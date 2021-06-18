package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.domain.{BlockBody, BlockHeader, Receipt}
import io.iohk.ethereum.mpt.{ExtensionNode, HashNode, HexPrefix, MptNode}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.ETH62._
import io.iohk.ethereum.network.p2p.messages.ETH63._
import io.iohk.ethereum.network.p2p.messages.ETH63.MptNodeEncoders._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.network.{EtcPeerManagerActor, PeerId}
import io.iohk.ethereum.{Fixtures, Timeouts, crypto}
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockchainHostActorSpec extends AnyFlatSpec with Matchers {

  it should "return Receipts for block hashes" in new TestSetup {
    peerEventBus.expectMsg(
      Subscribe(
        MessageClassifier(
          Set(Codes.GetNodeDataCode, Codes.GetReceiptsCode, Codes.GetBlockBodiesCode, Codes.GetBlockHeadersCode),
          PeerSelector.AllPeers
        )
      )
    )

    //given
    val receiptsHashes = Seq(
      ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
      ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"))
    )

    val receipts: Seq[Seq[Receipt]] = Seq(Seq(), Seq())

    blockchain
      .storeReceipts(receiptsHashes(0), receipts(0))
      .and(blockchain.storeReceipts(receiptsHashes(1), receipts(1)))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetReceipts(receiptsHashes), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(Receipts(receipts), peerId))
  }

  it should "return BlockBodies for block hashes" in new TestSetup {
    //given
    val blockBodiesHashes = Seq(
      ByteString(Hex.decode("a218e2c611f21232d857e3c8cecdcdf1f65f25a4477f98f6f47e4063807f2308")),
      ByteString(Hex.decode("1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347"))
    )

    val blockBodies = Seq(baseBlockBody, baseBlockBody)

    blockchain
      .storeBlockBody(blockBodiesHashes(0), blockBodies(0))
      .and(blockchain.storeBlockBody(blockBodiesHashes(1), blockBodies(1)))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetBlockBodies(blockBodiesHashes), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(BlockBodies(blockBodies), peerId))
  }

  it should "return block headers by block number" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 4)

    blockchain
      .storeBlockHeader(firstHeader)
      .and(blockchain.storeBlockHeader(secondHeader))
      .and(blockchain.storeBlockHeader(baseBlockHeader.copy(number = 5)))
      .and(blockchain.storeBlockHeader(baseBlockHeader.copy(number = 6)))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetBlockHeaders(Left(3), 2, 0, reverse = false), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader)), peerId))
  }

  it should "return block headers by block number when response is shorter then what was requested" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 4)

    blockchain
      .storeBlockHeader(firstHeader)
      .and(blockchain.storeBlockHeader(secondHeader))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetBlockHeaders(Left(3), 3, 0, reverse = false), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader)), peerId))
  }

  it should "return block headers by block number in reverse order" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 2)

    blockchain
      .storeBlockHeader(firstHeader)
      .and(blockchain.storeBlockHeader(secondHeader))
      .and(blockchain.storeBlockHeader(baseBlockHeader.copy(number = 1)))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetBlockHeaders(Left(3), 2, 0, reverse = true), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader)), peerId))
  }

  it should "return block headers by block hash" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 4)

    blockchain
      .storeBlockHeader(firstHeader)
      .and(blockchain.storeBlockHeader(secondHeader))
      .and(blockchain.storeBlockHeader(baseBlockHeader.copy(number = 5)))
      .and(blockchain.storeBlockHeader(baseBlockHeader.copy(number = 6)))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetBlockHeaders(Right(firstHeader.hash), 2, 0, reverse = false), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader)), peerId))
  }

  it should "return block headers by block hash when skipping headers" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 5)

    blockchain
      .storeBlockHeader(firstHeader)
      .and(blockchain.storeBlockHeader(baseBlockHeader.copy(number = 4)))
      .and(blockchain.storeBlockHeader(secondHeader))
      .and(blockchain.storeBlockHeader(baseBlockHeader.copy(number = 6)))
      .and(blockchain.storeBlockHeader(baseBlockHeader.copy(number = 7)))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(
      GetBlockHeaders(Right(firstHeader.hash), maxHeaders = 2, skip = 1, reverse = false),
      peerId
    )

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader)), peerId))
  }

  it should "return block headers in reverse when there are skipped blocks" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 1)

    blockchain
      .storeBlockHeader(firstHeader)
      .and(blockchain.storeBlockHeader(secondHeader))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetBlockHeaders(Right(firstHeader.hash), 2, 1, reverse = true), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader)), peerId))
  }

  it should "return block headers in reverse when there are skipped blocks and we are asking for blocks before genesis" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 3)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 1)

    blockchain
      .storeBlockHeader(firstHeader)
      .and(blockchain.storeBlockHeader(secondHeader))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetBlockHeaders(Right(firstHeader.hash), 3, 1, reverse = true), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader)), peerId))
  }

  it should "return block headers in reverse when there are skipped blocks ending at genesis" in new TestSetup {
    //given
    val firstHeader: BlockHeader = baseBlockHeader.copy(number = 4)
    val secondHeader: BlockHeader = baseBlockHeader.copy(number = 2)

    blockchain
      .storeBlockHeader(firstHeader)
      .and(blockchain.storeBlockHeader(secondHeader))
      .commit()

    //when
    blockchainHost ! MessageFromPeer(GetBlockHeaders(Right(firstHeader.hash), 4, 1, reverse = true), peerId)

    //then
    etcPeerManager.expectMsg(
      EtcPeerManagerActor.SendMessage(BlockHeaders(Seq(firstHeader, secondHeader, blockchain.genesisHeader)), peerId)
    )
  }

  it should "return evm code for hash" in new TestSetup {
    //given
    val fakeEvmCode = ByteString(Hex.decode("ffddaaffddaaffddaaffddaaffddaa"))
    val evmCodeHash: ByteString = ByteString(crypto.kec256(fakeEvmCode.toArray[Byte]))

    blockchain.storeEvmCode(evmCodeHash, fakeEvmCode).commit()

    //when
    blockchainHost ! MessageFromPeer(GetNodeData(Seq(evmCodeHash)), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(NodeData(Seq(fakeEvmCode)), peerId))
  }

  it should "return mptNode for hash" in new TestSetup {
    //given
    val exampleNibbles = ByteString(HexPrefix.bytesToNibbles(Hex.decode("ffddaa")))
    val exampleHash = ByteString(Hex.decode("ab" * 32))
    val extensionNode: MptNode = ExtensionNode(exampleNibbles, HashNode(exampleHash.toArray[Byte]))

    storagesInstance.storages.stateStorage.saveNode(
      ByteString(extensionNode.hash),
      extensionNode.toBytes: Array[Byte],
      0
    )

    //when
    blockchainHost ! MessageFromPeer(GetNodeData(Seq(ByteString(extensionNode.hash))), peerId)

    //then
    etcPeerManager.expectMsg(EtcPeerManagerActor.SendMessage(NodeData(Seq(extensionNode.toBytes)), peerId))
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    override implicit lazy val system = ActorSystem("BlockchainHostActor_System")

    blockchain.storeBlockHeader(Fixtures.Blocks.Genesis.header).commit()

    val peerConf = new PeerConfiguration {
      override val fastSyncHostConfiguration: FastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = 200
        val maxBlocksBodiesPerMessage: Int = 200
        val maxReceiptsPerMessage: Int = 200
        val maxMptComponentsPerMessage: Int = 200
      }
      override val rlpxConfiguration: RLPxConfiguration = new RLPxConfiguration {
        override val waitForTcpAckTimeout: FiniteDuration = Timeouts.normalTimeout
        override val waitForHandshakeTimeout: FiniteDuration = Timeouts.normalTimeout
      }
      override val waitForHelloTimeout: FiniteDuration = 30 seconds
      override val waitForStatusTimeout: FiniteDuration = 30 seconds
      override val waitForChainCheckTimeout: FiniteDuration = 15 seconds
      override val connectMaxRetries: Int = 3
      override val connectRetryDelay: FiniteDuration = 1 second
      override val disconnectPoisonPillTimeout: FiniteDuration = 5 seconds
      override val minOutgoingPeers = 5
      override val maxOutgoingPeers = 10
      override val maxIncomingPeers = 5
      override val maxPendingPeers = 5
      override val pruneIncomingPeers = 0
      override val minPruneAge = 1.minute
      override val networkId: Int = 1

      override val updateNodesInitialDelay: FiniteDuration = 5.seconds
      override val updateNodesInterval: FiniteDuration = 20.seconds
      override val shortBlacklistDuration: FiniteDuration = 1.minute
      override val longBlacklistDuration: FiniteDuration = 3.minutes
      override val statSlotDuration: FiniteDuration = 1.minute
      override val statSlotCount: Int = 30
    }

    val baseBlockHeader = Fixtures.Blocks.Block3125369.header
    val baseBlockBody = BlockBody(Nil, Nil)

    val peerId = PeerId("1")

    val peerEventBus = TestProbe()
    val etcPeerManager = TestProbe()

    val blockchainHost = TestActorRef(
      Props(
        new BlockchainHostActor(
          blockchain,
          storagesInstance.storages.evmCodeStorage,
          peerConf,
          peerEventBus.ref,
          etcPeerManager.ref
        )
      )
    )
  }

}
