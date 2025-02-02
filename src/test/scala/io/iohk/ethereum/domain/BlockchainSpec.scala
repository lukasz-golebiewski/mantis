package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1097
import io.iohk.ethereum.mpt.{HashNode, MerklePatriciaTrie}
import io.iohk.ethereum.{BlockHelpers, Fixtures, ObjectGenerators}
import io.iohk.ethereum.ObjectGenerators._
import io.iohk.ethereum.proof.MptProofVerifier
import io.iohk.ethereum.proof.ProofVerifyResult.ValidProof
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BlockchainSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {

  val checkpoint = ObjectGenerators.fakeCheckpointGen(2, 5).sample.get
  val checkpointBlockGenerator = new CheckpointBlockGenerator

  "Blockchain" should "be able to store a block and return it if queried by hash" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.storeBlock(validBlock).commit()
    val block = blockchain.getBlockByHash(validBlock.header.hash)
    block.isDefined should ===(true)
    validBlock should ===(block.get)
    val blockHeader = blockchain.getBlockHeaderByHash(validBlock.header.hash)
    blockHeader.isDefined should ===(true)
    validBlock.header should ===(blockHeader.get)
    val blockBody = blockchain.getBlockBodyByHash(validBlock.header.hash)
    blockBody.isDefined should ===(true)
    validBlock.body should ===(blockBody.get)
  }

  it should "be able to store a block and retrieve it by number" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.storeBlock(validBlock).commit()
    val block = blockchain.getBlockByNumber(validBlock.header.number)
    block.isDefined should ===(true)
    validBlock should ===(block.get)
  }

  it should "be able to do strict check of block existence in the chain" in new EphemBlockchainTestSetup {
    val validBlock = Fixtures.Blocks.ValidBlock.block
    blockchain.save(validBlock, Seq.empty, ChainWeight(100, 100), saveAsBestBlock = true)
    blockchain.isInChain(validBlock.hash) === (false)
    // simulation of node restart
    blockchain.saveBestKnownBlocks(validBlock.header.number - 1)
    blockchain.isInChain(validBlock.hash) should ===(false)
  }

  it should "be able to query a stored blockHeader by it's number" in new EphemBlockchainTestSetup {
    val validHeader = Fixtures.Blocks.ValidBlock.header
    blockchain.storeBlockHeader(validHeader).commit()
    val header = blockchain.getBlockHeaderByNumber(validHeader.number)
    header.isDefined should ===(true)
    validHeader should ===(header.get)
  }

  it should "not return a value if not stored" in new EphemBlockchainTestSetup {
    blockchain.getBlockByNumber(Fixtures.Blocks.ValidBlock.header.number).isEmpty should ===(true)
    blockchain.getBlockByHash(Fixtures.Blocks.ValidBlock.header.hash).isEmpty should ===(true)
  }

  it should "be able to store a block with checkpoint and retrieve it and checkpoint" in new EphemBlockchainTestSetup {
    val parent = Fixtures.Blocks.Genesis.block
    blockchain.storeBlock(parent)

    val validBlock = new CheckpointBlockGenerator().generate(parent, checkpoint)

    blockchain.save(validBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    val retrievedBlock = blockchain.getBlockByHash(validBlock.header.hash)
    retrievedBlock.isDefined should ===(true)
    validBlock should ===(retrievedBlock.get)

    blockchain.getLatestCheckpointBlockNumber() should ===(validBlock.number)
    blockchain.getBestBlockNumber() should ===(validBlock.number)
  }

  it should "be able to rollback block with checkpoint and store the previous existed checkpoint" in new EphemBlockchainTestSetup {
    val genesis = Fixtures.Blocks.Genesis.block
    blockchain.storeBlock(genesis)

    def nextBlock(parent: Block, body: BlockBody = BlockBody.empty): Block =
      Block(
        header = parent.header.copy(
          number = parent.number + 1,
          parentHash = parent.hash,
          extraFields = HefPostEcip1097(None)
        ),
        body = body
      )

    val firstBlock = checkpointBlockGenerator.generate(genesis, checkpoint) // Older checkpoint
    val secondBlock = nextBlock(firstBlock)
    val thirdBlock = checkpointBlockGenerator.generate(secondBlock, checkpoint)

    blockchain.save(firstBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)
    blockchain.save(secondBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)
    blockchain.save(thirdBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    blockchain.removeBlock(thirdBlock.hash, withState = true)

    blockchain.getLatestCheckpointBlockNumber() should ===(firstBlock.number)
    blockchain.getBestBlockNumber() should ===(secondBlock.number)
  }

  it should "be able to rollback block with last checkpoint in the chain" in new EphemBlockchainTestSetup {
    val genesis = Fixtures.Blocks.Genesis.block
    blockchain.storeBlock(genesis)

    val validBlock = checkpointBlockGenerator.generate(genesis, checkpoint)

    blockchain.save(validBlock, Seq.empty, ChainWeight(0, 0), saveAsBestBlock = true)

    blockchain.removeBlock(validBlock.hash, withState = true)

    blockchain.getLatestCheckpointBlockNumber() should ===(genesis.number)
    blockchain.getBestBlockNumber() should ===(genesis.number)
  }

  it should "return an account given an address and a block number" in new EphemBlockchainTestSetup {
    val address = Address(42)
    val account = Account.empty(UInt256(7))

    val validHeader = Fixtures.Blocks.ValidBlock.header

    val stateStorage = StateStorage.createTestStateStorage(EphemDataSource())._1
    val emptyMpt = MerklePatriciaTrie[Address, Account](
      storagesInstance.storages.stateStorage.getBackingStorage(0)
    )
    val mptWithAcc = emptyMpt.put(address, account)
    val headerWithAcc = validHeader.copy(stateRoot = ByteString(mptWithAcc.getRootHash))

    blockchain.storeBlockHeader(headerWithAcc).commit()

    val retrievedAccount = blockchain.getAccount(address, headerWithAcc.number)
    retrievedAccount shouldEqual Some(account)
  }

  it should "return correct account proof" in new EphemBlockchainTestSetup {
    val address = Address(42)
    val account = Account.empty(UInt256(7))

    val validHeader = Fixtures.Blocks.ValidBlock.header

    val emptyMpt = MerklePatriciaTrie[Address, Account](
      storagesInstance.storages.stateStorage.getBackingStorage(0)
    )
    val mptWithAcc = emptyMpt.put(address, account)

    val headerWithAcc = validHeader.copy(stateRoot = ByteString(mptWithAcc.getRootHash))

    blockchain.storeBlockHeader(headerWithAcc).commit()

    //unhappy path
    val wrongAddress = Address(666)
    val retrievedAccountProofWrong = blockchain.getAccountProof(wrongAddress, headerWithAcc.number)
    //the account doesn't exist, so we can't retrieve it, but we do receive a proof of non-existence with a full path of nodes that we iterated
    retrievedAccountProofWrong.isDefined shouldBe true
    retrievedAccountProofWrong.size shouldBe 1
    mptWithAcc.get(wrongAddress) shouldBe None

    //happy path
    val retrievedAccountProof = blockchain.getAccountProof(address, headerWithAcc.number)
    retrievedAccountProof.isDefined shouldBe true
    retrievedAccountProof.map { proof =>
      MptProofVerifier.verifyProof(mptWithAcc.getRootHash, address, proof) shouldBe ValidProof
    }
  }

  it should "return proof for non-existent account" in new EphemBlockchainTestSetup {
    val emptyMpt = MerklePatriciaTrie[Address, Account](
      storagesInstance.storages.stateStorage.getBackingStorage(0)
    )
    val mptWithAcc = emptyMpt.put(Address(42), Account.empty(UInt256(7)))

    val headerWithAcc = Fixtures.Blocks.ValidBlock.header.copy(stateRoot = ByteString(mptWithAcc.getRootHash))

    blockchain.storeBlockHeader(headerWithAcc).commit()

    val wrongAddress = Address(666)
    val retrievedAccountProofWrong = blockchain.getAccountProof(wrongAddress, headerWithAcc.number)
    //the account doesn't exist, so we can't retrieve it, but we do receive a proof of non-existence with a full path of nodes(root node) that we iterated
    (retrievedAccountProofWrong.getOrElse(Vector.empty).toList match {
      case _ @HashNode(_) :: Nil => true
      case _ => false
    }) shouldBe true
    mptWithAcc.get(wrongAddress) shouldBe None
  }

  it should "return correct best block number after applying and rollbacking blocks" in new TestSetup {
    forAll(intGen(min = 1: Int, max = maxNumberBlocksToImport)) { numberBlocksToImport =>
      val testSetup = newSetup()
      import testSetup._

      // Import blocks
      val blocksToImport = BlockHelpers.generateChain(numberBlocksToImport, Fixtures.Blocks.Genesis.block)

      // Randomly select the block import to persist (empty means no persistance)
      val blockImportToPersist = Gen.option(Gen.oneOf(blocksToImport)).sample.get
      (stubStateStorage
        .onBlockSave(_: BigInt, _: BigInt)(_: () => Unit))
        .when(*, *, *)
        .onCall((bn, _, persistFn) => {
          if (blockImportToPersist.exists(_.number == bn)) persistFn()
        })

      blocksToImport.foreach { block =>
        blockchainWithStubPersisting.save(block, Nil, ChainWeight.zero, true)
      }

      blockchainWithStubPersisting.getBestBlockNumber() shouldBe blocksToImport.last.number
      blockchainStoragesWithStubPersisting.appStateStorage.getBestBlockNumber() shouldBe blockImportToPersist.fold(
        0: BigInt
      )(_.number)

      // Rollback blocks
      val numberBlocksToRollback = intGen(0, numberBlocksToImport).sample.get
      val (blocksNotRollbacked, blocksToRollback) = blocksToImport.splitAt(numberBlocksToRollback)

      // Randomly select the block rollback to persist (empty means no persistance)
      val blockRollbackToPersist =
        if (blocksToRollback.isEmpty) None else Gen.option(Gen.oneOf(blocksToRollback)).sample.get
      (stubStateStorage
        .onBlockRollback(_: BigInt, _: BigInt)(_: () => Unit))
        .when(*, *, *)
        .onCall((bn, _, persistFn) => {
          if (blockRollbackToPersist.exists(_.number == bn)) persistFn()
        })

      blocksToRollback.reverse.foreach { block =>
        blockchainWithStubPersisting.removeBlock(block.hash, true)
      }

      val expectedMemoryBestBlock = blocksNotRollbacked.lastOption.fold(0: BigInt)(_.number)
      val expectedPersistedBestBlock = calculatePersistedBestBlock(
        blockImportToPersist.map(_.number),
        blockRollbackToPersist.map(_.number),
        blocksToRollback.map(_.number)
      )
      blockchainWithStubPersisting.getBestBlockNumber() shouldBe expectedMemoryBestBlock
      blockchainStoragesWithStubPersisting.appStateStorage.getBestBlockNumber() shouldBe expectedPersistedBestBlock
    }
  }

  trait TestSetup extends MockFactory {
    val maxNumberBlocksToImport: Int = 30

    def calculatePersistedBestBlock(
        blockImportPersisted: Option[BigInt],
        blockRollbackPersisted: Option[BigInt],
        blocksRollbacked: Seq[BigInt]
    ): BigInt = {
      (blocksRollbacked, blockImportPersisted) match {
        case (Nil, Some(bi)) =>
          // No blocks rollbacked, last persist was the persist during import
          bi
        case (nonEmptyRollbackedBlocks, Some(bi)) =>
          // Last forced persist during apply/rollback
          val maxForcedPersist = blockRollbackPersisted.fold(bi) { br => (br - 1).max(bi) }

          // The above number would have been decreased by any rollbacked blocks
          (nonEmptyRollbackedBlocks.head - 1).min(maxForcedPersist)
        case (_, None) =>
          // If persisted rollback, then it  was decreased by the future rollbacks, if not no persistance was ever done
          blockRollbackPersisted.fold(0: BigInt)(_ => blocksRollbacked.head - 1)
      }
    }

    trait StubPersistingBlockchainSetup {
      def stubStateStorage: StateStorage
      def blockchainStoragesWithStubPersisting: BlockchainStorages
      def blockchainWithStubPersisting: BlockchainImpl
    }

    def newSetup(): StubPersistingBlockchainSetup = {
      new StubPersistingBlockchainSetup with EphemBlockchainTestSetup {
        override val stubStateStorage = stub[StateStorage]
        override val blockchainStoragesWithStubPersisting = new BlockchainStorages {
          val blockHeadersStorage = storagesInstance.storages.blockHeadersStorage
          val blockBodiesStorage = storagesInstance.storages.blockBodiesStorage
          val blockNumberMappingStorage = storagesInstance.storages.blockNumberMappingStorage
          val receiptStorage = storagesInstance.storages.receiptStorage
          val evmCodeStorage = storagesInstance.storages.evmCodeStorage
          val chainWeightStorage = storagesInstance.storages.chainWeightStorage
          val transactionMappingStorage = storagesInstance.storages.transactionMappingStorage
          val nodeStorage = storagesInstance.storages.nodeStorage
          val pruningMode = storagesInstance.storages.pruningMode
          val appStateStorage = storagesInstance.storages.appStateStorage
          val cachedNodeStorage = storagesInstance.storages.cachedNodeStorage
          val stateStorage = stubStateStorage
        }
        override val blockchainWithStubPersisting = BlockchainImpl(blockchainStoragesWithStubPersisting)

        blockchainWithStubPersisting.storeBlock(Fixtures.Blocks.Genesis.block)
      }
    }

  }
}
