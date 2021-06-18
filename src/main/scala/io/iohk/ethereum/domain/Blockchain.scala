package io.iohk.ethereum.domain

import java.util.concurrent.atomic.AtomicReference
import akka.util.ByteString
import cats.syntax.flatMap._
import cats.instances.option._
import io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate
import io.iohk.ethereum.db.dataSource.RocksDbDataSource.IterationError
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain
import io.iohk.ethereum.domain.BlockchainImpl.BestBlockLatestCheckpointNumbers
import io.iohk.ethereum.jsonrpc.ProofService.StorageProof
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, MptNode}
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import io.iohk.ethereum.vm.{Storage, WorldStateProxy}
import monix.reactive.Observable

import scala.annotation.tailrec

/**
  * Entity to be used to persist and query  Blockchain related objects (blocks, transactions, ommers)
  */
// scalastyle:off number.of.methods
trait Blockchain {

  type S <: Storage[S]
  type WS <: WorldStateProxy[WS, S]

  /**
    * Allows to query a blockHeader by block hash
    *
    * @param hash of the block that's being searched
    * @return [[BlockHeader]] if found
    */
  def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader]

  def getBlockHeaderByNumber(number: BigInt): Option[BlockHeader] = {
    for {
      hash <- getHashByBlockNumber(number)
      header <- getBlockHeaderByHash(hash)
    } yield header
  }

  /**
    * Allows to query a blockBody by block hash
    *
    * @param hash of the block that's being searched
    * @return [[io.iohk.ethereum.domain.BlockBody]] if found
    */
  def getBlockBodyByHash(hash: ByteString): Option[BlockBody]

  /**
    * Allows to query for a block based on it's hash
    *
    * @param hash of the block that's being searched
    * @return Block if found
    */
  def getBlockByHash(hash: ByteString): Option[Block] =
    for {
      header <- getBlockHeaderByHash(hash)
      body <- getBlockBodyByHash(hash)
    } yield Block(header, body)

  /**
    * Allows to query for a block based on it's number
    *
    * @param number Block number
    * @return Block if it exists
    */
  def getBlockByNumber(number: BigInt): Option[Block] =
    for {
      hash <- getHashByBlockNumber(number)
      block <- getBlockByHash(hash)
    } yield block

  /**
    * Get an account for an address and a block number
    *
    * @param address address of the account
    * @param blockNumber the block that determines the state of the account
    */
  def getAccount(address: Address, blockNumber: BigInt): Option[Account]

  def getAccountProof(address: Address, blockNumber: BigInt): Option[Vector[MptNode]]

  /**
    * Get account storage at given position
    *
    * @param rootHash storage root hash
    * @param position storage position
    */
  def getAccountStorageAt(rootHash: ByteString, position: BigInt, ethCompatibleStorage: Boolean): ByteString

  /**
    * Get a storage-value and its proof being the path from the root node until the last matching node.
    *
    * @param rootHash storage root hash
    * @param position storage position
    */
  def getStorageProofAt(
      rootHash: ByteString,
      position: BigInt,
      ethCompatibleStorage: Boolean
  ): StorageProof

  /**
    * Returns the receipts based on a block hash
    * @param blockhash
    * @return Receipts if found
    */
  def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]]

  /**
    * Returns EVM code searched by it's hash
    * @param hash Code Hash
    * @return EVM code if found
    */
  def getEvmCodeByHash(hash: ByteString): Option[ByteString]

  /**
    * Returns MPT node searched by it's hash
    * @param hash Node Hash
    * @return MPT node
    */
  def getMptNodeByHash(hash: ByteString): Option[MptNode]

  /**
    * Looks up ChainWeight for a given chain
    * @param blockhash Hash of top block in the chain
    * @return ChainWeight if found
    */
  def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight]

  def getChainWeightByNumber(blockNumber: BigInt): Option[ChainWeight] =
    getHashByBlockNumber(blockNumber).flatMap(getChainWeightByHash)

  def getBestBlockNumber(): BigInt

  def getBestBlock(): Option[Block]

  def getLatestCheckpointBlockNumber(): BigInt

  /**
    * Persists full block along with receipts and chain weight
    * @param saveAsBestBlock - whether to save the block's number as current best block
    */
  def save(block: Block, receipts: Seq[Receipt], chainWeight: ChainWeight, saveAsBestBlock: Boolean): Unit

  /**
    * Persists a block in the underlying Blockchain Database
    * Note: all store* do not update the database immediately, rather they create
    * a [[io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate]] which then has to be committed (atomic operation)
    *
    * @param block Block to be saved
    */
  def storeBlock(block: Block): DataSourceBatchUpdate = {
    storeBlockHeader(block.header).and(storeBlockBody(block.header.hash, block.body))
  }

  def removeBlock(hash: ByteString, withState: Boolean): Unit

  /**
    * Persists a block header in the underlying Blockchain Database
    *
    * @param blockHeader Block to be saved
    */
  def storeBlockHeader(blockHeader: BlockHeader): DataSourceBatchUpdate

  def storeBlockBody(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate

  def storeReceipts(blockHash: ByteString, receipts: Seq[Receipt]): DataSourceBatchUpdate

  def storeEvmCode(hash: ByteString, evmCode: ByteString): DataSourceBatchUpdate

  def storeChainWeight(blockhash: ByteString, weight: ChainWeight): DataSourceBatchUpdate

  def saveBestKnownBlocks(bestBlockNumber: BigInt, latestCheckpointNumber: Option[BigInt] = None): Unit

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit

  /**
    * Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  protected def getHashByBlockNumber(number: BigInt): Option[ByteString]

  def genesisHeader: BlockHeader = getBlockHeaderByNumber(0).get

  def genesisBlock: Block = getBlockByNumber(0).get

  def getWorldStateProxy(
      blockNumber: BigInt,
      accountStartNonce: UInt256,
      stateRootHash: ByteString,
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): WS

  def getReadOnlyWorldStateProxy(
      blockNumber: Option[BigInt],
      accountStartNonce: UInt256,
      stateRootHash: ByteString,
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): WS

  def getStateStorage: StateStorage

  def mptStateSavedKeys(): Observable[Either[IterationError, ByteString]]

  /**
    * Strict check if given block hash is in chain
    * Using any of getXXXByHash is not always accurate - after restart the best block is often lower than before restart
    * The result of that is returning data of blocks which we don't consider as a part of the chain anymore
    * @param hash block hash
    */
  def isInChain(hash: ByteString): Boolean = {
    (for {
      header <- getBlockHeaderByHash(hash) if header.number <= getBestBlockNumber()
      hash <- getHashByBlockNumber(header.number)
    } yield header.hash == hash).getOrElse(false)
  }
}
// scalastyle:on

class BlockchainImpl(
    protected val blockHeadersStorage: BlockHeadersStorage,
    protected val blockBodiesStorage: BlockBodiesStorage,
    protected val blockNumberMappingStorage: BlockNumberMappingStorage,
    protected val receiptStorage: ReceiptStorage,
    protected val evmCodeStorage: EvmCodeStorage,
    protected val nodeStorage: NodeStorage,
    protected val chainWeightStorage: ChainWeightStorage,
    protected val transactionMappingStorage: TransactionMappingStorage,
    protected val appStateStorage: AppStateStorage,
    protected val stateStorage: StateStorage
) extends Blockchain
    with Logger {

  override def getStateStorage: StateStorage = stateStorage

  // There is always only one writer thread (ensured by actor), but can by many readers (api calls)
  // to ensure visibility of writes, needs to be volatile or atomic ref
  // Laziness required for mocking BlockchainImpl on tests
  private lazy val bestKnownBlockAndLatestCheckpoint: AtomicReference[BestBlockLatestCheckpointNumbers] =
    new AtomicReference(
      BestBlockLatestCheckpointNumbers(
        appStateStorage.getBestBlockNumber(),
        appStateStorage.getLatestCheckpointBlockNumber()
      )
    )

  override def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] =
    blockHeadersStorage.get(hash)

  override def getBlockBodyByHash(hash: ByteString): Option[BlockBody] =
    blockBodiesStorage.get(hash)

  override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = receiptStorage.get(blockhash)

  override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = evmCodeStorage.get(hash)

  override def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight] = chainWeightStorage.get(blockhash)

  override def getBestBlockNumber(): BigInt = {
    val bestSavedBlockNumber = appStateStorage.getBestBlockNumber()
    val bestKnownBlockNumber = bestKnownBlockAndLatestCheckpoint.get().bestBlockNumber
    log.debug(
      "Current best saved block number {}. Current best known block number {}",
      bestSavedBlockNumber,
      bestKnownBlockNumber
    )

    // The cached best block number should always be more up-to-date than the one on disk, we are keeping access to disk
    // above only for logging purposes
    bestKnownBlockNumber
  }

  override def getLatestCheckpointBlockNumber(): BigInt =
    bestKnownBlockAndLatestCheckpoint.get().latestCheckpointNumber

  //returns the best known block if it's available in the storage, otherwise the best stored block
  override def getBestBlock(): Option[Block] = {
    val bestBlockNumber = getBestBlockNumber()
    log.debug("Trying to get best block with number {}", bestBlockNumber)
    getBlockByNumber(bestBlockNumber) orElse getBlockByNumber(appStateStorage.getBestBlockNumber())
  }

  override def getAccount(address: Address, blockNumber: BigInt): Option[Account] =
    getAccountMpt(blockNumber) >>= (_.get(address))

  override def getAccountProof(address: Address, blockNumber: BigInt): Option[Vector[MptNode]] =
    getAccountMpt(blockNumber) >>= (_.getProof(address))

  private def getAccountMpt(blockNumber: BigInt): Option[MerklePatriciaTrie[Address, Account]] =
    getBlockHeaderByNumber(blockNumber).map { bh =>
      val storage = stateStorage.getBackingStorage(blockNumber)
      MerklePatriciaTrie[Address, Account](
        rootHash = bh.stateRoot.toArray,
        source = storage
      )
    }

  override def getAccountStorageAt(
      rootHash: ByteString,
      position: BigInt,
      ethCompatibleStorage: Boolean
  ): ByteString = {
    val storage = stateStorage.getBackingStorage(0)
    val mpt =
      if (ethCompatibleStorage) domain.EthereumUInt256Mpt.storageMpt(rootHash, storage)
      else domain.ArbitraryIntegerMpt.storageMpt(rootHash, storage)

    val bigIntValue = mpt.get(position).getOrElse(BigInt(0))
    val byteArrayValue = bigIntValue.toByteArray

    // BigInt.toArray actually might return one more byte than necessary because it adds a sign bit, which in our case
    // will always be 0. This would add unwanted 0 bytes and might cause the value to be 33 byte long while an EVM
    // word is 32 byte long.
    if (bigIntValue != 0)
      ByteString(byteArrayValue.dropWhile(_ == 0))
    else
      ByteString(byteArrayValue)
  }

  override def getStorageProofAt(
      rootHash: ByteString,
      position: BigInt,
      ethCompatibleStorage: Boolean
  ): StorageProof = {
    val storage: MptStorage = stateStorage.getBackingStorage(0)
    val mpt: MerklePatriciaTrie[BigInt, BigInt] = {
      if (ethCompatibleStorage) domain.EthereumUInt256Mpt.storageMpt(rootHash, storage)
      else domain.ArbitraryIntegerMpt.storageMpt(rootHash, storage)
    }
    val value: Option[BigInt] = mpt.get(position)
    val proof: Option[Vector[MptNode]] = mpt.getProof(position)
    StorageProof(position, value, proof)
  }

  private def persistBestBlocksData(): Unit = {
    val currentBestBlockNumber = getBestBlockNumber()
    val currentBestCheckpointNumber = getLatestCheckpointBlockNumber()
    log.debug(
      "Persisting app info data into database. Persisted block number is {}. " +
        "Persisted checkpoint number is {}",
      currentBestBlockNumber,
      currentBestCheckpointNumber
    )

    appStateStorage
      .putBestBlockNumber(currentBestBlockNumber)
      .and(appStateStorage.putLatestCheckpointBlockNumber(currentBestCheckpointNumber))
      .commit()
  }

  def save(block: Block, receipts: Seq[Receipt], weight: ChainWeight, saveAsBestBlock: Boolean): Unit = {
    if (saveAsBestBlock && block.hasCheckpoint) {
      log.debug(
        "New best known block block number - {}, new best checkpoint number - {}",
        block.header.number,
        block.header.number
      )
      saveBestKnownBlockAndLatestCheckpointNumber(block.header.number, block.header.number)
    } else if (saveAsBestBlock) {
      log.debug(
        "New best known block block number - {}",
        block.header.number
      )
      saveBestKnownBlock(block.header.number)
    }

    log.debug("Saving new block block {} to database", block.idTag)
    storeBlock(block)
      .and(storeReceipts(block.header.hash, receipts))
      .and(storeChainWeight(block.header.hash, weight))
      .commit()

    // not transactional part
    // the best blocks data will be persisted only when the cache will be persisted
    stateStorage.onBlockSave(block.header.number, appStateStorage.getBestBlockNumber())(persistBestBlocksData)
  }

  override def storeBlockHeader(blockHeader: BlockHeader): DataSourceBatchUpdate = {
    val hash = blockHeader.hash
    blockHeadersStorage.put(hash, blockHeader).and(saveBlockNumberMapping(blockHeader.number, hash))
  }

  override def getMptNodeByHash(hash: ByteString): Option[MptNode] =
    stateStorage.getNode(hash)

  override def storeBlockBody(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate = {
    blockBodiesStorage.put(blockHash, blockBody).and(saveTxsLocations(blockHash, blockBody))
  }

  override def storeReceipts(blockHash: ByteString, receipts: Seq[Receipt]): DataSourceBatchUpdate =
    receiptStorage.put(blockHash, receipts)

  override def storeEvmCode(hash: ByteString, evmCode: ByteString): DataSourceBatchUpdate =
    evmCodeStorage.put(hash, evmCode)

  override def saveBestKnownBlocks(bestBlockNumber: BigInt, latestCheckpointNumber: Option[BigInt] = None): Unit = {
    latestCheckpointNumber match {
      case Some(number) =>
        saveBestKnownBlockAndLatestCheckpointNumber(bestBlockNumber, number)
      case None =>
        saveBestKnownBlock(bestBlockNumber)
    }
  }

  private def saveBestKnownBlock(bestBlockNumber: BigInt): Unit =
    bestKnownBlockAndLatestCheckpoint.updateAndGet(_.copy(bestBlockNumber = bestBlockNumber))

  private def saveBestKnownBlockAndLatestCheckpointNumber(number: BigInt, latestCheckpointNumber: BigInt): Unit =
    bestKnownBlockAndLatestCheckpoint.set(BestBlockLatestCheckpointNumbers(number, latestCheckpointNumber))

  def storeChainWeight(blockhash: ByteString, weight: ChainWeight): DataSourceBatchUpdate =
    chainWeightStorage.put(blockhash, weight)

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit =
    stateStorage.saveNode(nodeHash, nodeEncoded, blockNumber)

  override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    blockNumberMappingStorage.get(number)

  private def saveBlockNumberMapping(number: BigInt, hash: ByteString): DataSourceBatchUpdate =
    blockNumberMappingStorage.put(number, hash)

  private def removeBlockNumberMapping(number: BigInt): DataSourceBatchUpdate = {
    blockNumberMappingStorage.remove(number)
  }

  override def removeBlock(blockHash: ByteString, withState: Boolean): Unit = {
    val maybeBlock = getBlockByHash(blockHash)

    maybeBlock match {
      case Some(block) => removeBlock(block, withState)
      case None =>
        log.warn(s"Attempted removing block with hash ${ByteStringUtils.hash2string(blockHash)} that we don't have")
    }
  }

  // scalastyle:off method.length
  private def removeBlock(block: Block, withState: Boolean): Unit = {
    val blockHash = block.hash

    log.debug(s"Trying to remove block ${block.idTag}")

    val txList = block.body.transactionList
    val bestBlockNumber = getBestBlockNumber()
    val latestCheckpointNumber = getLatestCheckpointBlockNumber()

    val blockNumberMappingUpdates =
      if (getHashByBlockNumber(block.number).contains(blockHash))
        removeBlockNumberMapping(block.number)
      else blockNumberMappingStorage.emptyBatchUpdate

    val newBestBlockNumber: BigInt = (bestBlockNumber - 1).max(0)
    val newLatestCheckpointNumber: BigInt =
      if (block.hasCheckpoint && block.number == latestCheckpointNumber) {
        findPreviousCheckpointBlockNumber(block.number, block.number)
      } else latestCheckpointNumber

    /*
      This two below updates are an exception to the rule of only updating the best blocks when persisting the node
      cache.
      They are required in case we are removing a block that's marked on db as the best (or as the last checkpoint),
      to keep it's consistency, as it will no longer be the best block (nor the last checkpoint).

      This updates can't be done if the conditions are false as we might not have the associated mpt nodes, so falling
      into the case of having an incomplete best block and so an inconsistent db
     */
    val bestBlockNumberUpdates =
      if (appStateStorage.getBestBlockNumber() > newBestBlockNumber)
        appStateStorage.putBestBlockNumber(newBestBlockNumber)
      else appStateStorage.emptyBatchUpdate
    val latestCheckpointNumberUpdates =
      if (appStateStorage.getLatestCheckpointBlockNumber() > newLatestCheckpointNumber)
        appStateStorage.putLatestCheckpointBlockNumber(newLatestCheckpointNumber)
      else appStateStorage.emptyBatchUpdate

    log.debug(
      "Persisting app info data into database. Persisted block number is {}. Persisted checkpoint number is {}",
      newBestBlockNumber,
      newLatestCheckpointNumber
    )

    blockHeadersStorage
      .remove(blockHash)
      .and(blockBodiesStorage.remove(blockHash))
      .and(chainWeightStorage.remove(blockHash))
      .and(receiptStorage.remove(blockHash))
      .and(removeTxsLocations(txList))
      .and(blockNumberMappingUpdates)
      .and(bestBlockNumberUpdates)
      .and(latestCheckpointNumberUpdates)
      .commit()

    saveBestKnownBlocks(newBestBlockNumber, Some(newLatestCheckpointNumber))
    log.debug(
      "Removed block with hash {}. New best block number - {}, new best checkpoint block number - {}",
      ByteStringUtils.hash2string(blockHash),
      newBestBlockNumber,
      newLatestCheckpointNumber
    )

    // not transactional part
    if (withState)
      stateStorage.onBlockRollback(block.number, bestBlockNumber) { () => persistBestBlocksData() }
  }
  // scalastyle:on method.length

  def mptStateSavedKeys(): Observable[Either[IterationError, ByteString]] = {
    (nodeStorage.storageContent.map(c => c.map(_._1)) ++ evmCodeStorage.storageContent.map(c => c.map(_._1)))
      .takeWhileInclusive(_.isRight)
  }

  /**
    * Recursive function which try to find the previous checkpoint by traversing blocks from top to the bottom.
    * In case of finding the checkpoint block number, the function will finish the job and return result
    */
  @tailrec
  private def findPreviousCheckpointBlockNumber(
      blockNumberToCheck: BigInt,
      latestCheckpointBlockNumber: BigInt
  ): BigInt = {
    if (blockNumberToCheck > 0) {
      val maybePreviousCheckpointBlockNumber = for {
        currentBlock <- getBlockByNumber(blockNumberToCheck)
        if currentBlock.hasCheckpoint &&
          currentBlock.number < latestCheckpointBlockNumber
      } yield currentBlock.number

      maybePreviousCheckpointBlockNumber match {
        case Some(previousCheckpointBlockNumber) => previousCheckpointBlockNumber
        case None => findPreviousCheckpointBlockNumber(blockNumberToCheck - 1, latestCheckpointBlockNumber)
      }
    } else 0
  }

  private def saveTxsLocations(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate =
    blockBody.transactionList.zipWithIndex.foldLeft(transactionMappingStorage.emptyBatchUpdate) {
      case (updates, (tx, index)) =>
        updates.and(transactionMappingStorage.put(tx.hash, TransactionLocation(blockHash, index)))
    }

  private def removeTxsLocations(stxs: Seq[SignedTransaction]): DataSourceBatchUpdate = {
    stxs.map(_.hash).foldLeft(transactionMappingStorage.emptyBatchUpdate) { case (updates, hash) =>
      updates.and(transactionMappingStorage.remove(hash))
    }
  }

  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy

  override def getWorldStateProxy(
      blockNumber: BigInt,
      accountStartNonce: UInt256,
      stateRootHash: ByteString,
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      stateStorage.getBackingStorage(blockNumber),
      accountStartNonce,
      (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
      stateRootHash,
      noEmptyAccounts,
      ethCompatibleStorage
    )

  //FIXME Maybe we can use this one in regular execution too and persist underlying storage when block execution is successful
  override def getReadOnlyWorldStateProxy(
      blockNumber: Option[BigInt],
      accountStartNonce: UInt256,
      stateRootHash: ByteString,
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      stateStorage.getReadOnlyStorage,
      accountStartNonce,
      (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
      stateRootHash,
      noEmptyAccounts = noEmptyAccounts,
      ethCompatibleStorage = ethCompatibleStorage
    )
}

trait BlockchainStorages {
  val blockHeadersStorage: BlockHeadersStorage
  val blockBodiesStorage: BlockBodiesStorage
  val blockNumberMappingStorage: BlockNumberMappingStorage
  val receiptStorage: ReceiptStorage
  val evmCodeStorage: EvmCodeStorage
  val chainWeightStorage: ChainWeightStorage
  val transactionMappingStorage: TransactionMappingStorage
  val nodeStorage: NodeStorage
  val pruningMode: PruningMode
  val appStateStorage: AppStateStorage
  val cachedNodeStorage: CachedNodeStorage
  val stateStorage: StateStorage
}

object BlockchainImpl {
  def apply(storages: BlockchainStorages): BlockchainImpl =
    new BlockchainImpl(
      blockHeadersStorage = storages.blockHeadersStorage,
      blockBodiesStorage = storages.blockBodiesStorage,
      blockNumberMappingStorage = storages.blockNumberMappingStorage,
      receiptStorage = storages.receiptStorage,
      evmCodeStorage = storages.evmCodeStorage,
      nodeStorage = storages.nodeStorage,
      chainWeightStorage = storages.chainWeightStorage,
      transactionMappingStorage = storages.transactionMappingStorage,
      appStateStorage = storages.appStateStorage,
      stateStorage = storages.stateStorage
    )

  private case class BestBlockLatestCheckpointNumbers(bestBlockNumber: BigInt, latestCheckpointNumber: BigInt)
}
