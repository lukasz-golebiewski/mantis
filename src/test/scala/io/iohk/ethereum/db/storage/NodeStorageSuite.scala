package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.network.p2p.messages.ETH63.MptNodeEncoders._
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class NodeStorageSuite extends AnyFunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  test("NodeStorage insert") {
    forAll(Gen.listOf(nodeGen)) { unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct
      val initialNodeStorage: NodeStorage = new NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage) { case (recNodeStorage, node) =>
        recNodeStorage.update(Nil, Seq(ByteString(node.hash) -> node.toBytes))
      }

      mptNodes.foreach { node =>
        val obtainedNode = nodeStorage.get(ByteString(node.hash)).map(_.toMptNode)
        assert(obtainedNode.contains(node))
      }
    }
  }

  test("NodeStorage delete") {
    forAll(Gen.listOf(nodeGen)) { unfilteredMptNodes =>
      val mptNodes = unfilteredMptNodes.distinct

      //Nodes are inserted
      val initialNodeStorage: NodeStorage = new NodeStorage(EphemDataSource())
      val nodeStorage = mptNodes.foldLeft(initialNodeStorage) { case (recNodeStorage, node) =>
        recNodeStorage.update(Nil, Seq(ByteString(node.hash) -> node.toBytes))
      }

      //Nodes are deleted
      val (toDelete, toLeave) = mptNodes.splitAt(Gen.choose(0, mptNodes.size).sample.get)
      val nodeStorageAfterDelete = toDelete.foldLeft(nodeStorage) { case (recNodeStorage, node) =>
        recNodeStorage.update(Seq(ByteString(node.hash)), Nil)
      }

      toLeave.foreach { node =>
        val obtainedNode = nodeStorageAfterDelete.get(ByteString(node.hash)).map(_.toMptNode)
        assert(obtainedNode.contains(node))
      }
      toDelete.foreach { node => assert(nodeStorageAfterDelete.get(ByteString(node.hash)).isEmpty) }
    }
  }
}
