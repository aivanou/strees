package org.ml.trees

import java.util.concurrent.atomic.AtomicReference

import breeze.linalg.DenseVector

import scala.annotation.tailrec
import scala.collection._

class DecisionTree(minThreshold: Int, impurityThreshold: Double) {

  private var root: Node = new UnlabeledLeaf(null)

  private var unlabeledLeafsRef: AtomicReference[immutable.HashSet[UnlabeledLeaf]] = init()

  private def init(): AtomicReference[immutable.HashSet[UnlabeledLeaf]] = {
    val ref = new AtomicReference[immutable.HashSet[UnlabeledLeaf]]()
    ref.set(immutable.HashSet[UnlabeledLeaf](root.asInstanceOf[UnlabeledLeaf]))
    ref
  }

  def fitSet(ds: Dataset): Double = {
    var error = 0.0
    for (i <- 0 until ds.size) {
      val nd = fit(ds.features(i, ::).inner)
      nd match {
        case leaf: LabeledLeaf =>
          if (ds.labels(i) != leaf.label) {
            error += 1
          }
      }
    }
    error
  }

  def uleafs(): Int = uleafs(root)

  def uleafs(node: Node): Int = node match {
    case sp: SplitNode => uleafs(sp.left) + uleafs(sp.right)
    case ul: UnlabeledLeaf => 1
    case _ => 0
  }

  def converged: Boolean = unlabeledLeafsRef.get().isEmpty

  def cnt: Int = unlabeledLeafsRef.get().size

  def fit(featureVector: DenseVector[Double]): Node = fit(featureVector, root)

  def fit(node: Option[Node], featureVector: DenseVector[Double]): Node = node match {
    case Some(nd) => fit(featureVector, nd)
    case None => fit(featureVector, root)
  }

  def predict(feature: DenseVector[Double]): Int = fit(feature) match {
    case l: LabeledLeaf => l.label
    case _ => -1
  }

  def tryLabel(leaf: UnlabeledLeaf, impurity: Double, distribution: Map[Int, Int]): Boolean = {
    if (leaf.parent == null) return false
    val parent = leaf.parent
    val npoints = distribution.values.sum
    val (label, _) = distribution.reduce((p1, p2) => if (p1._2 > p2._2) p1 else p2)
    if (distribution.size == 1 || npoints <= minThreshold || impurityThreshold >= impurity) {
      val newLeaf = new LabeledLeaf(parent, label)
      updateLeafs(leaf, (leafs, leaf) => leafs - leaf)
      setNode(parent, newLeaf, leaf)
      true
    } else {
      false
    }
  }

  @tailrec
  private def updateLeafs(leaf: UnlabeledLeaf, func: (immutable.HashSet[UnlabeledLeaf], UnlabeledLeaf) => immutable.HashSet[UnlabeledLeaf]): Unit = {
    val prevLeafs = unlabeledLeafsRef.get()
    val newLeafs = func(prevLeafs, leaf)
    if (!unlabeledLeafsRef.compareAndSet(prevLeafs, newLeafs)) {
      updateLeafs(leaf, func)
    }
  }

  def setNode(parent: SplitNode, leaf: Node, prevLeaf: Node): Unit = {
    if (parent.left == prevLeaf) {
      parent.left = leaf
    } else parent.right = leaf
  }

  def grow(leaf: UnlabeledLeaf, featureIndex: Int, splitValue: Double): Unit = {
    val newNode = splitNode(splitValue, featureIndex)
    updateLeafs(leaf, (leafs, leaf) => leafs - leaf)
    if (leaf.parent == null) {
      root = newNode
    } else {
      val parent = leaf.parent
      setNode(parent, newNode, leaf)
      newNode.parent = parent
    }
  }

  def splitNode(splitValue: Double, featureIndex: Int): SplitNode = {
    val splitNode = new SplitNode(null, null, null, splitValue, featureIndex)
    val leftLeaf = new UnlabeledLeaf(splitNode)
    val rightLeaf = new UnlabeledLeaf(splitNode)
    updateLeafs(rightLeaf, (leafs, leaf) => leafs + leaf)
    updateLeafs(leftLeaf, (leafs, leaf) => leafs + leaf)
    splitNode.left = leftLeaf
    splitNode.right = rightLeaf
    splitNode
  }

  @tailrec
  private def fit(featureVector: DenseVector[Double], node: Node): Node = {
    node match {
      case splitNode: SplitNode =>
        if (featureVector(splitNode.featureIndex) < splitNode.splitValue) fit(featureVector, splitNode.left)
        else fit(featureVector, splitNode.right)
      case el: Node => el
    }
  }
}

class Node(var parent: SplitNode)

class SplitNode(var left: Node, var right: Node, parent: SplitNode, var splitValue: Double, var featureIndex: Int) extends Node(parent)

class LabeledLeaf(parent: SplitNode, var label: Int) extends Node(parent)

class UnlabeledLeaf(parent: SplitNode) extends Node(parent)
