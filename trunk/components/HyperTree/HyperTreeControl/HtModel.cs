using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
  /// <summary> The HTModel class implements the model for the HyperTree.
  /// It's a tree of HTModelNode and HTModelNodeComposite, each keeping the initial layout of the tree in the Poincarre's Model.
  /// </summary>
  public class HtModel
  {
    #region Private fields

    private HtModelNode _root = null; // the root of the tree's model 
    private double _length = 0.3;  // distance between node and children
    private int _nodes = 0;    // number of nodes

    #endregion

    #region ctor

    /// <summary> Constructor.
    /// </summary>
    /// <param name="root">The root of the real tree.</param>
    public HtModel(IHtNode root)
    {
      if (root.IsLeaf)
      {
        _root = new HtModelNode(root, this);
      }
      else
      {
        _root = new HtModelNodeComposite(root, this);
      }
      _root.LayoutHyperbolicTree();
    }

    #endregion

    #region Public properties

    /// <summary> Gets the root of the tree model.
    /// </summary>
    public HtModelNode Root
    {
      get
      {
        return _root;
      }
    }

    /// <summary> Gets the distance between a node and its children in the hyperbolic space.
    /// </summary>
    public double Length
    {
      get
      {
        return _length;
      }
    }

    /// <summary> Gets the number of nodes.
    /// </summary>
    public int NumberOfNodes
    {
      get
      {
        return _nodes;
      }
    }

    #endregion

    #region Nodes methods

    /// <summary> Increments the number of nodes.
    /// </summary>
    public void IncrementNumberOfNodes()
    {
      _nodes++;
    }

    #endregion
  }
}
