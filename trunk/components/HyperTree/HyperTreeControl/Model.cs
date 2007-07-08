using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
    /// <summary> The Model class implements the model for the HyperTree.
    /// It's a tree of NodeModel and CompositeNodeModel, each keeping the initial layout of the tree in the Poincarre's Model.
  /// </summary>
  public class Model
  {
    #region Private fields

    private NodeModel _root = null; // the root of the tree's model 
    private double _length = 0.3;  // distance between node and children
    private int _nodeCount = 0;    // number of nodes

    #endregion

    #region ctor

    /// <summary> Constructor.
    /// </summary>
    /// <param name="root">The root of the real tree.</param>
    public Model(INode root)
    {
      if (root.IsLeaf)
      {
        _root = new NodeModel(root, this);
      }
      else
      {
        _root = new CompositeNodeModel(root, this);
      }
      _root.LayoutHyperbolicTree();
    }

    #endregion

    #region Public properties

    /// <summary> Gets the root of the tree model.
    /// </summary>
    public NodeModel Root
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

    #endregion

    #region Nodes methods

    /// <summary> Increments the number of nodes.
    /// </summary>
    public void IncrementNumberOfNodes()
    {
      _nodeCount++;
    }

    #endregion
  }
}
