using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
  /// <summary> The HTModelNode class implements encapsulation of a HTNode for the model. 
  /// It keeps the original euclidian coordinates of the node. 
  /// It implements the Composite design pattern.
  /// </summary>
  public class HtModelNode
  {
    #region Private fields

    private IHtNode _node = null; // encapsulated IHtNode
    private HtModel _model = null; // tree model
    private HtModelNodeComposite _parent = null; // parent node
    private HtCoordE _z = null; // Euclidian coordinates
    private double _weight = 1.0;  // part of space taken by this node

    #endregion

    #region ctor

    /// <summary> Constructor for root node.
    /// </summary>
    /// <param name="node">The encapsulated IHtNode.</param>
    /// <param name="model">The tree model using this HTModelNode.</param>
    public HtModelNode(IHtNode node, HtModel model)
      : this(node, null, model)
    {
    }

    /// <summary> Constructor.     
    /// </summary>
    /// <param name="node">The encapsulated IHtNode.</param>
    /// <param name="parent">The encapsulated IHtNode.</param>
    /// <param name="model">The tree model using this HTModelNode.</param>
    public HtModelNode(IHtNode node, HtModelNodeComposite parent, HtModel model)
    {
      _node = node;
      _parent = parent;
      _model = model;
      model.IncrementNumberOfNodes();

      _z = new HtCoordE();
    }

    #endregion

    #region Public properties

    /// <summary> Gets the name of this node.
    /// </summary>
    public string Name
    {
      get
      {
        return _node.NodeName;
      }
    }

    /// <summary> Gets the encapsulated node.
    /// </summary>
    public IHtNode Node
    {
      get
      {
        return _node;
      }
    }

    /// <summary> Gets the euclidian coordinates.
    /// </summary>
    public HtCoordE Z
    {
      get { return _z; }
      set { _z = value; }
    }

    /// <summary> Gets the encapsulated tree model.
    /// </summary>
    public HtModel Model
    {
      get { return _model; }
      set { _model = value; }
    }

    #region Weight Managment

    /// <summary> Gets or sets the weight of this node.
    /// </summary>
    public double Weight
    {
      get
      {
        return _weight;
      }
      set
      {
        _weight = value;
      }
    }
    #endregion

    #region Tree management

    /// <summary> Gets the parent of this node.
    /// </summary>
    public HtModelNodeComposite Parent
    {
      get
      {
        return _parent;
      }
    }

    /// <summary> Returns true if this node is not an instance of HtModelNodeComposite.
    /// </summary>
    public virtual bool IsLeaf
    {
      get
      {
        return true;
      }
    }

    #endregion

    #region Coordinates

    /// <summary> Gets the coordinates of this node.
    /// Thoses are the original hyperbolic coordinates, without any translations.
    /// <remarks>this is NOT a copy but the true object (for performance).</remarks>
    /// <returns>The original hyperbolic coordinates.</returns>
    /// </summary>
    public HtCoordE Coordinates
    {
      get
      {
        return _z;
      }
    }

    #endregion

    #endregion

    #region Hyperbolic layout

    /// <summary> Layouts the nodes in the hyperbolic space.
    /// </summary>
    public void LayoutHyperbolicTree()
    {
      this.Layout(0.0, Math.PI, _model.Length);
    }

    /// <summary> Layout this node in the hyperbolic space.
    /// First set the point at the right distance, then translate by father's coordinates.
    /// Then, compute the right angle and the right width.
    /// </summary>
    /// <param name="angle">The angle from the x axis (bold as love)</param>
    /// <param name="width">The angular width to divide, / 2</param>
    /// <param name="length">The parent-child length.</param>
    public virtual void Layout(double angle, double width, double length)
    {
      // Nothing to do for the root node
      if (_parent == null)
      {
        return;
      }

      HtCoordE __zp = _parent.Coordinates;

      // We first start as if the parent was the origin.
      // We still are in the hyperbolic space.
      _z.X = length * Math.Cos(angle);
      _z.Y = length * Math.Sin(angle);

      // Then translate by parent's coordinates
      _z.Translate(__zp);
    }

    #endregion

    #region  ToString
    public override string ToString()
    {
      string __result = Name +
                      "\n\t" + _z +
                      "\n\tWeight = " + _weight;
      return __result;
    }

    #endregion

  }
}
