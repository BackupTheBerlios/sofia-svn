using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
  /// <summary> The HTModelNodeComposite class implements the Composite design pattern for HTModelNode.
  /// It represents a HTModelNode which is not a leaf.
  /// </summary>
  public class HtModelNodeComposite : HtModelNode
  {
    #region Private fields

    private List<HtModelNode> _children = null; // children of this node
    private double _globalWeight = 0.0;  // sum of children weight

    #endregion

    #region ctor

    /// <summary>Constructor for root node.
    /// </summary>
    /// <param name="node">The encapsulated HTNode.</param>
    /// <param name="model">The tree model using this HtModelNode.</param>
    public HtModelNodeComposite(IHtNode node, HtModel model)
      : this(node, null, model)
    {

    }

    /// <summary>Connstructor.
    /// </summary>
    /// <param name="node">The encapsulated HTNode.</param>
    /// <param name="parent">The parent node.</param>
    /// <param name="model">The tree model using this HtModelNode.</param>
    public HtModelNodeComposite(IHtNode node, HtModelNodeComposite parent, HtModel model)
      : base(node, parent, model)
    {
      _children = new List<HtModelNode>();

      HtModelNode __child = null;
      foreach (IHtNode __childNode in node.Children)
      {
        if (__childNode.IsLeaf)
        {
          __child = new HtModelNode(__childNode, this, model);
        }
        else
        {
          __child = new HtModelNodeComposite(__childNode, this, model);
        }
        AddChild(__child);
      }

      // here the down of the tree is built, so we can compute the weight
      ComputeWeight();
    }

    #endregion

    #region Weight Managment

    /// <summary> Compute the Weight of this node. 
    /// As the weight is computed with the log of the sum of child's weight, we must have all children  built before starting the computing.
    /// </summary>
    private void ComputeWeight()
    {
      foreach (HtModelNode __child in _children)
        _globalWeight += __child.Weight;

      if (_globalWeight != 0.0)
        Weight += Math.Log(_globalWeight);
    }

    #endregion

    #region Tree management

    /// <summary> Gets the children of this node, in an Enumeration.
    /// </summary>
    public List<HtModelNode> Children
    {
      get
      {
        return this._children;
      }
    }

    /// <summary> Adds the HtModelNode as a children.
    /// </summary>
    /// <param name="child">The child.</param>  
    void AddChild(HtModelNode child)
    {
      _children.Add(child);
    }

    /// <summary> Returns false as this node is an instance of HTModelNodeComposite.
    /// </summary>
    public override bool IsLeaf
    {
      get
      {
        return false;
      }
    }

    #endregion

    #region Hyperbolic layout

    /// <summary> Layout this node and its children in the hyperbolic space.
    /// Mainly, divide the width angle between children and put the children at the right angle.
    /// Compute also an optimized length to the children.     
    /// </summary>
    /// <param name="angle"> The angle from the x axis (bold as love)</param>
    /// <param name="width">The angular width to divide, / 2</param>
    /// <param name="length">The parent-child length.</param>
    public override void Layout(double angle, double width, double length)
    {
      base.Layout(angle, width, length);

      if (Parent != null)
      {
        // Compute the new starting angle
        // e(i a) = T(z)oT(zp) (e(i angle))
        HtCoordE __a = new HtCoordE(Math.Cos(angle), Math.Sin(angle));
        HtCoordE __nz = new HtCoordE(-Z.X, -Z.X);
        __a.Translate(Parent.Coordinates);
        __a.Translate(__nz);
        angle = __a.Arg();

        // Compute the new width
        // e(i w) = T(-length) (e(i width))
        // decomposed to do it faster :-)
        double __c = Math.Cos(width);
        double __A = 1 + length * length;
        double __B = 2 * length;
        width = Math.Acos((__A * __c - __B) / (__A - __B * __c));
      }

      HtCoordE __dump = new HtCoordE();

      int __nbrChild = _children.Count;
      double __l1 = (0.95 - Model.Length);
      double __l2 = Math.Cos((20.0 * Math.PI) / (2.0 * __nbrChild + 38.0));
      length = Model.Length + (__l1 * __l2);

      double __startAngle = angle - width;

      // It may be interesting to sort children by weight instead
      foreach (HtModelNode __child in _children)
      {
        double __percent = __child.Weight / _globalWeight;
        double __childWidth = width * __percent;
        double __childAngle = __startAngle + __childWidth;
        __child.Layout(__childAngle, __childWidth, length);
        __startAngle += 2.0 * __childWidth;
      }
    }

    #endregion

    #region ToString

    public override string ToString()
    {
      string result = base.ToString();
      result += "\n\tChildren :";
      foreach (HtModelNode child in _children)
        result += "\n\t-> " + child.Name;
      return result;
    }

    #endregion
  }
}
