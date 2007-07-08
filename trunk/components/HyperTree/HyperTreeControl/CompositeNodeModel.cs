using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
    /// <summary> The CompositeNodeModel class implements the Composite design pattern for NodeModel.
    /// It represents a NodeModel which is not a leaf.
    /// </summary>
    public class CompositeNodeModel : NodeModel
    {
        #region Private fields

        private List<NodeModel> _children = null; // children of this node
        private double _globalWeight = 0.0;  // sum of children weight

        #endregion

        #region ctor

        /// <summary>Constructor for root node.
        /// </summary>
        /// <param name="node">The encapsulated INode.</param>
        /// <param name="model">The tree model using this NodeModel.</param>
        public CompositeNodeModel(INode node, Model model)
            : this(node, null, model)
        {

        }

        /// <summary>Connstructor.
        /// </summary>
        /// <param name="node">The encapsulated INode.</param>
        /// <param name="parent">The parent node.</param>
        /// <param name="model">The tree model using this NodeModel.</param>
        public CompositeNodeModel(INode node, CompositeNodeModel parent, Model model)
            : base(node, parent, model)
        {
            _children = new List<NodeModel>();

            NodeModel __child = null;
            foreach (INode childNode in node.ChildNodes)
            {
                if (childNode.IsLeaf)
                {
                    __child = new NodeModel(childNode, this, model);
                }
                else
                {
                    __child = new CompositeNodeModel(childNode, this, model);
                }
                this.AddChild(__child);
            }

            // here the down of the tree is built, so we can compute the weight
            this.ComputeWeight();
        }

        #endregion

        #region Weight Managment

        /// <summary> Compute the Weight of this node. 
        /// As the weight is computed with the log of the sum of child's weight, we must have all children  built before starting the computing.
        /// </summary>
        private void ComputeWeight()
        {
            foreach (NodeModel child in _children)
                _globalWeight += child.Weight;

            if (_globalWeight != 0.0)
                Weight += Math.Log(_globalWeight);
        }

        #endregion

        #region Tree management

        /// <summary> Gets the children of this node.
        /// </summary>
        public List<NodeModel> Children
        {
            get
            {
                return _children;
            }
        }

        /// <summary> Adds the <see cref="NodeModel"/> as a children.
        /// </summary>
        /// <param name="child">The child.</param>  
        private void AddChild(NodeModel child)
        {
            _children.Add(child);
        }

        /// <summary> Returns false as this node is an instance of <see cref="CompositeNodeModel"/>.
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
        /// <param name="angle"> The angle from the x axis.</param>
        /// <param name="width">The angular width to divide. / 2</param>
        /// <param name="length">The parent-child length.</param>
        public override void Layout(double angle, double width, double length)
        {
            base.Layout(angle, width, length);

            if (Parent != null)
            {
                // Compute the new starting angle
                // e(i a) = T(z)oT(zp) (e(i angle))
                EuclidianVector __a = new EuclidianVector(Math.Cos(angle), Math.Sin(angle));
                EuclidianVector __nz = new EuclidianVector(-Z.X, -Z.X);
                __a.Translate(Parent.OriginalCoordinates);
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

            EuclidianVector __dump = new EuclidianVector();

            int __nbrChild = _children.Count;
            double __l1 = (0.95 - Model.Length);
            double __l2 = Math.Cos((20.0 * Math.PI) / (2.0 * __nbrChild + 38.0));
            length = Model.Length + (__l1 * __l2);

            double __startAngle = angle - width;

            // It may be interesting to sort children by weight instead
            foreach (NodeModel child in _children)
            {
                double __percent = child.Weight / _globalWeight;
                double __childWidth = width * __percent;
                double __childAngle = __startAngle + __childWidth;
                child.Layout(__childAngle, __childWidth, length);
                __startAngle += 2.0 * __childWidth;
            }
        }

        #endregion

        #region ToString

        public override string ToString()
        {
            string __result = base.ToString();
            __result += "Children :";
            foreach (NodeModel __child in _children)
                __result += "-> " + __child.Name;
            return __result;
        }

        #endregion
    }
}
