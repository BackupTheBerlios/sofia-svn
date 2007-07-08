using System;
using System.Collections.Generic;
using System.Text;

namespace HyperTreeControl
{
    /// <summary> The NodeModel class implements encapsulation of a INode for the model. 
    /// It keeps the original euclidian coordinates of the node. 
    /// It implements the Composite design pattern.
    /// </summary>
    public class NodeModel
    {
        #region Private fields

        private INode _node = null; // encapsulated INode
        private Model _model = null; // tree model
        private CompositeNodeModel _parent = null; // parent node
        private EuclidianVector _z = null; // Euclidian coordinates
        private double _weight = 1.0;  // part of space taken by this node

        #endregion

        #region ctor

        /// <summary> Constructor for root node.
        /// </summary>
        /// <param name="node">The encapsulated <see cref="INode"/>.</param>
        /// <param name="model">The tree model using this <see cref="NodeModel"/>.</param>
        public NodeModel(INode node, Model model)
            : this(node, null, model)
        {
        }

        /// <summary> Constructor.     
        /// </summary>
        /// <param name="node">The encapsulated INode.</param>
        /// <param name="parent">The parent node.</param>
        /// <param name="model">The tree model using this NodeModel.</param>
        public NodeModel(INode node, CompositeNodeModel parent, Model model)
        {
            _node = node;
            _parent = parent;
            _model = model;
            model.IncrementNumberOfNodes();

            _z = new EuclidianVector();
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
        public INode Node
        {
            get
            {
                return _node;
            }
        }

        /// <summary> Gets the euclidian coordinates.
        /// </summary>
        public EuclidianVector Z
        {
            get { return _z; }
            set { _z = value; }
        }

        /// <summary> Gets the encapsulated tree model.
        /// </summary>
        public Model Model
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
        public CompositeNodeModel Parent
        {
            get
            {
                return _parent;
            }
        }

        /// <summary> Returns true if this node is not an instance of CompositeNodeModel.
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
        /// <returns>The original hyperbolic coordinates.</returns>
        /// </summary>
        public EuclidianVector OriginalCoordinates
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
        /// <param name="angle">The angle from the x axis</param>
        /// <param name="width">The angular width to divide, / 2</param>
        /// <param name="length">The parent-child length.</param>
        public virtual void Layout(double angle, double width, double length)
        {
            // Nothing to do for the root node
            if (_parent == null)
            {
                return;
            }

            EuclidianVector __zp = _parent.OriginalCoordinates;

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
                            ":" + _z +
                            ";Weight = " + _weight;
            return __result;
        }

        #endregion

    }
}
