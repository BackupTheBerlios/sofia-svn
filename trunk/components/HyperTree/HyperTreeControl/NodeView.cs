using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Media;
using System.Windows;
using System.Windows.Controls;
using System.Globalization;
using WiredPrairie.Decorators;

namespace HyperTreeControl
{
    /// <summary>
    /// The NodeView class contains the drawing coordinates of a <see cref="ModelNodel"></see>for the <see cref="IView"/>.
    /// It implements the Composite design pattern.
    /// </summary>
    public class NodeView : SmartBorder
    {
        #region fields

        private Renderer _renderer = null;  // drawing model
        private NodeModel _nodeModel = null;  // encapsulated NodeModel

        private EuclidianVector _ze = null;  // current euclidian coordinates
        private EuclidianVector _oldZe = null;  // old euclidian coordinates
        private ScreenVector _screenCoordinates = null;  // current screen coordinates

        private CompositeNodeView _parentNode = null;  // father of this node
        private NodeView _brother = null;  // brother of this node

        private bool _active = false; // should be drawed ?

        #endregion

        #region Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="parentNode">The father of this node.</param>
        /// <param name="nodeModel">The encapsulated <see cref="NodeModel"/>.</param>
        /// <param name="renderer">The drawing model.</param>
        public NodeView(CompositeNodeView parentNode, NodeModel nodeModel, Renderer renderer)
        {
            _parentNode = parentNode;
            _nodeModel = nodeModel;
            _renderer = renderer;

            this.Opacity = 0.9;
            this.Child = new NodeLabel(this);

            _ze = new EuclidianVector(nodeModel.OriginalCoordinates);
            _oldZe = new EuclidianVector(_ze);
            _screenCoordinates = new ScreenVector();

            // store this object in INode -> NodeView mapping
            renderer.MapNode(nodeModel.Node, this);

            renderer.Children.Add(this);
        }

        #endregion

        #region General accessors

        /// <summary> Gets or sets the brother of this node.
        /// </summary>
        public NodeView Brother
        {
            get
            {
                return _brother;
            }

            set
            {
                _brother = value;
            }
        }

        /// <summary> Gets the encapsulated HtModelNode.
        /// </summary>
        public NodeModel NodeModel
        {
            get
            {
                return _nodeModel;
            }
        }

        /// <summary> Gets the color of the node.
        /// </summary>
        public Color Color
        {
            get
            {
                return _nodeModel.Node.Color;
            }
        }

        /// <summary> Gets the name of this node.
        /// </summary>
        public string NodeName
        {
            get
            {
                return _nodeModel.Name;
            }
        }

        /// <summary> Gets the size of the node.
        /// </summary>
        public int Size
        {
            get
            {
                return _nodeModel.Node.Size;
            }
        }


        /// <summary> Gets the thickness of the border.
        /// </summary>
        public int BorderSize
        {
            get
            {
                return _nodeModel.Node.BorderSize;
            }
        }

        /// <summary> Gets the image which is displayed inside the node.
        /// </summary>
        public Image Image
        {
            get
            {
                return _nodeModel.Node.Image;
            }
        }

        #endregion

        #region Coordinates

        /// <summary> Gets the current coordinates of this node.
        /// </summary>
        public EuclidianVector Coordinates
        {
            get
            {
                return _ze;
            }
        }

        /// <summary> Gets the old coordinates of this node.
        /// </summary>
        public EuclidianVector OldCoordinates
        {
            get
            {
                return _oldZe;
            }
        }

        /// <summary> Gets the screen coordinates of this node.
        /// </summary>
        public ScreenVector ScreenCoordinates
        {
            get
            {
                return _screenCoordinates;
            }
        }

        /// <summary> Refresh the screen coordinates of this node. 
        /// </summary>
        /// <param name="origin">The origin of the screen plane.</param>
        /// <param name="max">The (xMax, yMax) point in the screen plane.</param>
        public virtual void RefreshScreenCoordinates(ScreenVector origin, ScreenVector max)
        {
            _screenCoordinates.ProjectionEtoS(_ze, origin, max);
        }

        #endregion

        #region Drawing


        /// <summary> Draws the branches from this node to its children.
        /// </summary>
        /// <remarks>Overriden by the <see cref="CompositeNodeView"/> class.</remarks>
        /// <param name="canvas">The graphic canvas.</param>
        public virtual void DrawBranches(DrawingContext dc) { }

        /// <summary> Draw this node.
        /// Adjust size and position accordingly to the font metrics.
        /// </summary>
        /// <param name="dc">The graphic canvas.</param>        
        public virtual void DrawNodes(DrawingContext dc)
        {
            //get the font metrics
            FontFamily __font = new FontFamily("Arial");
            FormattedText __formattedText = new FormattedText(
               this.NodeName,
               CultureInfo.CurrentCulture,
               FlowDirection.LeftToRight,
               new Typeface(
                   __font,
                   FontStyles.Normal,
                   FontWeights.Light,
                   FontStretches.Normal),
               10,
               Brushes.Black
               );

            int __height = (int)__formattedText.Height;
            int __width = (int)__formattedText.Width;
            this.Height = __height + 2 * this.Size;
            this.Width = __width + 10 + 2 * this.Size;
            ScreenVector __zs = this.ScreenCoordinates;

            double __x = __zs.X - (this.Width / 2) - this.Size;
            double __y = __zs.Y - (this.Height / 2) - this.Size;

            int __space = this.GetSpace();
            if (__space >= __height)
            {
                _active = true;
                this.Visibility = Visibility.Visible;
                Canvas.SetLeft(this, __x);
                Canvas.SetTop(this, __y);
            }
            else
            {
                _active = false;
                this.Visibility = Visibility.Collapsed;
            }
        }

        /// <summary> Returns the minimal distance between this node
        /// and his father and his brother.
        /// </summary>
        /// <returns>The minimal distance.</returns>
        public virtual int GetSpace()
        {
            int __dF = -1;
            int __dB = -1;

            if (_parentNode != null)
            {
                ScreenVector __zF = _parentNode.ScreenCoordinates;
                __dF = _screenCoordinates.GetDistance(__zF);
            }
            if (_brother != null)
            {
                ScreenVector __zB = _brother.ScreenCoordinates;
                __dB = _screenCoordinates.GetDistance(__zB);
            }

            // this means that the node is a standalone node
            if ((__dF == -1) && (__dB == -1))
            {
                return int.MaxValue;
            }
            else if (__dF == -1)
            {
                return __dB;
            }
            else if (__dB == -1)
            {
                return __dF;
            }
            else
            {
                return Math.Min(__dF, __dB);
            }
        }

        #endregion

        #region Translation

        /// <summary> Translates this node by the given vector.
        /// </summary>
        /// <param name="t">The translation vector.</param>
        public virtual void Translate(EuclidianVector t)
        {
            _ze.Translate(_oldZe, t);
        }

        /// <summary> Transform this node by the given transformation.
        /// </summary>
        /// <param name="t">The transformation.</param>
        public virtual void Transform(HyperbolicTransformation t)
        {
            _ze.Copy(_oldZe);
            _ze.Transform(t);
        }

        /// <summary> Ends the translation.
        /// </summary>
        public virtual void EndTranslation()
        {
            _oldZe.Copy(_ze);
        }

        /// <summary> Restores the hyperbolic tree to its origin.
        /// </summary>
        public virtual void Restore()
        {
            EuclidianVector __orig = _nodeModel.OriginalCoordinates;
            _ze.X = __orig.X;
            _ze.Y = __orig.Y;
            _oldZe.Copy(_ze);
        }

        #endregion

        #region Node searching

        /// <summary> Returns the node (if any) whose screen coordinates' zone contains thoses given in parameters.
        /// </summary>
        /// <param name="zs">The given screen coordinate.</param>
        /// <returns>the searched <see cref="NodeView"/> if found; <code>null</code> otherwise.</returns>
        public virtual NodeView FindNode(ScreenVector zs)
        {
            if (this.Contains(zs))
            {
                return this;
            }
            else
            {
                return null;
            }
        }

        /// <summary> Performs the specified action on this node.
        /// </summary>
        /// <param name="zs">The given screen coordinate.</param>
        /// <param name="action">The <see cref="System.Action<T>"></see> delegate to perform on the node whose screen coordinates' zone contains thoses given in parameters.</param>
        /// <param name="actionElse">The <see cref="System.Action<T>"></see> delegate to perform on the node whose screen coordinates' zone do not contains thoses given in parameters.</param>
        public virtual void ForEach(ScreenVector zs, Action<NodeView> action, Action<NodeView> actionElse)
        {
            if (this.Contains(zs))
            {
                action(this);
            }
            else
            {
                actionElse(this);
            }
        }

        #endregion

        #region Zone containing

        /// <summary> Is the given <see cref="ScreenVector"/> within this control ?
        /// </summary>
        /// <param name="zs">The given point.</param>
        /// <returns><code>true</code> if it is, <code>false</code> otherwise.</returns>
        public bool Contains(ScreenVector zs)
        {
            if (_active)
            {
                if ((zs.X >= Canvas.GetLeft(this)) && (zs.X <= (Canvas.GetLeft(this) + this.Width)) &&
                    (zs.Y >= Canvas.GetTop(this)) && (zs.Y <= (Canvas.GetTop(this) + this.Height)))
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else
            {
                return this.ScreenCoordinates.Contains(zs);
            }
        }

        #endregion

        #region ToString

        /// <summary> Returns a string representation of the object.
        /// </summary>
        /// <returns>A string representation of the object.</returns>
        public override string ToString()
        {
            string __result = NodeName +
                            ";" + _ze +
                            ";" + _screenCoordinates;
            return __result;
        }

        #endregion
    }
}
