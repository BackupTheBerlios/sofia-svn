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
    public class HtDrawNode: SmartBorder
    {
        #region fields

        private HtDraw _model = null;  // drawing model
        private HtModelNode _node = null;  // encapsulated HTModelNode

        private HtCoordE _ze = null;  // current euclidian coordinates
        private HtCoordE _oldZe = null;  // old euclidian coordinates
        protected HtCoordS _zs = null;  // current screen coordinates

        private HtDrawNodeComposite _father = null;  // father of this node
        private HtDrawNode _brother = null;  // brother of this node

        private bool _active = false; // should be drawed ?

        #endregion

        #region Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="father">The father of this node.</param>
        /// <param name="node">The encapsulated <see cref="HtModelNode"/>.</param>
        /// <param name="model">The drawing model.</param>
        public HtDrawNode(HtDrawNodeComposite father, HtModelNode node, HtDraw model)
        {
            _father = father;
            _node = node;
            _model = model;

            //this.Background = new LinearGradientBrush(Colors.DarkGray, Colors.LightGray, 90);
            //this.BorderBrush = Brushes.White;
            //this.BorderThickness = new Thickness(1);
            //this.CornerRadius = new CornerRadius(3);
            this.Opacity = 0.9;

            this.Child = new HtNodeLabel(this);
            
            _ze = new HtCoordE(node.Coordinates);
            _oldZe = new HtCoordE(_ze);
            _zs = new HtCoordS();

            // store this object in IHtNode -> HtDrawNode mapping
            model.MapNode(node.Node, this);

            model.Children.Add(this);
        }

        #endregion

        #region General accessors

        /// <summary> Gets or sets the brother of this node.
        /// </summary>
        public HtDrawNode Brother
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
        public HtModelNode HtModelNode
        {
            get
            {
                return _node;
            }
        }

        /// <summary> Gets the color of the node.
        /// </summary>
        public Color Color
        {
            get
            {
                return _node.Node.Color;
            }
        }

        /// <summary> Gets the name of this node.
        /// </summary>
        public string NodeName
        {
            get
            {
                return _node.Name;
            }
        }

        /// <summary> Gets the size of the node.
        /// </summary>
        public int Size
        {
            get
            {
                return _node.Node.Size;
            }
        }


        /// <summary> Gets the thickness of the border.
        /// </summary>
        public int BorderSize
        {
            get
            {
                return _node.Node.BorderSize;
            }
        }

        /// <summary> Gets the image which is displayed inside the node.
        /// </summary>
        public Image Image
        {
            get
            {
                return _node.Node.Image;
            }
        }

        #endregion

        #region Coordinates

        /// <summary> Gets the current coordinates of this node.
        /// </summary>
        public HtCoordE Coordinates
        {
            get
            {
                return _ze;
            }
        }

        /// <summary> Gets the old coordinates of this node.
        /// </summary>
        public HtCoordE OldCoordinates
        {
            get
            {
                return _oldZe;
            }
        }

        /// <summary> Gets the screen coordinates of this node.
        /// </summary>
        public HtCoordS ScreenCoordinates
        {
            get
            {
                return _zs;
            }
        }

        /// <summary> Refresh the screen coordinates of this node. 
        /// </summary>
        /// <param name="sOrigin">The origin of the screen plane.</param>
        /// <param name="sMax">The (xMax, yMax) point in the screen plane.</param>
        public virtual void RefreshScreenCoordinates(HtCoordS sOrigin, HtCoordS sMax)
        {
            _zs.ProjectionEtoS(_ze, sOrigin, sMax);
        }

        #endregion

        #region Drawing


        /// <summary> Draws the branches from this node to its children.
        /// </summary>
        /// <remarks>Overriden by the <see cref="HtDrawNodeComposite"/> class.</remarks>
        /// <param name="canvas">The graphic canvas.</param>
        public virtual void DrawBranches(DrawingContext dc) { }

        /// <summary> Draw this node.
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public virtual void DrawNodes(DrawingContext dc)
        {
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
            HtCoordS __zs = this.ScreenCoordinates;

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

            if (_father != null)
            {
                HtCoordS __zF = _father.ScreenCoordinates;
                __dF = _zs.GetDistance(__zF);
            }
            if (_brother != null)
            {
                HtCoordS __zB = _brother.ScreenCoordinates;
                __dB = _zs.GetDistance(__zB);
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
        public virtual void Translate(HtCoordE t)
        {
            _ze.Translate(_oldZe, t);
        }

        /// <summary> Transform this node by the given transformation.
        /// </summary>
        /// <param name="t">The transformation.</param>
        public virtual void Transform(HtTransformation t)
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
            HtCoordE __orig = _node.Coordinates;
            _ze.X = __orig.X;
            _ze.Y = __orig.Y;
            _oldZe.Copy(_ze);
        }

        #endregion

        #region Node searching

        /// <summary> Returns the node (if any) whose screen coordinates' zone
        /// contains thoses given in parameters.
        /// </summary>
        /// <param name="zs">The given screen coordinate.</param>
        /// <returns>the searched <see cref="HtDrawNode"/> if found; <code>null</code> otherwise.</returns>
        public virtual HtDrawNode FindNode(HtCoordS zs)
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

        #endregion

        #region Zone containing

        /// <summary> Is the given <see cref="HtCoordS"/> within this control ?
        /// </summary>
        /// <param name="zs">The given point.</param>
        /// <returns><code>true</code> if it is, <code>false</code> otherwise.</returns>
        public bool Contains(HtCoordS zs)
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
                            "\n\t" + _ze +
                            "\n\t" + _zs;
            return __result;
        }

        #endregion
    }
}
