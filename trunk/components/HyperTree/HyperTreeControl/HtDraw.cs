using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Windows.Controls;
using System.Windows;
using System.Windows.Threading;
using System.Windows.Media;
using System.Windows.Markup;

namespace HyperTreeControl
{
    public class HtDraw : Canvas
    {
        #region fields

        public static readonly int NBR_FRAMES = 50; // number of intermediates animation frames

        private HtModel _model = null;  // the tree model
        private IHtView _view = null;  // the view using this drawing model
        private HtDrawNode _drawRoot = null;  // the root of the drawing tree 
        private double[] _ray = null;

        private HtCoordS _sOrigin = null;  // origin of the screen plane
        private HtCoordS _sMax = null;  // max point in the screen plane 

        private Dictionary<IHtNode, HtDrawNode> _drawToHTNodeMap;

        private HtDrawNode _animatedNode = null;
        private HtCoordE _velocity = new HtCoordE();

        #endregion

        #region ctor

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="model">The tree model to draw.</param>
        /// <param name="view">The view using this drawing model.</param>
        public HtDraw(HtModel model, IHtView view)
        {
            // initialize mapping
            _drawToHTNodeMap = new Dictionary<IHtNode, HtDrawNode>();

            _view = view;
            _model = model;

            _sOrigin = new HtCoordS();
            _sMax = new HtCoordS();

            _ray = new double[4];
            _ray[0] = model.Length;

            for (int i = 1; i < _ray.Length; i++)
            {
                _ray[i] = (_ray[0] + _ray[i - 1]) / (1 + (_ray[0] * _ray[i - 1]));
            }

            HtModelNode __root = model.Root;
            if (__root.IsLeaf)
            {
                _drawRoot = new HtDrawNode(null, __root, this);
            }
            else
            {
                _drawRoot = new HtDrawNodeComposite(null, (HtModelNodeComposite)__root, this);
            }

            this.Background = new LinearGradientBrush(Colors.Black, Colors.DarkBlue, 90);

            CompositionTarget.Rendering += UpdatePosition;

        }

        #endregion

        #region Internal classes accessors
        /*
        public static HtModel Model
        {
            get
            {
                return _model;
            }
        }

        public static double[] Ray
        {
            get
            {
                return _ray;
            }
        }

        public static IHtView View
        {
            get
            {
                return _view;
            }
        }
        */

        #endregion

        #region Screen coordinates

        /// <summary>
        /// Refresh the screen coordinates of the drawing tree.
        /// </summary>
        public void RefreshScreenCoordinates()
        {
            _sMax.X = (int)((this.Width) / 2);
            _sMax.Y = (int)((this.Height) / 2);
            _sOrigin.X = _sMax.X + 0;
            _sOrigin.Y = _sMax.Y + 0;
            _drawRoot.RefreshScreenCoordinates(_sOrigin, _sMax);
        }

        /// <summary>
        /// Gets the origin of the screen plane.
        /// </summary>
        public HtCoordS SOrigin
        {
            get
            {
                return _sOrigin;
            }
        }

        /// <summary>
        /// Gets the point representing the up right corner of the screen plane, 
        /// thus giving x and y maxima.
        /// </summary>
        public HtCoordS SMax
        {
            get
            {
                return _sMax;
            }
        }

        #endregion

        #region Drawing

        protected override void OnRender(DrawingContext dc)
        {
            this.RefreshScreenCoordinates();
            base.OnRender(dc);
            this.DrawBranches(dc);
            this.DrawNodes(dc);
        }

        /// <summary>
        ///Draws the branches of the hyperbolic tree.
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public void DrawBranches(DrawingContext dc)
        {
            _drawRoot.DrawBranches(dc);
        }

        /// <summary>
        /// Draws the nodes of the hyperbolic tree.
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public void DrawNodes(DrawingContext dc)
        {
            _drawRoot.DrawNodes(dc);
        }

        private void UpdatePosition(object sender, EventArgs e)
        {

            if (_animatedNode != null)
            {
                HtCoordE __toCenter = new HtCoordE(-_animatedNode.Coordinates.X, -_animatedNode.Coordinates.Y);
                _velocity.X *= 0.90;
                _velocity.Y *= 0.90;
                if (_velocity.D() > 0.01)
                    this.Translate(_animatedNode.OldCoordinates, _velocity);
                else
                {
                    _animatedNode = null;
                    this.EndTranslation();
                    _view.StartMouseListening();
                }
            }
        }
        #endregion

        #region Translation

        /// <summary> Translates the hyperbolic tree by the given vector.
        /// </summary>
        /// <param name="zs">The first coordinates.</param>
        /// <param name="ze">The second coordinates.</param>
        public void Translate(HtCoordE zs, HtCoordE ze)
        {
            HtCoordE __zo = new HtCoordE(_drawRoot.OldCoordinates);
            __zo.X = -__zo.X;
            __zo.Y = -__zo.Y;
            HtCoordE __zs2 = new HtCoordE(zs);
            __zs2.Translate(__zo);

            HtCoordE __t = new HtCoordE();
            double __de = ze.D2();
            double __ds = __zs2.D2();
            double __dd = 1.0 - __de * __ds;
            __t.X = (ze.X * (1.0 - __ds) - __zs2.X * (1.0 - __de)) / __dd;
            __t.Y = (ze.Y * (1.0 - __ds) - __zs2.Y * (1.0 - __de)) / __dd;

            if (__t.IsValid)
            {
                HtTransformation __to = new HtTransformation();
                __to.Composition(__zo, __t);

                _drawRoot.Transform(__to);
                _view.Repaint();
            }
        }

        /// <summary> Signal that the translation ended.
        /// </summary>
        public void EndTranslation()
        {
            _drawRoot.EndTranslation();
        }

        /// <summary> Translate the hyperbolic tree 
        /// so that the given node  is put at the origin of the hyperbolic tree.        
        /// </summary>
        /// <param name="node">The given <see cref="HtDrawNode"/></param>
        public void TranslateToOrigin(HtDrawNode node)
        {
            _view.StopMouseListening();
            _velocity = new HtCoordE(node.Coordinates);
            _animatedNode = node;            
        }

        /// <summary> Restores the hyperbolic tree to its origin.
        /// </summary>
        public void Restore()
        {
            _drawRoot.Restore();
            _view.Repaint();
        }

        #endregion

        #region Node searching

        /// <summary> Returns the node (if any) whose screen coordinates' zone
        /// contains thoses given in parameters.
        /// </summary>
        /// <param name="zs">The given screen coordinate.</param>
        /// <returns>The searched <see cref="HtDrawNode"/> if found;
        /// <code>null</code> otherwise</returns>
        public HtDrawNode FindNode(HtCoordS zs)
        {
            return _drawRoot.FindNode(zs);
        }


        /// <summary> Maps a <see cref="IHtNode"/> to a <see cref="HtDrawNode"/>.
        /// Used for backwards finding a <see cref="HtDrawNode"/> instance for a given
        /// <see cref="IHtNode"/>.
        /// </summary>
        /// <param name="htNode">The <see cref="IHtNode"/></param>
        /// <param name="drawNode">the <see cref="HtDrawNode"/> for the given <see cref="IHtNode"/></param>
        public void MapNode(IHtNode htNode, HtDrawNode drawNode)
        {
            _drawToHTNodeMap.Add(htNode, drawNode);
        }

        /// <summary> Finds a <see cref="HtDrawNode"/> for a given <see cref="IHtNode"/>.
        /// </summary>
        /// <param name="htNode">the <see cref="IHtNode"/> for which we want to find the <see cref="HtDrawNode"/>.</param>
        /// <returns>The <see cref="HtDrawNode"/> for the given <see cref="IHtNode"/>.</returns>
        public HtDrawNode FindDrawNode(IHtNode htNode)
        {
            HtDrawNode __drawNode = _drawToHTNodeMap[htNode];
            return __drawNode;
        }

        #endregion
    }    
}
