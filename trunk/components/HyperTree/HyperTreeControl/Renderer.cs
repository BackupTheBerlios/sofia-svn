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
    /// <summary>
    /// The Renderer class implements the drawing model for the <see cref="IView"/>.
    /// </summary>
    public class Renderer : Canvas
    {
        #region fields

        private Model _model = null;  // the tree model
        private IView _view = null;  // the view using this drawing model
        private NodeView _rootNodeView = null;  // the root of the drawing tree 
        private double[] _ray = null;

        private ScreenVector _screenPlaneOrigin = null;  // origin of the screen plane
        private ScreenVector _screenPlaneMaxPoint = null;  // max point in the screen plane 

        private Dictionary<INode, NodeView> _nodeViewMap;

        private NodeView _animatedNode = null;
        private EuclidianVector _velocity = new EuclidianVector();

        #endregion

        #region ctor

        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="model">The tree model to draw.</param>
        /// <param name="view">The view using this drawing model.</param>
        public Renderer(Model model, IView view)
        {
            // initialize mapping
            _nodeViewMap = new Dictionary<INode, NodeView>();

            _view = view;
            _model = model;

            _screenPlaneOrigin = new ScreenVector();
            _screenPlaneMaxPoint = new ScreenVector();

            _ray = new double[4];
            _ray[0] = model.Length;

            for (int i = 1; i < _ray.Length; i++)
            {
                _ray[i] = (_ray[0] + _ray[i - 1]) / (1 + (_ray[0] * _ray[i - 1]));
            }

            NodeModel __rootNodeModel = model.Root;
            if (__rootNodeModel.IsLeaf)
            {
                _rootNodeView = new NodeView(null, __rootNodeModel, this);
            }
            else
            {
                _rootNodeView = new CompositeNodeView(null, (CompositeNodeModel)__rootNodeModel, this);
            }

            this.Background = new LinearGradientBrush(Colors.Black, Colors.DarkBlue, 90);

            CompositionTarget.Rendering += UpdatePosition;

        }

        #endregion

        #region Screen coordinates

        /// <summary>
        /// Refresh the screen coordinates of the drawing tree.
        /// </summary>
        public void RefreshScreenCoordinates()
        {
            _screenPlaneMaxPoint.X = (int)((this.Width) / 2);
            _screenPlaneMaxPoint.Y = (int)((this.Height) / 2);
            _screenPlaneOrigin.X = _screenPlaneMaxPoint.X + 0;
            _screenPlaneOrigin.Y = _screenPlaneMaxPoint.Y + 0;
            _rootNodeView.RefreshScreenCoordinates(_screenPlaneOrigin, _screenPlaneMaxPoint);
        }

        /// <summary>
        /// Gets the origin of the screen plane.
        /// </summary>
        public ScreenVector SOrigin
        {
            get
            {
                return _screenPlaneOrigin;
            }
        }

        /// <summary>
        /// Gets the point representing the up right corner of the screen plane, 
        /// thus giving x and y maxima.
        /// </summary>
        public ScreenVector SMax
        {
            get
            {
                return _screenPlaneMaxPoint;
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
            _rootNodeView.DrawBranches(dc);
        }

        /// <summary>
        /// Draws the nodes of the hyperbolic tree.
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public void DrawNodes(DrawingContext dc)
        {
            _rootNodeView.DrawNodes(dc);
        }

        private void UpdatePosition(object sender, EventArgs e)
        {

            if (_animatedNode != null)
            {
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
        public void Translate(EuclidianVector zs, EuclidianVector ze)
        {
            EuclidianVector __zo = new EuclidianVector(_rootNodeView.OldCoordinates);
            __zo.X = -__zo.X;
            __zo.Y = -__zo.Y;
            EuclidianVector __zs2 = new EuclidianVector(zs);
            __zs2.Translate(__zo);

            EuclidianVector __t = new EuclidianVector();
            double __de = ze.D2();
            double __ds = __zs2.D2();
            double __dd = 1.0 - __de * __ds;
            __t.X = (ze.X * (1.0 - __ds) - __zs2.X * (1.0 - __de)) / __dd;
            __t.Y = (ze.Y * (1.0 - __ds) - __zs2.Y * (1.0 - __de)) / __dd;

            if (__t.IsValid)
            {
                HyperbolicTransformation __to = new HyperbolicTransformation();
                __to.Composition(__zo, __t);

                _rootNodeView.Transform(__to);
                _view.Repaint();
            }
        }

        /// <summary> Signal that the translation ended.
        /// </summary>
        public void EndTranslation()
        {
            _rootNodeView.EndTranslation();
        }

        /// <summary> Translate the hyperbolic tree 
        /// so that the given node  is put at the origin of the hyperbolic tree.        
        /// </summary>
        /// <param name="node">The given <see cref="HtDrawNode"/></param>
        public void TranslateToOrigin(NodeView node)
        {
            _view.StopMouseListening();
            _velocity = new EuclidianVector(node.Coordinates);
            _animatedNode = node;            
        }

        /// <summary> Restores the hyperbolic tree to its origin.
        /// </summary>
        public void Restore()
        {
            _rootNodeView.Restore();
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
        public NodeView FindNode(ScreenVector zs)
        {
            return _rootNodeView.FindNode(zs);
        }

        /// <summary> Performs the specified action on each child node of the node with the specified coordinates.
        /// </summary>
        /// <param name="zs">The given screen coordinate.</param>
        /// <param name="action">The <see cref="System.Action<T>"></see> delegate to perform on each node.</param>
        public void ForEachNode(ScreenVector zs, Action<NodeView> action, Action<NodeView> actionElse)
        {
            _rootNodeView.ForEach(zs, action, actionElse);
        }


        /// <summary> Maps a <see cref="IHtNode"/> to a <see cref="HtDrawNode"/>.
        /// Used for backwards finding a <see cref="HtDrawNode"/> instance for a given
        /// <see cref="IHtNode"/>.
        /// </summary>
        /// <param name="htNode">The <see cref="IHtNode"/></param>
        /// <param name="drawNode">the <see cref="HtDrawNode"/> for the given <see cref="IHtNode"/></param>
        public void MapNode(INode htNode, NodeView drawNode)
        {
            _nodeViewMap.Add(htNode, drawNode);
        }

        /// <summary> Finds a <see cref="HtDrawNode"/> for a given <see cref="IHtNode"/>.
        /// </summary>
        /// <param name="htNode">the <see cref="IHtNode"/> for which we want to find the <see cref="HtDrawNode"/>.</param>
        /// <returns>The <see cref="HtDrawNode"/> for the given <see cref="IHtNode"/>.</returns>
        public NodeView FindDrawNode(INode htNode)
        {
            NodeView __drawNode = _nodeViewMap[htNode];
            return __drawNode;
        }

        #endregion
    }    
}
