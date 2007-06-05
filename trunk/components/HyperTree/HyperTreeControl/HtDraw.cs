using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Windows.Controls;
using System.Windows;
using System.Windows.Threading;
using System.Windows.Media;

namespace HyperTreeControl
{
    public class HtDraw
    {
        #region fields

        public static readonly int NBR_FRAMES = 10; // number of intermediates animation frames

        private static HtModel _model = null;  // the tree model
        private static IHtView _view = null;  // the view using this drawing model
        private static HtDrawNode _drawRoot = null;  // the root of the drawing tree 
        private static double[] _ray = null;

        private HtCoordS sOrigin = null;  // origin of the screen plane
        private HtCoordS sMax = null;  // max point in the screen plane 

        private bool _fastMode = false; // fast mode
        private Dictionary<IHtNode, HtDrawNode> _drawToHTNodeMap;

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
            HtModelNode root = model.Root;
            sOrigin = new HtCoordS();
            sMax = new HtCoordS();

            _ray = new double[4];
            _ray[0] = model.Length;

            for (int i = 1; i < _ray.Length; i++)
            {
                _ray[i] = (_ray[0] + _ray[i - 1]) / (1 + (_ray[0] * _ray[i - 1]));
            }

            if (root.IsLeaf)
            {
                _drawRoot = new HtDrawNode(null, root, this);
            }
            else
            {
                _drawRoot = new HtDrawNodeComposite(null, (HtModelNodeComposite)root, this);
            }
        }

        #endregion

        #region Internal classes accessors

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

        #endregion

        #region Screen coordinates

        /// <summary>
        /// Refresh the screen coordinates of the drawing tree.
        /// </summary>
        public void RefreshScreenCoordinates()
        {
            Rect insets = _view.Insets;
            sMax.X = (int)((_view.Width - insets.Left - insets.Right) / 2);
            sMax.Y = (int)((_view.Height - insets.Top - insets.Bottom) / 2);
            sOrigin.X = sMax.X + (int)insets.Left;
            sOrigin.Y = sMax.Y + (int)insets.Top;
            _drawRoot.RefreshScreenCoordinates(sOrigin, sMax);
        }

        /// <summary>
        /// Gets the origin of the screen plane.
        /// </summary>
        public HtCoordS SOrigin
        {
            get
            {
                return sOrigin;
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
                return sMax;
            }
        }

        #endregion

        #region Drawing

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

        #endregion

        #region Translation

        /// <summary> Translates the hyperbolic tree by the given vector.
        /// </summary>
        /// <param name="zs">The first coordinates.</param>
        /// <param name="ze">The second coordinates.</param>
        public static void Translate(HtCoordE zs, HtCoordE ze)
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
        public static void EndTranslation()
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
            AnimThread __t = new AnimThread(node);
            __t.Start();
        }

        /// <summary> Restores the hyperbolic tree to its origin.
        /// </summary>
        public void Restore()
        {
            _drawRoot.Restore();
            _view.Repaint();
        }

        /// <summary> Sets the fast mode, where nodes are no more drawed.
        /// </summary>
        public bool FastMode
        {
            set
            {
                if (value != _fastMode)
                {
                    _fastMode = value;
                    _drawRoot.FastMode = value;
                    if (value == false)
                    {
                        _view.Repaint();
                    }
                }
            }
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
        private HtDrawNode FindDrawNode(IHtNode htNode)
        {
            HtDrawNode __drawNode = _drawToHTNodeMap[htNode];
            return __drawNode;
        }

        #endregion
    }        

    #region Inner animation thread

    internal delegate void AnimDelegate();

    /// <summary>
    /// Used for the Dispatcher delegate invoke
    /// </summary>
    internal interface IRunnable
    {
        void Run();
    }

    /// <summary> The AnimThread class implements the thread that do the animation
    /// when clicking on a node.
    /// </summary>
    internal class AnimThread
    {

        private HtDrawNode _node = null; // node to put at the origin
        private IRunnable _task = null; // translation task

        /// <summary> Constructor.
        /// </summary>
        /// <param name="node">The node to put at the origin.</param>
        public AnimThread(HtDrawNode node)
        {
            _node = node;
        }

        /// <summary> Starts the animation.
        /// </summary>
        public void Start()
        {
            ThreadPool.QueueUserWorkItem(new WaitCallback(this.Run));
        }

        /// <summary> Delegate for the  <see cref="System.Windows.Threading.Dispatcher"/> event queue.
        /// </summary>
        /// <param name="stateInfo"></param>
        private void Run(object stateInfo)
        {

            HtCoordE __zn = _node.OldCoordinates;
            HtCoordE __zf = new HtCoordE();

            int __frames = HtDraw.NBR_FRAMES;
            int __nodes = HtDraw.Model.NumberOfNodes;

            double __d = __zn.D();
            for (int __i = 0; __i < HtDraw.Ray.Length; __i++)
            {
                if (__d > HtDraw.Ray[__i])
                {
                    __frames += HtDraw.NBR_FRAMES / 2;
                }
            }

            double __factorX = __zn.X / __frames;
            double __factorY = __zn.Y / __frames;

            for (int __i = 1; __i < __frames; __i++)
            {
                __zf.X = __zn.X - (__i * __factorX);
                __zf.Y = __zn.Y - (__i * __factorY);
                _task = new TranslateThread(__zn, __zf);
                try
                {
                    Dispatcher.CurrentDispatcher.Invoke(DispatcherPriority.Render, new AnimDelegate(_task.Run));
                }
                catch (Exception)
                {
                    //TODO : throw more convenient exception
                    throw;
                }
            }

            __zf.X = 0.0;
            __zf.Y = 0.0;
            _task = new LastTranslateThread(__zn, __zf);
            try
            {
                Dispatcher.CurrentDispatcher.Invoke(DispatcherPriority.Render, new AnimDelegate(_task.Run));
            }
            catch (Exception)
            {
                //TODO : throw more convenient exception
                throw;
            }
        }

    }

    /// <summary> Part of the animation thread.
    /// </summary>
    internal class TranslateThread : IRunnable
    {

        HtCoordE zStart = null;
        HtCoordE zEnd = null;

        public TranslateThread(HtCoordE z1, HtCoordE z2)
        {
            zStart = z1;
            zEnd = z2;
        }

        /// <summary> Implementation of <see cref="IRunnable"/>.
        /// </summary>
        public void Run()
        {
            HtDraw.Translate(zStart, zEnd);
            HtDraw.View.Repaint();
        }
    }

    /// <summary> Part of the animation thread.    
    /// </summary>
    internal class LastTranslateThread : IRunnable
    {

        HtCoordE zStart = null;
        HtCoordE zEnd = null;

        public LastTranslateThread(HtCoordE z1, HtCoordE z2)
        {
            zStart = z1;
            zEnd = z2;
        }

        /// <summary> Implementation of <see cref="IRunnable"/>.
        /// </summary>
        public void Run()
        {
            HtDraw.Translate(zStart, zEnd);
            HtDraw.EndTranslation();
            HtDraw.View.Repaint();
            HtDraw.View.StartMouseListening();
        }
    }

    #endregion
}
