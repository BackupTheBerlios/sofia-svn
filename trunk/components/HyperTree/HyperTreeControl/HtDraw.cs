using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Windows.Controls;
using System.Windows;
using System.Windows.Threading;

namespace HyperTreeControl
{
    public class HtDraw
    {
        public static int NBR_FRAMES = 10; // number of intermediates animation frames

        private static HtModel _model = null;  // the tree model
        private static IHtView _view = null;  // the view using this drawing model
        private static HtDrawNode _drawRoot = null;  // the root of the drawing tree 

        private HtCoordS sOrigin = null;  // origin of the screen plane
        private HtCoordS sMax = null;  // max point in the screen plane 

        private static double[] _ray = null;

        private bool fastMode = false; // fast mode

        /** Maps {@link HTNode}s to {@link HTDrawNode}s. */
        private Dictionary<IHtNode, HtDrawNode> drawToHTNodeMap;


        /* --- Constructor --- */

        /**
         * Constructor.
         *
         * @param model    the tree model to draw 
         * @param view     the view using this drawing model
         */
        public HtDraw(HtModel model, IHtView view)
        {

            // initialize mapping
            drawToHTNodeMap = new Dictionary<IHtNode, HtDrawNode>();

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

            return;
        }


        /* --- Screen coordinates --- */

        /**
         * Refresh the screen coordinates of the drawing tree.
         */
        void refreshScreenCoordinates()
        {
            Rect insets = _view.Insets;
            sMax.X = (int)((_view.Width - insets.Left - insets.Right) / 2);
            sMax.Y = (int)((_view.Height - insets.Top - insets.Bottom) / 2);
            sOrigin.X = sMax.X + (int)insets.Left;
            sOrigin.Y = sMax.Y + (int)insets.Top;
            _drawRoot.RefreshScreenCoordinates(sOrigin, sMax);
        }

        /**
         * Returns the origin of the screen plane.
         * WARNING : this is not a copy but the original object.
         *
         * @return    the origin of the screen plane
         */
        public HtCoordS SOrigin
        {
            get
            {
                return sOrigin;
            }
        }

        /**
         * Return the point representing the up right corner
         * of the screen plane, thus giving x and y maxima.
         * WARNING : this is not a copy but the original object.
         *
         * @return    the max point
         */
        public HtCoordS SMax
        {
            get
            {
                return sMax;
            }
        }

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

        /* --- Drawing --- */

        /**
         * Draws the branches of the hyperbolic tree.
         *
         * @param g    the graphic context
         */
        void drawBranches(Canvas g)
        {
            _drawRoot.DrawBranches(g);
        }

        /**
         * Draws the nodes of the hyperbolic tree.
         *
         * @param g    the graphic context
         */
        void drawNodes(Canvas g)
        {
            _drawRoot.DrawNodes(g);
        }


        /* --- Translation --- */

        /**
         * Translates the hyperbolic tree by the given vector.
         *
         * @param t    the translation vector
         */
        public static void Translate(HtCoordE zs, HtCoordE ze)
        {
            HtCoordE zo = new HtCoordE(_drawRoot.OldCoordinates);
            zo.X = -zo.X;
            zo.Y = -zo.Y;
            HtCoordE zs2 = new HtCoordE(zs);
            zs2.Translate(zo);

            HtCoordE t = new HtCoordE();
            double de = ze.D2();
            double ds = zs2.D2();
            double dd = 1.0 - de * ds;
            t.X = (ze.X * (1.0 - ds) - zs2.X * (1.0 - de)) / dd;
            t.Y = (ze.Y * (1.0 - ds) - zs2.Y * (1.0 - de)) / dd;

            if (t.IsValid)
            {
                HtTransformation to = new HtTransformation();
                to.Composition(zo, t);

                _drawRoot.Transform(to);
                _view.Repaint();
            }
        }

        /**
         * Signal that the translation ended.
         */
        public static void EndTranslation()
        {
            _drawRoot.EndTranslation();
        }

        /**
         * Translate the hyperbolic tree so that the given node 
         * is put at the origin of the hyperbolic tree.
         *
         * @param node    the given HTDrawNode
         */
        void translateToOrigin(HtDrawNode node)
        {
            _view.StopMouseListening();
            AnimThread t = new AnimThread(node);
            t.Start();
        }

        /**
         * Restores the hyperbolic tree to its origin.
         */
        void restore()
        {
            _drawRoot.Restore();
            _view.Repaint();
        }

        /**
         * Sets the fast mode, where nodes are no more drawed.
         *
         * @param mode    setting on or off.
         */
        public bool FastMode
        {
            set
            {
                if (value != fastMode)
                {
                    fastMode = value;
                    _drawRoot.FastMode = value;
                    if (value == false)
                    {
                        _view.Repaint();
                    }
                }
            }
        }


        /* --- Node searching --- */

        /**
         * Returns the node (if any) whose screen coordinates' zone
         * contains thoses given in parameters.
         *
         * @param zs    the given screen coordinate
         * @return      the searched HTDrawNode if found;
         *              <CODE>null</CODE> otherwise
         */
        private HtDrawNode FindNode(HtCoordS zs)
        {
            return _drawRoot.FindNode(zs);
        }


        /** Maps a {@link HTNode} to a {@link HTDrawNode}.
         * Used for backwards finding a {@link HTDrawNode} instance for a given
         * {@link HTNode}.
         * @param htNode the {@link HTNode}.
         * @param drawNode the {@link HTDrawNode} for the given {@link HTNode}.
         */
        public void MapNode(IHtNode htNode, HtDrawNode drawNode)
        {
            drawToHTNodeMap.Add(htNode, drawNode);
        }

        /** Finds a {@link HTDrawNode} for a given {@link HTNode}.
         * @param htNode the {@link HTNode} for which we want to find the
         *     {@link HTDrawNode}.
         * @return the {@link HTDrawNode} for the given {@link HTNode}.
         */
        protected HtDrawNode findDrawNode(IHtNode htNode)
        {

            HtDrawNode drawNode = drawToHTNodeMap[htNode];
            return drawNode;
        }        

    }

    /* --- Inner animation thread --- */

    internal delegate void AnimDelegate();

    internal interface IRunnable
    {
        void Run();
    }

    /**
     * The AnimThread class implements the thread that do the animation
     * when clicking on a node.
     */
    internal class AnimThread
    {

        private HtDrawNode node = null; // node to put at the origin
        private IRunnable tTask = null; // translation task

        /**
         * Constructor.
         *
         * @param node    the node to put at the origin
         */
        public AnimThread(HtDrawNode node)
        {
            this.node = node;
            return;
        }

        /**
         * Do the animation.
         */
        public void Start()
        {
            ThreadPool.QueueUserWorkItem(new WaitCallback(run));
        }

        private void run(object stateInfo)
        {
            HtCoordE zn = node.OldCoordinates;
            HtCoordE zf = new HtCoordE();

            int frames = HtDraw.NBR_FRAMES;
            int nodes = HtDraw.Model.NumberOfNodes;

            double d = zn._d();
            for (int i = 0; i < HtDraw.Ray.Length; i++)
            {
                if (d > HtDraw.Ray[i])
                {
                    frames += HtDraw.NBR_FRAMES / 2;
                }
            }

            double factorX = zn.X / frames;
            double factorY = zn.Y / frames;

            for (int i = 1; i < frames; i++)
            {
                zf.X = zn.X - (i * factorX);
                zf.Y = zn.Y - (i * factorY);
                tTask = new TranslateThread(zn, zf);
                try
                {
                    Dispatcher.CurrentDispatcher.Invoke(DispatcherPriority.Render, new AnimDelegate(tTask.Run));
                }
                catch (Exception)
                {
                    throw;
                }
            }

            zf.X = 0.0;
            zf.Y = 0.0;
            tTask = new LastTranslateThread(zn, zf);
            try
            {
                Dispatcher.CurrentDispatcher.Invoke(DispatcherPriority.Render, new AnimDelegate(tTask.Run));
            }
            catch (Exception)
            {
                throw;
            }
        }

    }

    internal class TranslateThread: IRunnable
    {

        HtCoordE zStart = null;
        HtCoordE zEnd = null;

        public TranslateThread(HtCoordE z1, HtCoordE z2)
        {
            zStart = z1;
            zEnd = z2;
        }

        public void Run()
        {
            HtDraw.Translate(zStart, zEnd);
            HtDraw.View.Repaint();
        }
    }

    internal class LastTranslateThread: IRunnable
    {

        HtCoordE zStart = null;
        HtCoordE zEnd = null;

        public LastTranslateThread(HtCoordE z1, HtCoordE z2)
        {
            zStart = z1;
            zEnd = z2;
        }

        public void Run()
        {
            HtDraw.Translate(zStart, zEnd);
            HtDraw.EndTranslation();
            HtDraw.View.Repaint();
            HtDraw.View.StartMouseListening();
        }
    }

}
