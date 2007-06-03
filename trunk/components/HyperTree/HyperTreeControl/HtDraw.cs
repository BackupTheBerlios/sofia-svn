using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Windows.Controls;

namespace HyperTreeControl
{
    class HtDraw
    {
        private static int NBR_FRAMES = 10; // number of intermediates animation frames

        private HtModel model = null;  // the tree model
        private IHtView view = null;  // the view using this drawing model
        private HtDrawNode drawRoot = null;  // the root of the drawing tree 

        private HtCoordS sOrigin = null;  // origin of the screen plane
        private HtCoordS sMax = null;  // max point in the screen plane 

        private double[] ray = null;

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
        HtDraw(HtModel model, IHtView view)
        {

            // initialize mapping
            drawToHTNodeMap = new Dictionary<IHtNode, HtDrawNode>();

            this.view = view;
            this.model = model;
            HTModelNode root = model.getRoot();
            sOrigin = new HTCoordS();
            sMax = new HtCoordS();

            ray = new double[4];
            ray[0] = model.Length();

            for (int i = 1; i < ray.length; i++)
            {
                ray[i] = (ray[0] + ray[i - 1]) / (1 + (ray[0] * ray[i - 1]));
            }

            if (root.isLeaf())
            {
                drawRoot = new HtDrawNode(null, root, this);
            }
            else
            {
                drawRoot = new HtDrawNodeComposite(null, (HtModelNodeComposite)root, this);
            }

            return;
        }


        /* --- Screen coordinates --- */

        /**
         * Refresh the screen coordinates of the drawing tree.
         */
        void refreshScreenCoordinates()
        {
            Insets insets = view.getInsets();
            sMax.x = (view.getWidth() - insets.left - insets.right) / 2;
            sMax.y = (view.getHeight() - insets.top - insets.bottom) / 2;
            sOrigin.x = sMax.x + insets.left;
            sOrigin.y = sMax.y + insets.top;
            drawRoot.refreshScreenCoordinates(sOrigin, sMax);
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


        /* --- Drawing --- */

        /**
         * Draws the branches of the hyperbolic tree.
         *
         * @param g    the graphic context
         */
        void drawBranches(Canvas g)
        {
            drawRoot.drawBranches(g);
        }

        /**
         * Draws the nodes of the hyperbolic tree.
         *
         * @param g    the graphic context
         */
        void drawNodes(Canvas g)
        {
            drawRoot.drawNodes(g);
        }


        /* --- Translation --- */

        /**
         * Translates the hyperbolic tree by the given vector.
         *
         * @param t    the translation vector
         */
        void translate(HtCoordE zs, HtCoordE ze)
        {
            HtCoordE zo = new HtCoordE(drawRoot.OldCoordinates);
            zo.x = -zo.x;
            zo.y = -zo.y;
            HtCoordE zs2 = new HtCoordE(zs);
            zs2._translate(zo);

            HtCoordE t = new HtCoordE();
            double de = ze._d2();
            double ds = zs2._d2();
            double dd = 1.0 - de * ds;
            t.x = (ze.x * (1.0 - ds) - zs2.x * (1.0 - de)) / dd;
            t.y = (ze.y * (1.0 - ds) - zs2.y * (1.0 - de)) / dd;

            if (t.isValid())
            {
                HtTransformation to = new HtTransformation();
                to._composition(zo, t);

                drawRoot._transform(to);
                view.repaint();
            }
        }

        /**
         * Signal that the translation ended.
         */
        void endTranslation()
        {
            drawRoot.endTranslation();
        }

        /**
         * Translate the hyperbolic tree so that the given node 
         * is put at the origin of the hyperbolic tree.
         *
         * @param node    the given HTDrawNode
         */
        void translateToOrigin(HtDrawNode node)
        {
            view.stopMouseListening();
            AnimThread t = new AnimThread(node);
            t.start();
        }

        /**
         * Restores the hyperbolic tree to its origin.
         */
        void restore()
        {
            drawRoot.restore();
            view.repaint();
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
                    drawRoot.FastMode = value;
                    if (value == false)
                    {
                        view.repaint();
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
        HtDrawNode findNode(HtCoordS zs)
        {
            return drawRoot._findNode(zs);
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

            HtDrawNode drawNode = (HtDrawNode)drawToHTNodeMap.get(htNode);
            return drawNode;
        }


        /* --- Inner animation thread --- */

        /**
         * The AnimThread class implements the thread that do the animation
         * when clicking on a node.
         */
        class AnimThread
        {

            private HtDrawNode node = null; // node to put at the origin
            private Thread tTask = null; // translation task

            /**
             * Constructor.
             *
             * @param node    the node to put at the origin
             */
            AnimThread(HtDrawNode node)
            {
                this.node = node;
                return;
            }

            /**
             * Do the animation.
             */
            public void run()
            {
                HtCoordE zn = node.OldCoordinates;
                HtCoordE zf = new HtCoordE();

                int frames = NBR_FRAMES;
                int nodes = model.NumberOfNodes;

                double d = zn._d();
                for (int i = 0; i < ray.length; i++)
                {
                    if (d > ray[i])
                    {
                        frames += NBR_FRAMES / 2;
                    }
                }

                double factorX = zn.x / frames;
                double factorY = zn.y / frames;

                for (int i = 1; i < frames; i++)
                {
                    zf.x = zn.x - (i * factorX);
                    zf.y = zn.y - (i * factorY);
                    tTask = new TranslateThread(zn, zf);
                    try
                    {
                        SwingUtilities.invokeAndWait(tTask);
                    }
                    catch (Exception e)
                    {
                        throw;
                    }
                }

                zf.x = 0.0;
                zf.y = 0.0;
                tTask = new LastTranslateThread(zn, zf);
                try
                {
                    SwingUtilities.invokeAndWait(tTask);
                }
                catch (Exception e)
                {
                    throw;
                }
            }

            /* --- Inner's inner --- */

            class TranslateThread
            {

                HtCoordE zStart = null;
                HtCoordE zEnd = null;

                TranslateThread(HtCoordE z1, HtCoordE z2)
                {
                    zStart = z1;
                    zEnd = z2;
                }

                public void run()
                {
                    translate(zStart, zEnd);
                    view.repaint();
                }
            }

            class LastTranslateThread
            {

                HtCoordE zStart = null;
                HtCoordE zEnd = null;

                LastTranslateThread(HtCoordE z1, HtCoordE z2)
                {
                    zStart = z1;
                    zEnd = z2;
                }

                public void run()
                {
                    translate(zStart, zEnd);
                    endTranslation();
                    view.repaint();
                    view.startMouseListening();
                }
            }


        }

    }
}
