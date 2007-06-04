using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;

namespace HyperTreeControl
{
    public class HtDrawNodeComposite : HtDrawNode
    {
        private HtModelNodeComposite node = null; // encapsulated HtModelNode
        private List<HtDrawNode> children = null; // children of this node
        private Dictionary<HtDrawNode, HtGeodesic> geodesics = null; // geodesics linking the children


        /* --- Constructor --- */

        /**
         * Constructor.
         *
         * @param father    the father of this node
         * @param node      the encapsulated HTModelNode
         * @param model     the drawing model
         */
        public HtDrawNodeComposite(HtDrawNodeComposite father, HtModelNodeComposite node, HtDraw model)
            : base(father, node, model)
        {

            this.node = node;
            this.children = new List<HtDrawNode>();
            this.geodesics = new Dictionary<HtDrawNode, HtGeodesic>();

            HtModelNode childNode = null;
            HtDrawNode child = null;
            HtDrawNode brother = null;
            bool first = true;
            bool second = false;
            foreach (HtModelNode __childNode in node.Children)
            {
                if (childNode.IsLeaf)
                {
                    child = new HtDrawNode(this, childNode, model);
                }
                else
                {
                    child = new HtDrawNodeComposite(this, (HtModelNodeComposite)childNode, model);
                }
                addChild(child);
                if (first)
                {
                    brother = child;
                    first = false;
                    second = true;
                }
                else if (second)
                {
                    child.Brother = brother;
                    brother.Brother = child;
                    brother = child;
                    second = false;
                }
                else
                {
                    child.Brother = brother;
                    brother = child;
                }
            }
        }


        /* --- Children --- */

        /**
         * Returns the children of this node, 
         * in an Enumeration.
         *
         * @return    the children of this node
         */
        public List<HtDrawNode> Children
        {
            get
            {
                return this.children;
            }
        }

        /** 
         * Adds the HtDrawNode as a children.
         *
         * @param child    the child
         */
        void addChild(HtDrawNode child)
        {
            children.Add(child);
            geodesics.Add(child, new HtGeodesic(Coordinates, child.Coordinates));
        }


        /* --- Screen Coordinates --- */

        /**
         * Refresh the screen coordinates of this node
         * and recurse on children.
         *
         * @param sOrigin   the origin of the screen plane
         * @param sMax      the (xMax, yMax) point in the screen plane
         */
        public override void RefreshScreenCoordinates(HtCoordS sOrigin, HtCoordS sMax)
        {
            base.RefreshScreenCoordinates(sOrigin, sMax);

            foreach (HtDrawNode child in children)
            {
                child.RefreshScreenCoordinates(sOrigin, sMax);
                HtGeodesic geod = geodesics[child];
                if (geod != null)
                {
                    geod.RefreshScreenCoordinates(sOrigin, sMax);
                }

            }

        }


        /* --- Drawing --- */

        /**
         * Draws the branches from this node to 
         * its children.
         *
         * @param g    the graphic context
         */
        void drawBranches(Canvas g)
        {
            foreach (HtDrawNode child in children)
            {
                HtGeodesic geod = geodesics[child];
                if (geod != null)
                {
                    geod.Draw(g);
                }
                child.DrawBranches(g);
            }

        }

        /**
         * Draws this node.
         *
         * @param g    the graphic context
         */
        public override void DrawNodes(Canvas g)
        {
            if (fastMode == false)
            {
                base.DrawNodes(g);

                foreach (HtDrawNode child in children)
                {
                    child.DrawNodes(g);
                }
            }
        }

        /**
         * Returns the minimal distance between this node
         * and his father, his brother, and his children.
         *
         * @return    the minimal distance
         */
        public override int GetSpace()
        {
            int space = base.GetSpace();

            if (children.Count > 0)
            {
                HtDrawNode child = children[0];
                HtCoordS zC = child.ScreenCoordinates;
                int dC = zs.GetDistance(zC);

                if (space == -1)
                {
                    return dC;
                }
                else
                {
                    return Math.Min(space, dC);
                }
            }
            else
            {
                return space;
            }
        }


        /* --- Translation --- */

        /**
         * Translates this node by the given vector.
         *
         * @param t    the translation vector
         */
        public override void Translate(HtCoordE t)
        {
            base.Translate(t);

            foreach (HtDrawNode child in children)
            {
                child.Translate(t);
                HtGeodesic geod = geodesics[child];
                if (geod != null)
                {
                    geod.Rebuild();
                }

            }

        }

        /**
         * Transform this node by the given transformation.
         *
         * @param t    the transformation
         */
        public override void Transform(HtTransformation t)
        {
            base.Transform(t);

            foreach (HtDrawNode child in children)
            {
                child.Transform(t);
                HtGeodesic geod = geodesics[child];
                if (geod != null)
                {
                    geod.Rebuild();
                }
            }
        }

        /**
         * Ends the translation.
         */
        public override void EndTranslation()
        {
            base.EndTranslation();

            foreach (HtDrawNode child in children)
            {
                child.EndTranslation();
            }
        }

        /**
         * Restores the hyperbolic tree to its origin.
         */
        public override void Restore()
        {
            base.Restore();

            foreach (HtDrawNode child in children)
            {
                child.Restore();
                HtGeodesic geod = geodesics[child];
                if (geod != null)
                {
                    geod.Rebuild();
                }
            }

        }

        /**
         * Sets the fast mode, where nodes are no more drawed.
         *
         * @param mode    setting on or off.
         */
        public override bool FastMode
        {
            get
            {
                return base.FastMode;
            }
            set
            {
                foreach (HtDrawNode child in children)
                {
                    child.FastMode = value;
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
        public override HtDrawNode FindNode(HtCoordS zs)
        {
            HtDrawNode result = base.FindNode(zs);
            if (result != null)
            {
                return result;
            }
            else
            {
                foreach (HtDrawNode child in children)
                {
                    result = child.FindNode(zs);
                    if (result != null)
                    {
                        return result;
                    }
                }

                return null;
            }
        }


        /* --- ToString --- */

        /**
         * Returns a string representation of the object.
         *
         * @return    a String representation of the object
         */
        public override string ToString()
        {
            string result = base.ToString();
            result += "\n\tChildren :";
            foreach (HtDrawNode child in children)
            {
                result += "\n\t-> " + child.Name;
            }

            return result;
        }

    }
}
