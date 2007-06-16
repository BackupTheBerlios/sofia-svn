using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows;

namespace HyperTreeControl
{
    public class HtDrawNodeComposite : HtDrawNode
    {
        #region fields

        private HtModelNodeComposite _node = null; // encapsulated HtModelNode
        private List<HtDrawNode> _childNodes = null; // children of this node
        private Dictionary<HtDrawNode, HtGeodesic> _geodesics = null; // geodesics linking the children

        #endregion

        #region Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="father">The father of this node.</param>
        /// <param name="node">The encapsulated <see cref="HtModelNode"/>.</param>
        /// <param name="model">The drawing model.</param>
        public HtDrawNodeComposite(HtDrawNodeComposite father, HtModelNodeComposite node, HtDraw model)
            : base(father, node, model)
        {

            _node = node;
            _childNodes = new List<HtDrawNode>();
            _geodesics = new Dictionary<HtDrawNode, HtGeodesic>();

            HtDrawNode __child = null;
            HtDrawNode __brother = null;
            bool __first = true;
            bool __second = false;
            foreach (HtModelNode __childNode in _node.Children)
            {
                if (__childNode.IsLeaf)
                {
                    __child = new HtDrawNode(this, __childNode, model);
                }
                else
                {
                    __child = new HtDrawNodeComposite(this, (HtModelNodeComposite)__childNode, model);
                }
                this.AddChild(__child);
                if (__first)
                {
                    __brother = __child;
                    __first = false;
                    __second = true;
                }
                else if (__second)
                {
                    __child.Brother = __brother;
                    __brother.Brother = __child;
                    __brother = __child;
                    __second = false;
                }
                else
                {
                    __child.Brother = __brother;
                    __brother = __child;
                }
            }
        }

        #endregion

        #region Children

        /// <summary> Gets the children of this node.
        /// </summary>
        public List<HtDrawNode> ChildNodes
        {
            get
            {
                return _childNodes;
            }
        }

        /// <summary> Adds the HtDrawNode as a children.
        /// </summary>
        /// <param name="child">The child.</param>
        private void AddChild(HtDrawNode child)
        {
            _childNodes.Add(child);
            _geodesics.Add(child, new HtGeodesic(Coordinates, child.Coordinates));
        }

        #endregion

        #region Screen Coordinates

        /// <summary> Refresh the screen coordinates of this node
        /// and recurse on children.
        /// </summary>
        /// <param name="sOrigin">The origin of the screen plane.</param>
        /// <param name="sMax">The (xMax, yMax) point in the screen plane.</param>
        public override void RefreshScreenCoordinates(HtCoordS sOrigin, HtCoordS sMax)
        {
            base.RefreshScreenCoordinates(sOrigin, sMax);

            foreach (HtDrawNode child in _childNodes)
            {
                child.RefreshScreenCoordinates(sOrigin, sMax);
                HtGeodesic geod = _geodesics[child];
                if (geod != null)
                {
                    geod.RefreshScreenCoordinates(sOrigin, sMax);
                }

            }

        }

        #endregion

        #region Drawing

        /// <summary> Draws the branches from this node to its children. 
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public override void DrawBranches(DrawingContext dc)
        {
            foreach (HtDrawNode child in _childNodes)
            {
                HtGeodesic __geod = _geodesics[child];
                if (__geod != null)
                {
                    __geod.Draw(dc);
                }
                child.DrawBranches(dc);
            }
        }

        /// <summary> Draw this node.
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public override void DrawNodes(DrawingContext dc)
        {
            base.DrawNodes(dc);

            foreach (HtDrawNode __child in _childNodes)
            {
                __child.DrawNodes(dc);
            }
        }

        /// <summary> Returns the minimal distance between this node
        /// and his father and his brother.
        /// </summary>
        /// <returns>The minimal distance.</returns>
        public override int GetSpace()
        {
            int __space = base.GetSpace();

            if (_childNodes.Count > 0)
            {
                HtDrawNode __child = _childNodes[0];
                HtCoordS __zC = __child.ScreenCoordinates;
                int __dC = _zs.GetDistance(__zC);

                if (__space == -1)
                {
                    return __dC;
                }
                else
                {
                    return Math.Min(__space, __dC);
                }
            }
            else
            {
                return __space;
            }
        }

        #endregion

        #region Translation

        /// <summary> Translates this node by the given vector. 
        /// </summary>
        /// <param name="t">The translation vector.</param>
        public override void Translate(HtCoordE t)
        {
            base.Translate(t);

            foreach (HtDrawNode __child in _childNodes)
            {
                __child.Translate(t);
                HtGeodesic __geod = _geodesics[__child];
                if (__geod != null)
                {
                    __geod.Rebuild();
                }
            }
        }

        /// <summary> Transform this node by the given transformation.
        /// </summary>
        /// <param name="t">The transformation.</param>
        public override void Transform(HtTransformation t)
        {
            base.Transform(t);

            foreach (HtDrawNode __child in _childNodes)
            {
                __child.Transform(t);
                HtGeodesic __geod = _geodesics[__child];
                if (__geod != null)
                {
                    __geod.Rebuild();
                }
            }
        }

        /// <summary> Ends the translation.
        /// </summary>
        public override void EndTranslation()
        {
            base.EndTranslation();

            foreach (HtDrawNode __child in _childNodes)
            {
                __child.EndTranslation();
            }
        }

        /// <summary> Restores the hyperbolic tree to its origin.
        /// </summary>
        public override void Restore()
        {
            base.Restore();

            foreach (HtDrawNode __child in _childNodes)
            {
                __child.Restore();
                HtGeodesic __geod = _geodesics[__child];
                if (__geod != null)
                {
                    __geod.Rebuild();
                }
            }
        }

        #endregion

        #region Node searching

        /// <summary> Returns the node (if any) whose screen coordinates' zone
        /// contains thoses given in parameters.
        /// </summary>
        /// <param name="zs">The given screen coordinate.</param>
        /// <returns>the searched <see cref="HtDrawNode"/> if found; <code>null</code> otherwise.</returns>
        public override HtDrawNode FindNode(HtCoordS zs)
        {
            HtDrawNode __result = base.FindNode(zs);
            if (__result != null)
            {
                return __result;
            }
            else
            {
                foreach (HtDrawNode __child in _childNodes)
                {
                    __result = __child.FindNode(zs);
                    if (__result != null)
                    {
                        return __result;
                    }
                }

                return null;
            }
        }

        #endregion

        #region ToString

        /// <summary> Returns a string representation of the object.
        /// </summary>
        /// <returns>A string representation of the object.</returns>
        public override string ToString()
        {
            string __result = base.ToString();
            __result += "\n\tChildren :";
            foreach (HtDrawNode __child in _childNodes)
            {
                __result += "\n\t-> " + __child.NodeName;
            }

            return __result;
        }

        #endregion
    }
}
