using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows;

namespace HyperTreeControl
{
    /// <summary> The CompositeNodeView class implements the Composite design pattern for NodeView.
    /// It represents a NodeView which is not a leaf.
    /// </summary>
    public class CompositeNodeView : NodeView
    {
        #region fields

        private CompositeNodeModel _node = null; // encapsulated NodeModel
        private List<NodeView> _childNodes = null; // children of this node
        private Dictionary<NodeView, Geodesic> _geodesics = null; // geodesics linking the children

        #endregion

        #region Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="father">The father of this node.</param>
        /// <param name="node">The encapsulated <see cref="NodeModel"/>.</param>
        /// <param name="renderer">The drawing model.</param>
        public CompositeNodeView(CompositeNodeView father, CompositeNodeModel node, Renderer renderer)
            : base(father, node, renderer)
        {
            _node = node;
            _childNodes = new List<NodeView>();
            _geodesics = new Dictionary<NodeView, Geodesic>();

            NodeView __child = null;
            NodeView __brother = null;
            bool __first = true;
            bool __second = false;
            foreach (NodeModel childNode in _node.Children)
            {
                if (childNode.IsLeaf)
                {
                    __child = new NodeView(this, childNode, renderer);
                }
                else
                {
                    __child = new CompositeNodeView(this, (CompositeNodeModel)childNode, renderer);
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
        public List<NodeView> ChildNodes
        {
            get
            {
                return _childNodes;
            }
        }

        /// <summary> Adds the <see cref="NodeView"/> as a children.
        /// </summary>
        /// <param name="child">The child.</param>
        private void AddChild(NodeView child)
        {
            _childNodes.Add(child);
            _geodesics.Add(child, new Geodesic(Coordinates, child.Coordinates));
        }

        #endregion

        #region Screen Coordinates

        /// <summary> Refresh the screen coordinates of this node and recurse on children.
        /// </summary>
        /// <param name="origin">The origin of the screen plane.</param>
        /// <param name="max">The (xMax, yMax) point in the screen plane.</param>
        public override void RefreshScreenCoordinates(ScreenVector origin, ScreenVector max)
        {
            base.RefreshScreenCoordinates(origin, max);

            foreach (NodeView child in _childNodes)
            {
                child.RefreshScreenCoordinates(origin, max);
                Geodesic geod = _geodesics[child];
                if (geod != null)
                {
                    geod.RefreshScreenCoordinates(origin, max);
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
            foreach (NodeView child in _childNodes)
            {
                Geodesic __geod = _geodesics[child];
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

            foreach (NodeView child in _childNodes)
            {
                child.DrawNodes(dc);
            }
        }

        /// <summary> Returns the minimal distance between this node and his father and his brother.
        /// </summary>
        /// <returns>The minimal distance.</returns>
        public override int GetSpace()
        {
            int __space = base.GetSpace();

            if (_childNodes.Count > 0)
            {
                NodeView __child = _childNodes[0];
                ScreenVector __zC = __child.ScreenCoordinates;
                int __dC = this.ScreenCoordinates.GetDistance(__zC);

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
        public override void Translate(EuclidianVector t)
        {
            base.Translate(t);

            foreach (NodeView child in _childNodes)
            {
                child.Translate(t);
                Geodesic __geodesic = _geodesics[child];
                if (__geodesic != null)
                {
                    __geodesic.Rebuild();
                }
            }
        }

        /// <summary> Transform this node by the given transformation.
        /// </summary>
        /// <param name="t">The transformation.</param>
        public override void Transform(HyperbolicTransformation t)
        {
            base.Transform(t);

            foreach (NodeView child in _childNodes)
            {
                child.Transform(t);
                Geodesic __geodesic = _geodesics[child];
                if (__geodesic != null)
                {
                    __geodesic.Rebuild();
                }
            }
        }

        /// <summary> Ends the translation.
        /// </summary>
        public override void EndTranslation()
        {
            base.EndTranslation();

            foreach (NodeView child in _childNodes)
            {
                child.EndTranslation();
            }
        }

        /// <summary> Restores the hyperbolic tree to its origin.
        /// </summary>
        public override void Restore()
        {
            base.Restore();

            foreach (NodeView child in _childNodes)
            {
                child.Restore();
                Geodesic __geodesic = _geodesics[child];
                if (__geodesic != null)
                {
                    __geodesic.Rebuild();
                }
            }
        }

        #endregion

        #region Node searching

        /// <summary> Returns the node (if any) whose screen coordinates's zone contains thoses given in parameters.
        /// </summary>
        /// <param name="zs">The given screen coordinate.</param>
        /// <returns>The searched <see cref="NodeView"/> if found; <code>null</code> otherwise.</returns>
        public override NodeView FindNode(ScreenVector zs)
        {
            NodeView __result = base.FindNode(zs);
            if (__result != null)
            {
                return __result;
            }
            else
            {
                foreach (NodeView child in _childNodes)
                {
                    __result = child.FindNode(zs);
                    if (__result != null)
                    {
                        return __result;
                    }
                }

                return null;
            }
        }

        /// <summary> Performs the specified action on each child node of this node.
        /// </summary>
        /// <param name="zs">The given screen coordinate.</param>
        /// <param name="action">The <see cref="System.Action<T>"></see> delegate to perform on the node whose screen coordinates' zone contains thoses given in parameters.</param>
        /// <param name="actionElse">The <see cref="System.Action<T>"></see> delegate to perform on the node whose screen coordinates' zone do not contains thoses given in parameters.</param>
        public override void ForEach(ScreenVector zs, Action<NodeView> action, Action<NodeView> actionElse)
        {
            base.ForEach(zs, action, actionElse);

            foreach (NodeView child in _childNodes)
            {
                child.ForEach(zs, action, actionElse);
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
            __result += "Children :";
            foreach (NodeView __child in _childNodes)
            {
                __result += " ->" + __child.NodeName;
            }

            return __result;
        }

        #endregion
    }
}
