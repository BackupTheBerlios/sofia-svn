using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Media;
using System.Windows.Controls;
using System.Windows;
using System.Windows.Markup;

namespace HyperTreeControl
{
    /// <summary> Default node.
    /// </summary>
    public abstract class NodeBase : StackPanel, INode
    {
        #region fields

        private string _name = null; // name of the node
        private bool _leaf = true; // is a leaf
        private List<INode> _children = null; // children

        #endregion

        #region ctor

        public NodeBase(string name)
        {
            _name = name;
            _children = new List<INode>();
        }

        public NodeBase(string name, bool leaf)
            : this(name)
        {
            _leaf = leaf;
        }

        #endregion

        #region public methods

        public void Add(INode node)
        {
            _children.Add(node);
        }

        #endregion

        #region IHtNode Members

        public virtual List<INode> ChildNodes
        {
            get { return _children; }
        }

        public virtual bool IsLeaf
        {
            get { return false; }
        }

        public virtual string NodeName
        {
            get { return _name; }
        }

        public System.Windows.Media.Color Color
        {
            get { return Colors.White; }
        }

        public int Size
        {
            get { return 5; }
        }

        public int BorderSize
        {
            get { return 1; }
        }

        public System.Windows.Controls.Image Image
        {
            get { return null; }
        }

        #endregion

    }
}
