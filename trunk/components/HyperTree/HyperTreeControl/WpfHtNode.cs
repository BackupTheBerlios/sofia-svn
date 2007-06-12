using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Media;
using System.Windows.Controls;
using System.Windows;
using System.Windows.Markup;

namespace HyperTreeControl
{
    public abstract class WpfHtNode: Label, IHtNode
    {
        #region IHtNode Members

        public virtual List<IHtNode> Children
        {
            get { throw new Exception("The method or operation is not implemented."); }
        }

        public virtual bool IsLeaf
        {
            get { throw new Exception("The method or operation is not implemented."); }
        }

        public virtual string Name
        {
            get { throw new Exception("The method or operation is not implemented."); }
        }

        public System.Windows.Media.Color Color
        {
            get { return Colors.White; }
        }

        public int Size
        {
            get { return 1; }
        }

        public int BorderSize
        {
            get { return 1; }
        }

        public System.Windows.Controls.Image Image
        {
            get { return null; }
        }

        public virtual void Add(IAddChild parent, IHtNode node)
        {
            Children.Add(node);
            parent.AddChild(this);
        }

        #endregion
        
    }
}
