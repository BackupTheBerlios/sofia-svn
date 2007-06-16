using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Media;
using System.Windows.Controls;
using System.Windows;
using System.Windows.Markup;

namespace HyperTreeControl
{
    public abstract class WpfHtNode : StackPanel, IHtNode
    {
        #region IHtNode Members

        public virtual List<IHtNode> ChildNodes
        {
            get { throw new Exception("The method or operation is not implemented."); }
        }

        public virtual bool IsLeaf
        {
            get { throw new Exception("The method or operation is not implemented."); }
        }

        public virtual string NodeName
        {
            get { throw new Exception("The method or operation is not implemented."); }
        }

        public System.Windows.Media.Color Color
        {
            get { return Colors.White; }
        }

        public int Size
        {
            get { return 4; }
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
