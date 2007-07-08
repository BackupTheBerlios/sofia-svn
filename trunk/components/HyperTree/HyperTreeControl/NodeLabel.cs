using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows;
using System.Windows.Shapes;
using System.Globalization;

namespace HyperTreeControl
{
    /// <summary> The NodeLabel class implements the drawed label representing a node text content.
    /// </summary>
    public class NodeLabel : Label
    {
        #region fields

        private NodeView _node = null;  // represented node

        #endregion

        #region  Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="node">The represented node.</param>
        public NodeLabel(NodeView node)
        {
            _node = node;
            this.Content = _node.NodeName;
            this.HorizontalAlignment = HorizontalAlignment.Center;
            this.VerticalAlignment = VerticalAlignment.Center;
            this.FontFamily = new FontFamily("Arial");
            this.FontSize = 11;
            this.Padding = new Thickness(1);
        }

        #endregion

    }
}
