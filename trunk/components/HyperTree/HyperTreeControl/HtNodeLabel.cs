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
    public class HtNodeLabel : Label
    {
        #region fields

        private HtDrawNode _node = null;  // represented node

        #endregion

        #region  Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="node">The represented node.</param>
        public HtNodeLabel(HtDrawNode node)
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
