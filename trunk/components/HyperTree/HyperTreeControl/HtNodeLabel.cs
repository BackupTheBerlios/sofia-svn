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
    class HtNodeLabel: Label
    {
        #region fields

        private HtDrawNode _node = null;  // represented node
        private int _x = 0;     // x up-left corner
        private int _y = 0;     // y up-left corner
        private int _width = 0;     // width of the control
        private int _height = 0;     // height of the control
        private bool _active = false; // should be drawed ?

        #endregion

        #region  Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="node">The represented node.</param>
        public HtNodeLabel(HtDrawNode node)
        {
            _node = node;

            this.Background = new SolidColorBrush(Colors.Azure);
        }

        #endregion

        #region Draw

        /// <summary> Draw this control, if there is enought space.
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public void Draw(DrawingContext dc)
        {            
            

            FontFamily __font = new FontFamily("Arial");
            FormattedText __formattedText = new FormattedText(
               _node.Name,
               CultureInfo.CurrentCulture,
               FlowDirection.LeftToRight,
               new Typeface(
                   __font,
                   FontStyles.Normal,
                   FontWeights.Light,
                   FontStretches.Normal),
               10,
               Brushes.Black
               );            
            
            int __height = (int)__formattedText.Height;
            int __width = (int)__formattedText.Width;

            _height = __height + 2 * _node.Size;
            _width = __width + 10 + 2 * _node.Size;
            HtCoordS __zs = _node.ScreenCoordinates;

            _x = __zs.X - (_width / 2) - _node.Size;
            _y = __zs.Y - (__height / 2) - _node.Size;


            int __sx = __zs.X - (__width / 2) - _node.Size;
            int __sy = _y + (int)__formattedText.OverhangTrailing + ((int)__formattedText.OverhangLeading / 2) + _node.Size;

            
            int __space = _node.GetSpace();
            if (__space >= __height)
            {
                _active = true;

                /*

                StreamGeometry __geometry = new StreamGeometry();
                __geometry.FillRule = FillRule.EvenOdd;

                using (StreamGeometryContext ctx = __geometry.Open())
                {
                    ctx.BeginFigure(new Point(_x, _y), true, true);
                    ctx.LineTo(new Point(_x + _width, _y), false, false);
                    ctx.LineTo(new Point(_x + _width, _y + _height), false, false);
                    ctx.LineTo(new Point(_x, _y + _height), false, false);
                    ctx.LineTo(new Point(_x, _y), false, false);
                }

                __geometry.Freeze();

                dc.DrawGeometry(Brushes.LightBlue, new Pen(Brushes.Black, 1), __geometry);
                Geometry __textGeometry = __formattedText.BuildGeometry(new Point(__sx, __sy));
                dc.DrawGeometry(Brushes.Black, new Pen(Brushes.Black, 0), __textGeometry);
            */
            }
            else
            {
                _active = false;
            }
            
        }

        #endregion

        #region Zone containing

        /// <summary> Is the given <see cref="HtCoordS"/> within this control ?
        /// </summary>
        /// <param name="zs">The given point.</param>
        /// <returns><code>true</code> if it is, <code>false</code> otherwise.</returns>
        public bool Contains(HtCoordS zs)
        {
            if (_active)
            {
                if ((zs.X >= _x) && (zs.X <= (_x + _width)) &&
                    (zs.Y >= _y) && (zs.Y <= (_y + _height)))
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else
            {
                return _node.ScreenCoordinates.Contains(zs);
            }
        }

        #endregion

        #region ToString

        /// <summary> Returns a string representation of the object.
        /// </summary>
        /// <returns>A string representation of the object.</returns>
        public override string ToString()
        {
            string __result = "control of " + _node.Name +
                            "\n\tx = " + _x + " : y = " + _y +
                            "\n\tw = " + _width + " : h = " + _height;
            return __result;
        }

        #endregion

    }
}
