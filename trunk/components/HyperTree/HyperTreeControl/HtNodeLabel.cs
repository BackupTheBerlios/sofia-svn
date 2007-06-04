using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Media;

namespace HyperTreeControl
{
    class HtNodeLabel
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
        }

        #endregion

        #region Draw 

        /// <summary> Draw this control, if there is enought space.
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public void Draw(DrawingContext dc)
        {
            int __space = _node.GetSpace();
            if (__space >= _height)
            {
                _active = true;
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
