using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Media;

namespace HyperTreeControl
{
    class HtNodeLabel : Label
    {
        private HtDrawNode node = null;  // represented node
        private int x = 0;     // x up-left corner
        private int y = 0;     // y up-left corner
        private int width = 0;     // width of the label
        private int height = 0;     // height of the label
        private bool active = false; // should be drawed ?


        /* ---  Constructor --- */

        /**
         * Constructor.
         * 
         * @param node    the represented node
         */
        public HtNodeLabel(HtDrawNode node)
        {
            this.node = node;
        }


        /* --- Draw --- */

        /**
         * Draw this label, if there is enought space.
         *
         * @param g    the graphic context
         */
        public void Draw(Canvas g)
        {
            int space = node.GetSpace();
            if (space >= Height)
            {
                active = true;
            }
            else
            {
                active = false;
            }
        }


        /* --- Zone containing --- */

        /**
         * Is the given HTCoordS within this label ?
         *
         * @return    <CODE>true</CODE> if it is,
         *            <CODE>false</CODE> otherwise
         */
        public bool Contains(HtCoordS zs)
        {
            if (active)
            {
                if ((zs.X >= x) && (zs.X <= (x + width)) &&
                    (zs.Y >= y) && (zs.Y <= (y + height)))
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
                return node.ScreenCoordinates.Contains(zs);
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
            string result = "label of " + node.Name +
                            "\n\tx = " + x + " : y = " + y +
                            "\n\tw = " + width + " : h = " + height;
            return result;
        }

    }
}
