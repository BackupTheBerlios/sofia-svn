using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Input;

namespace HyperTreeControl
{
    public class HtAction
    {
        #region fields

        private HtDraw model = null; //the drawing model

        private HtCoordE startPoint = null; // starting point of dragging
        private HtCoordE endPoint = null; // ending point of dragging
        private HtCoordS clickPoint = null; // clicked point    

        #endregion

        #region ctor

        public HtAction(HtDraw model)
        {
            this.model = model;
            startPoint = new HtCoordE();
            endPoint = new HtCoordE();
            clickPoint = new HtCoordS();
        }
        #endregion

        #region Mouse handling

        /* --- MouseAdapter --- */

        /**
         * Called when a user pressed the mouse button
         * on the hyperbolic tree.
         * Used to get the starting point of the drag.
         *
         * @param e    the MouseEvent generated when clicking
         */
        public void MousePressed(MouseEventArgs e)
        {
            if (e.isShiftDown())
            {
                model.fastMode(true);
            }
            startPoint.projectionStoE(e.getX(), e.getY(),
                                      model.getSOrigin(),
                                      model.getSMax());
        }

        /**
         * Called when a user release the mouse button
         * on the hyperbolic tree.
         * Used to signal the end of the translation.
         *
         * @param e    not used here
         */
        public void mouseReleased(MouseEvent e)
        {
            model.fastMode(false);
            model.endTranslation();
        }

        /**
         * Called when a user clicked on the hyperbolic tree.
         * Used to put the corresponding node (if any) at the
         * center of the hyperbolic tree.
         *
         * @param e    the MouseEvent generated when clicking
         */
        public void mouseClicked(MouseEvent e)
        {
            if (e.isShiftDown())
            {
                model.restore();
            }
            else
            {
                clickPoint.x = e.getX();
                clickPoint.y = e.getY();

                HTDrawNode node = model.findNode(clickPoint);
                if (node != null)
                {
                    model.translateToOrigin(node);
                }
            }
        }


        /* --- MouseMotionListener --- */

        /**
         * Called when a used drag the mouse on the hyperbolic tree.
         * Used to translate the hypertree, thus moving the focus.
         *
         * @param e    the MouseEvent generated when draging
         */
        public void mouseDragged(MouseEvent e)
        {
            if (startPoint.isValid())
            {
                endPoint.projectionStoE(e.getX(), e.getY(),
                                        model.getSOrigin(),
                                        model.getSMax());
                if (endPoint.isValid())
                {
                    model.translate(startPoint, endPoint);
                }
            }
        }

        /**
         * Called when the mouse mouve into the hyperbolic tree.
         * Not used here.
         *
         * @param e    the MouseEvent generated when mouving
         */
        public void mouseMoved(MouseEvent e) { }

        #endregion
    }
}
