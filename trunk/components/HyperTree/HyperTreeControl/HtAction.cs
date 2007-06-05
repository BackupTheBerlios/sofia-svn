using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Input;
using System.Windows;

namespace HyperTreeControl
{
    public class HtAction
    {
        #region fields

        private HtDraw model = null; //the drawing model

        private HtCoordE startPoint = null; // starting point of dragging
        private HtCoordE endPoint = null; // ending point of dragging
        private HtCoordS clickPoint = null; // clicked point    

        private bool _statusClicked = false;

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

        private HtCoordS GetPosition(object sender, MouseButtonEventArgs e)
        {
            IInputElement __relativeTo = sender as IInputElement;
            if (__relativeTo != null)
            {
                return new HtCoordS((int)e.GetPosition(__relativeTo).X, (int)e.GetPosition(__relativeTo).Y);
            }
            else
            {
                return null;
            }
        }

        /// <summary> Called when a user pressed the mouse button on the hyperbolic tree.
        /// Used to get the starting point of the drag.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void MouseDownHandler(object sender, MouseButtonEventArgs e)
        {
            if (e.MiddleButton == MouseButtonState.Pressed)
                model.FastMode = true;

            HtCoordS __p = GetPosition(sender, e);

            if (__p != null)
            {
                startPoint.ProjectionStoE(__p.X, __p.Y, model.SOrigin, model.SMax);

                _statusClicked = true;
                clickPoint.X = __p.X;
                clickPoint.Y = __p.Y;
            }
        }

        /// <summary> Called when a user release the mouse button on the hyperbolic tree.
        /// Used to signal the end of the translation.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void MouseUpHandler(object sender, MouseButtonEventArgs e)
        {
            model.FastMode = false;
            HtDraw.EndTranslation();

             HtCoordS __p = GetPosition(sender, e);

            if (__p != null)
            {
                if (_statusClicked)
                {
                    _statusClicked = false;
                    if (__p.X - clickPoint.X < 5 && __p.Y - clickPoint.Y < 5)
                    {
                        this.MouseClicked(sender, e);
                    }
                    else
                    {
                        this.MouseDragged(sender, e);

                    }
                }
            }
        }

        /// <summary> Called when a user clicked on the hyperbolic tree.
        /// Used to put the corresponding node (if any) at the center of the hyperbolic tree.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void MouseClicked(object sender, MouseButtonEventArgs e)
        {

            if (e.MiddleButton == MouseButtonState.Pressed)
            {
                model.Restore();
            }
            else
            {
                HtDrawNode node = model.FindNode(clickPoint);
                if (node != null)
                {
                    model.TranslateToOrigin(node);
                }
            }
        }

        /// <summary> Called when a used drag the mouse on the hyperbolic tree.
        /// Used to translate the hypertree, thus moving the focus.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void MouseDragged(object sender, MouseButtonEventArgs e)
        {
            HtCoordS __p = GetPosition(sender, e);

            if (__p != null)
            {
                if (startPoint.IsValid)
                {
                    endPoint.ProjectionStoE(__p.X, __p.Y, model.SOrigin, model.SMax);
                    if (endPoint.IsValid)
                    {
                        HtDraw.Translate(startPoint, endPoint);
                    }
                }
            }
        }


        #endregion
    }
}
