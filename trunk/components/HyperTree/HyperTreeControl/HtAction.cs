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

        private bool _statusButtonDown = false;

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

        private HtCoordS GetPosition(object sender, MouseEventArgs e)
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
            if (e.OriginalSource is System.Windows.Controls.TextBlock)
            {
                HtCoordS __p = GetPosition(sender, e);

                if (__p != null)
                {
                   
                    if (sender is HtDraw)
                    {
                        HtDraw __htDraw = ((HtDraw)sender);
                        HtDrawNode __node = __htDraw.FindNode(__p);

                        if (__node != null)
                            __node.RenderIsPressed = true;
                        else
                            foreach (HtDrawNode __n in __htDraw.Children)
                            {
                                __n.RenderIsPressed = false;
                            }

                    }

                    startPoint.ProjectionStoE(__p.X, __p.Y, model.SOrigin, model.SMax);

                    clickPoint.X = __p.X;
                    clickPoint.Y = __p.Y;

                    if (e.ClickCount > 1)
                    {
                        _statusButtonDown = false;
                        MouseClicked(sender, e);
                    }
                    else
                    {
                        _statusButtonDown = true;
                    }
                }
            }
        }

        /// <summary> Called when a user release the mouse button on the hyperbolic tree.
        /// Used to signal the end of the translation.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void MouseUpHandler(object sender, MouseButtonEventArgs e)
        {
           
            HtCoordS __p = GetPosition(sender, e);

            if (__p != null)
            {
                if (sender is HtDraw)
                {
                    HtDraw __htDraw = ((HtDraw)sender);
                    HtDrawNode __node = __htDraw.FindNode(__p);

                    if (__node != null)
                        __node.RenderIsPressed = false;
                    else
                        foreach (HtDrawNode __n in __htDraw.Children)
                        {
                            __n.RenderIsPressed = false;
                        }

                    __htDraw.EndTranslation();

                    //uncomment this to allow translate to node clicked
                    if (_statusButtonDown)
                    {
                        if (__p.X - clickPoint.X < 5 && __p.Y - clickPoint.Y < 5)
                        {
                            this.MouseClicked(sender, e);
                        }
                    }
                }

                _statusButtonDown = false;
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


        public void MouseMoveHandler(object sender, MouseEventArgs e)
        {
            HtCoordS __p = this.GetPosition(sender, e);
            if (__p != null)
            {

                if (sender is HtDraw)
                {
                    HtDraw __htDraw = ((HtDraw)sender);
                    HtDrawNode __node = __htDraw.FindNode(__p);

                    /*
                    if (__node != null)
                        __node.RenderIsMouseOver = true;
                    else
                        foreach (HtDrawNode __n in __htDraw.Children)
                        {
                            __n.RenderIsMouseOver = false;
                        }
                    */
                    if (_statusButtonDown == true)
                    {
                        if (startPoint.IsValid)
                        {
                            endPoint.ProjectionStoE(__p.X, __p.Y, model.SOrigin, model.SMax);
                            if (endPoint.IsValid)
                            {
                                __htDraw.Translate(startPoint, endPoint);
                            }
                        }
                    }
                }
            }
        }


        #endregion
    }
}
