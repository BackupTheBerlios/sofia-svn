using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Input;
using System.Windows;

namespace HyperTreeControl
{
    /// <summary> The Controller class manage the action on the hypertree :
    /// drag of a node, etc.
    /// </summary>
    public class Controller
    {
        #region fields

        private Renderer _model = null; //the drawing model

        private EuclidianVector _startPoint = null; // starting point of dragging
        private EuclidianVector _endPoint = null; // ending point of dragging
        private ScreenVector _clickPoint = null; // clicked point    
        private bool _statusButtonDown = false;

        #endregion

        #region ctor

        /// <summary> Initialize a new instance of the <see cref="Controller"/> class.
        /// </summary>
        /// <param name="model"></param>
        public Controller(Renderer model)
        {
            _model = model;
            _startPoint = new EuclidianVector();
            _endPoint = new EuclidianVector();
            _clickPoint = new ScreenVector();
        }
        #endregion

        #region Mouse handling

        /// <summary> Gets the mouse position relatively to the sender of a mouse event.
        /// </summary>
        /// <param name="sender">The sender of the mouse event.</param>
        /// <param name="e">The <see cref="MouseEventArgs"/> passed to the mouse event handler.</param>
        /// <returns>The <see cref="ScreenVector"/> according to mouse position.</returns>
        private ScreenVector GetPosition(object sender, MouseEventArgs e)
        {
            IInputElement __relativeTo = sender as IInputElement;
            if (__relativeTo != null)
            {
                return new ScreenVector((int)e.GetPosition(__relativeTo).X, (int)e.GetPosition(__relativeTo).Y);
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
                ScreenVector __p = GetPosition(sender, e);

                if (__p != null)
                {

                    if (sender is Renderer)
                    {
                        Renderer __htDraw = ((Renderer)sender);
                        NodeView __node = __htDraw.FindNode(__p);

                        if (__node != null)
                            __node.RenderIsPressed = true;
                        else
                            foreach (NodeView __n in __htDraw.Children)
                            {
                                __n.RenderIsPressed = false;
                            }

                    }

                    _startPoint.ProjectionStoE(__p.X, __p.Y, _model.SOrigin, _model.SMax);

                    _clickPoint.X = __p.X;
                    _clickPoint.Y = __p.Y;

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

            ScreenVector __p = GetPosition(sender, e);

            if (__p != null)
            {
                if (sender is Renderer)
                {
                    Renderer __htDraw = ((Renderer)sender);
                    NodeView __node = __htDraw.FindNode(__p);

                    if (__node != null)
                        __node.RenderIsPressed = false;
                    else
                        foreach (NodeView __n in __htDraw.Children)
                        {
                            __n.RenderIsPressed = false;
                        }

                    __htDraw.EndTranslation();

                    //uncomment this to allow translate on simple node click
                    if (_statusButtonDown)
                    {
                        if (__p.X - _clickPoint.X < 5 && __p.Y - _clickPoint.Y < 5)
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
                _model.Restore();
            }
            else
            {
                NodeView node = _model.FindNode(_clickPoint);
                if (node != null)
                {
                    _model.TranslateToOrigin(node);
                }
            }
        }

        /// <summary> Called when the user moves the mouse over the hyperbolic tree.
        /// Used to signal the tree translation when the left button has been pressed over a node. 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void MouseMoveHandler(object sender, MouseEventArgs e)
        {
            ScreenVector __p = this.GetPosition(sender, e);
            if (__p != null)
            {

                if (sender is Renderer)
                {
                    Renderer __htDraw = ((Renderer)sender);
                    __htDraw.ForEachNode(__p,
                          delegate(NodeView node) { node.RenderIsMouseOver = true; },
                          delegate(NodeView node) { node.RenderIsMouseOver = false; }
                    );

                    if (_statusButtonDown == true)
                    {
                        if (_startPoint.IsValid)
                        {
                            _endPoint.ProjectionStoE(__p.X, __p.Y, _model.SOrigin, _model.SMax);
                            if (_endPoint.IsValid)
                            {
                                __htDraw.Translate(_startPoint, _endPoint);
                            }
                        }
                    }
                }
            }
        }


        #endregion
    }
}
