using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Input;
using System.Windows;
using System.Windows.Shapes;

namespace HyperTreeControl
{
    class WpfHtView : FrameworkElement, IHtView
    {

        #region fields
        private HtModel _model = null; // the tree model represented
        private HtDraw draw = null; // the drawing model
        private HtAction action = null; // action manager
        private bool fastMode = false;
        private bool longNameMode = false;
        private bool circleMode = false;
        private bool transNotCorrected = false;
        private bool quadMode = true;

        private Image image = null;

        #endregion

        #region Constructor

        /// <summary> Constructor. 
        /// </summary>
        /// <param name="model">The tree model to view.</param>
        public WpfHtView(HtModel model)
        {
            this.Margin = new System.Windows.Thickness(250);
            //TODO : inherits from Canvas in order to have the background property ?
            //this.Background = new SolidColorBrush(Colors.White);

            _model = model;
            draw = new HtDraw(_model, this);
            action = new HtAction(draw);
            this.StartMouseListening();
        }

        #endregion

        #region Node finding

        /// <summary> Returns the node containing the mouse event.
        /// </summary>
        /// <remarks>This will be a <see cref="IHtNode"/>.</remarks>
        /// <param name="e">The mouse event args on a node.</param>
        /// <returns>the node containing this event; could be <code>null</code> if no node was found.</returns>
        public IHtNode GetNodeUnderTheMouse(MouseEventArgs e)
        {
            int x = (int)e.GetPosition(this).X;
            int y = (int)e.GetPosition(this).Y;

            HtDrawNode node = draw.FindNode(new HtCoordS(x, y));
            if (node != null)
            {
                return node.HtModelNode.Node;

            }
            else
            {
                return null;
            }
        }

        #endregion

        #region Tooltip

        /**
     * 
     * Returns the tooltip to be displayed.
     * @param event    the event triggering the tooltip
     * @return         the String to be displayed
     */

        public string GetToolTipText(MouseEventArgs e)
        {
            int x = (int)e.GetPosition(this).X;
            int y = (int)e.GetPosition(this).Y;

            HtDrawNode node = draw.FindNode(new HtCoordS(x, y));
            if (node != null)
            {
                return node.Name;
            }
            else
            {
                return null;
            }
        }

        #endregion

        #region Paint

        /// <summary> Paint the component.
        /// </summary>
        /// <param name="drawingContext"></param>
        public void Repaint()
        {
            base.InvalidateVisual();
        }

        /// <summary>Overrides the <see cref="System.Windows.FrameworkElement.OnRender"/> method.
        /// </summary>
        /// <param name="drawingContext"></param>
        protected override void OnRender(DrawingContext drawingContext)
        {
            base.OnRender(drawingContext);
            if (image != null)
            {
                drawingContext.DrawImage(image, new Rect(0, 0, this.Width, this.Height));

                draw.RefreshScreenCoordinates();
                draw.DrawBranches(g);
                draw.DrawNodes(g);
            }
        }

        #endregion

        /// <summary> Stops the listening of mouse events.
        /// </summary>
        public void StopMouseListening()
        {
            Mouse.RemoveMouseDownHandler(this, action.MouseDownHandler);
            Mouse.RemoveMouseUpHandler(this, action.MouseUpHandler);
        }

        /// <summary> Starts the listening of mouse events.
        /// </summary>
        public void StartMouseListening()
        {
            Mouse.AddMouseDownHandler(this, action.MouseDownHandler);
            Mouse.AddMouseUpHandler(this, action.MouseUpHandler);
        }

        public void TranslateToOrigin(IHtNode node)
        {
            HtDrawNode __drawNode = draw.FindDrawNode(node);
            draw.TranslateToOrigin(__drawNode);
        }

        public Image Image
        {
            get
            {
                return this.image;
            }

            set
            {
                this.image = value;
            }
        }
    }

}
