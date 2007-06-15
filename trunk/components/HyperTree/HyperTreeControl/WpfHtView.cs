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
    public partial class WpfHtView : Canvas, IHtView
    {

        #region fields

        private HtModel _model = null; // the tree model represented
        private HtDraw _draw = null; // the drawing model
        private HtAction _action = null; // action manager

        private Image _image = null;

        #endregion

        #region Constructor

        /// <summary> Constructor. 
        /// </summary>
        /// <param name="model">The tree model to view.</param>
        public WpfHtView(HtModel model)
        {
            this.Margin = new System.Windows.Thickness(250);

            _model = model;
            _draw = new HtDraw(_model, this);
            _action = new HtAction(_draw);
            this.StartMouseListening();
        }

        public WpfHtView()
        {
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

            HtDrawNode node = _draw.FindNode(new HtCoordS(x, y));
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

            HtDrawNode node = _draw.FindNode(new HtCoordS(x, y));
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
        protected override void OnRender(DrawingContext dc)
        {
            base.OnRender(dc);
            /*
            if (_image != null)
            {
                dc.DrawImage(_image.Source, new Rect(0, 0, this.Width, this.Height));
            }

            _draw.RefreshScreenCoordinates();
            _draw.DrawBranches(dc);
            _draw.DrawNodes(dc);
             * */
        }

        #endregion

        #region Mouse handling

        /// <summary> Stops the listening of mouse events.
        /// </summary>
        public void StopMouseListening()
        {
            Mouse.RemoveMouseDownHandler(this, _action.MouseDownHandler);
            Mouse.RemoveMouseUpHandler(this, _action.MouseUpHandler);
            Mouse.RemoveMouseMoveHandler(this, _action.MouseMoveHandler);                        
        }        

        /// <summary> Starts the listening of mouse events.
        /// </summary>
        public void StartMouseListening()
        {
            Mouse.AddMouseDownHandler(this, _action.MouseDownHandler);
            Mouse.AddMouseUpHandler(this, _action.MouseUpHandler);
            Mouse.AddMouseMoveHandler(this, _action.MouseMoveHandler);
            //this.PreviewMouseDown += _action.MouseDownHandler;
        }

        #endregion

        #region IHtView Members

        public void TranslateToOrigin(IHtNode node)
        {
            HtDrawNode __drawNode = _draw.FindDrawNode(node);
            _draw.TranslateToOrigin(__drawNode);
        }

        public new int Height
        {
            get { return (int)base.Height; }
            set { base.Height = value; }
        }

        public new int Width
        {
            get { return (int)base.Width; }
            set { base.Width = value; }
        }

        public Rect Insets
        {
            get { return new Rect(new Size(base.Width, base.Height)); }
        }

        public Image Image
        {
            get
            {
                return this._image;
            }

            set
            {
                this._image = value;
            }
        }

        #endregion
    }

}
