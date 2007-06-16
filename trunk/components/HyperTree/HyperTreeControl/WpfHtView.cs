using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Input;
using System.Windows;
using System.Windows.Shapes;
using System.Windows.Markup;

namespace HyperTreeControl
{
    public partial class WpfHtView : IHtView
    {

        #region fields

        private HtModel _model; // the tree model represented
        private HtDraw _draw; // the drawing model
        private HtAction _action; // action manager
        private IAddChild _visualRoot;

        private Image _image = null;

        #endregion

        #region Constructor

        /// <summary> Constructor. 
        /// </summary>
        /// <param name="model">The tree model to view.</param>
        public WpfHtView(HtModel model, IAddChild visualRoot)
        {
            _model = model;
            _draw = new HtDraw(_model, this);
            _visualRoot = visualRoot;
            _action = new HtAction(_draw);
            this.StartMouseListening();

            _visualRoot.AddChild(_draw);
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
            int x = (int)e.GetPosition(_draw).X;
            int y = (int)e.GetPosition(_draw).Y;

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


        /// <summary> Returns the tooltip to be displayed
        /// </summary>
        /// <param name="e"></param>
        /// <returns></returns> 
        public string GetToolTipText(MouseEventArgs e)
        {
            int x = (int)e.GetPosition(_draw).X;
            int y = (int)e.GetPosition(_draw).Y;

            HtDrawNode node = _draw.FindNode(new HtCoordS(x, y));
            if (node != null)
            {
                return node.NodeName;
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
            _draw.InvalidateVisual();
        }

        #endregion

        #region Mouse handling

        /// <summary> Stops the listening of mouse events.
        /// </summary>
        public void StopMouseListening()
        {
            Mouse.RemoveMouseDownHandler(_draw, _action.MouseDownHandler);
            Mouse.RemoveMouseUpHandler(_draw, _action.MouseUpHandler);
            Mouse.RemoveMouseMoveHandler(_draw, _action.MouseMoveHandler);
        }

        /// <summary> Starts the listening of mouse events.
        /// </summary>
        public void StartMouseListening()
        {
            Mouse.AddMouseDownHandler(_draw, _action.MouseDownHandler);
            Mouse.AddMouseUpHandler(_draw, _action.MouseUpHandler);
            Mouse.AddMouseMoveHandler(_draw, _action.MouseMoveHandler);
        }

        #endregion

        #region IHtView Members


        public void TranslateToOrigin(IHtNode node)
        {
            HtDrawNode __drawNode = _draw.FindDrawNode(node);
            _draw.TranslateToOrigin(__drawNode);
        }

        public int Height
        {
            get { return (int)_draw.Height; }
            set
            {
                _draw.Height = value;
                _draw.RefreshScreenCoordinates();
            }
        }

        public int Width
        {
            get { return (int)_draw.Width; }
            set
            {
                _draw.Width = value;
                _draw.RefreshScreenCoordinates();
            }
        }

        public Rect Insets
        {
            get { return new Rect(new Size(_draw.Width, _draw.Height)); }
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
