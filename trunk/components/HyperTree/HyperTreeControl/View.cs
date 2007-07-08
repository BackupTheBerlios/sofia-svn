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
    public class View : IView
    {

        #region Fields

        private Model _model; // the tree model represented
        private Renderer _renderer; // the drawing model
        private Controller _controller; // action manager
        private IAddChild _visualRoot;
        private Image _image;

        #endregion

        #region Constructor

        /// <summary> Constructor. 
        /// </summary>
        /// <param name="model">The tree model to view.</param>
        public View(Model model, IAddChild visualRoot)
        {
            _model = model;            
            _visualRoot = visualRoot;

            _renderer = new Renderer(_model, this);
            _controller = new Controller(_renderer);            

            _visualRoot.AddChild(_renderer);

            this.StartMouseListening();
        }

        #endregion

        #region Tooltip


        /// <summary> Returns the tooltip to be displayed
        /// </summary>
        /// <param name="e">The mouse event args on a node.</param>
        /// <returns>The tooltip to be displayed.</returns> 
        public string GetToolTipText(MouseEventArgs e)
        {
            int x = (int)e.GetPosition(_renderer).X;
            int y = (int)e.GetPosition(_renderer).Y;

            NodeView node = _renderer.FindNode(new ScreenVector(x, y));
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
        public void Repaint()
        {
            _renderer.InvalidateVisual();
        }

        #endregion

        #region Mouse handling

        /// <summary> Stops the listening of mouse events.
        /// </summary>
        public void StopMouseListening()
        {
            Mouse.RemoveMouseDownHandler(_renderer, _controller.MouseDownHandler);
            Mouse.RemoveMouseUpHandler(_renderer, _controller.MouseUpHandler);
            Mouse.RemoveMouseMoveHandler(_renderer, _controller.MouseMoveHandler);
        }

        /// <summary> Starts the listening of mouse events.
        /// </summary>
        public void StartMouseListening()
        {
            Mouse.AddMouseDownHandler(_renderer, _controller.MouseDownHandler);
            Mouse.AddMouseUpHandler(_renderer, _controller.MouseUpHandler);
            Mouse.AddMouseMoveHandler(_renderer, _controller.MouseMoveHandler);
        }

        #endregion

        #region IView Members

        public int Height
        {
            get { return (int)_renderer.Height; }
            set
            {
                _renderer.Height = value;
                _renderer.RefreshScreenCoordinates();
                _renderer.InvalidateMeasure();
            }
        }

        public int Width
        {
            get { return (int)_renderer.Width; }
            set
            {
                _renderer.Width = value;
                _renderer.RefreshScreenCoordinates();
                _renderer.InvalidateMeasure();
            }
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
