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
  class WpfHtView: FrameworkElement, IHtView
  {
    
#region fields
    private HtModel    _model  = null; // the tree model represented
    private HtDraw     draw   = null; // the drawing model
    private HtAction   action = null; // action manager
    private bool    fastMode = false;
    private bool    longNameMode = false;
    private bool    circleMode = false;
    private bool    transNotCorrected = false;
    private bool    quadMode = true;

    private Image image = null;

#endregion

    #region Constructor

    /// <summary> Constructor. 
    /// </summary>
    /// <param name="model">The tree model to view.</param>
    public WpfHtView(HtModel model)
    {
        this.Margin = new System.Windows.Thickness(250);
        this.Background = new SolidColorBrush(Colors.White);

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
        int x = e.GetPosition().X;
        int y = e.GetPosition().Y;
        
        HtDrawNode node = draw.FindNode(new HtCoordS(x, y));
        if (node != null) {
            return node.HtModelNode.Node;
        
} else {
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
        int x = e.GetPosition().X;
        int y = e.GetPosition().Y;
        
        HtDrawNode node = draw.FindNode(new HtCoordS(x, y));
        if (node != null) {
            return node.Name;
        } else {
            return null;
        }
    }

#endregion

#region Paint 

    /**
     * Paint the component.
     *
     * @param g    the graphic context
     */
    //PDA
     
    protected override void  OnRender(DrawingContext drawingContext)
{
 	 base.OnRender(drawingContext);
        if (image != null) {
	    drawingContext.DrawImage(image, new Rect(0, 0, this.getWidth(), this.getHeight());

        draw.RefreshScreenCoordinates();
        draw.DrawBranches(g);
        draw.DrawNodes(g);
}

   

  /* --- Thread-safe locking --- */
  
    /**
     * Stops the listening of mouse events.
     */
    public void StopMouseListening() {
        this.MouseMove -= action.
        this.AddHandler(FrameworkElement.MouseMoveEvent, action
        this.removeMouseListener(action);
        this.removeMouseMotionListener(action);
    }
    
    /**
     * Starts the listening of mouse events.
     */
    public void startMouseListening() {
        this.addMouseListener(action);
        this.addMouseMotionListener(action);
    }


    public void translateToOrigin(HTNode node) {
	HTDrawNode drawNode = draw.findDrawNode(node);
        draw.translateToOrigin(drawNode);
        return;
    }

    public void setImage(Image image) {
        this.image = image;
        return;
    }
  }



#region IHtView Members

public void  StartMouseListening()
{
 	throw new Exception("The method or operation is not implemented.");
}

public void  StopMouseListening()
{
 	throw new Exception("The method or operation is not implemented.");
}

public void  Repaint()
{
 	throw new Exception("The method or operation is not implemented.");
}

public new int  Height
{
	get { throw new Exception("The method or operation is not implemented."); }
}

public new int  Width
{
	get { throw new Exception("The method or operation is not implemented."); }
}

public System.Windows.Rect  Insets
{
	get { throw new Exception("The method or operation is not implemented."); }
}

public IHtNode  GetNodeUnderTheMouse(System.Windows.Input.MouseEventHandler mouseEventHandler)
{
 	throw new Exception("The method or operation is not implemented.");
}

public void  TranslateToOrigin(IHtNode node)
{
 	throw new Exception("The method or operation is not implemented.");
}

public Image  Image
{
	  get 
	{ 
		throw new Exception("The method or operation is not implemented."); 
	}
	  set 
	{ 
		throw new Exception("The method or operation is not implemented."); 
	}
}

public void  AddMouseListener(System.Windows.Input.MouseEventHandler mouseEventHandler)
{
 	throw new Exception("The method or operation is not implemented.");
}

#endregion


}
