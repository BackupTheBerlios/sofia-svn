using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Media;
using System.Windows;
using System.Windows.Controls;

namespace HyperTreeControl
{
  public class HtDrawNode
  {
      
    private HtDraw model = null;  // drawing model
    private HtModelNode node = null;  // encapsulated HTModelNode

    private HtCoordE ze = null;  // current euclidian coordinates
    private HtCoordE oldZe = null;  // old euclidian coordinates
    protected HtCoordS zs = null;  // current screen coordinates

    private HtDrawNodeComposite father = null;  // father of this node
    private HtDrawNode brother = null;  // brother of this node

    private HtNodeLabel label = null;  // label of the node

    protected bool fastMode = false; // fast mode


    /* --- Constructor --- */

    /**
     * Constructor.
     *
     * @param father    the father of this node
     * @param node      the encapsulated HtModelNode
     * @param model     the drawing model
     */
    public HtDrawNode(HtDrawNodeComposite father, HtModelNode node, HtDraw model)
    {
      this.father = father;
      this.node = node;
      this.model = model;

      label = new HtNodeLabel(this);

      ze = new HtCoordE(node.Coordinates);
      oldZe = new HtCoordE(ze);
      zs = new HtCoordS();

      // store this object in HTNode -> HTDrawNode mapping
      model.MapNode(node.Node, this);

      return;
    }

    /// <summary>
    /// Gets or sets the brother of this node.
    /// </summary>
    public HtDrawNode Brother
    {
      get
      {
        return brother;
      }

      set
      {
        brother = value;
      }
    }

    /// <summary>
    /// Gets the encapsulated HtModelNode.
    /// </summary>
    public HtModelNode HtModelNode
    {
      get
      {
        return node;
      }
    }

    /// <summary>
    /// Gets the color of the node.
    /// </summary>
    public Color Color
    {
      get
      {
        return node.Node.Color;
      }
    }

    /// <summary>
    /// Gets the name of this node.
    /// </summary>
    public string Name
    {
      get
      {
        return node.Name;
      }
    }

    /// <summary>
    /// Gets the current coordinates of this node.
    /// </summary>
    public HtCoordE Coordinates
    {
      get
      {
        return ze;
      }
    }

    public HtCoordE OldCoordinates
    {
      get
      {
        return oldZe;
      }
    }

    public HtCoordS ScreenCoordinates
    {
      get
      {
        return zs;
      }
    }


    /**
     * Refresh the screen coordinates of this node.
     *
     * @param sOrigin   the origin of the screen plane
     * @param sMax      the (xMax, yMax) point in the screen plane
     */
    public virtual void RefreshScreenCoordinates(HtCoordS sOrigin, HtCoordS sMax)
    {
      zs.ProjectionEtoS(ze, sOrigin, sMax);
    }


    #region Drawing

    /**
     * Draws the branches from this node to 
     * its children.
     * Overidden by HTDrawNodeComposite
     *
     * @param g    the graphic context
     */
    public void DrawBranches(FrameworkElement canvas) { }

    /**
     * Draws this node.
     *
     * @param g    the graphic context
     */
      public virtual void DrawNodes(Canvas canvas)
    {
      if (fastMode == false)
      {
        label.Draw(canvas);
      }
    }

    /**
     * Returns the minimal distance between this node
     * and his father and his brother.
     *
     * @return    the minimal distance
     */
    public virtual int GetSpace()
    {
      int dF = -1;
      int dB = -1;

      if (father != null)
      {
        HtCoordS zF = father.ScreenCoordinates;
        dF = zs.GetDistance(zF);
      }
      if (brother != null)
      {
        HtCoordS zB = brother.ScreenCoordinates;
        dB = zs.GetDistance(zB);
      }

      // this means that the node is a standalone node
      if ((dF == -1) && (dB == -1))
      {
        return int.MaxValue;
      }
      else if (dF == -1)
      {
        return dB;
      }
      else if (dB == -1)
      {
        return dF;
      }
      else
      {
        return Math.Min(dF, dB);
      }
    }

    #endregion

    #region Translation
    /* --- Translation --- */

    /**
     * Translates this node by the given vector.
     *
     * @param t    the translation vector
     */
    public virtual void Translate(HtCoordE t)
    {
      ze.Translate(oldZe, t);
    }

    /**
     * Transform this node by the given transformation.
     *
     * @param t    the transformation
     */
    public virtual void Transform(HtTransformation t)
    {
      ze.Copy(oldZe);
      ze._transform(t);
    }

    /**
     * Ends the translation.
     */
    public virtual void EndTranslation()
    {
      oldZe.Copy(ze);
    }

    /**
     * Restores the hyperbolic tree to its origin.
     */
    public virtual void Restore()
    {
      HtCoordE orig = node.Coordinates;
      ze.X = orig.X;
      ze.Y = orig.Y;
      oldZe.Copy(ze);
    }

    /// <summary>
    /// Sets the fast mode, where nodes are no more drawed.
    /// </summary>
    public virtual bool FastMode
    {
      get
      {
        return fastMode;
      }

      set
      {
        fastMode = value;
      }
    }

    #endregion


    #region Node searching
    /* --- Node searching --- */

    /**
     * Returns the node (if any) whose screen coordinates' zone
     * contains thoses given in parameters.
     *
     * @param zs    the given screen coordinate
     * @return      the searched HTDrawNode if found;
     *              <CODE>null</CODE> otherwise
     */
    public virtual HtDrawNode FindNode(HtCoordS zs)
    {
      if (label.Contains(zs))
      {
        return this;
      }
      else
      {
        return null;
      }
    }

    #endregion


    /**
     * Returns a string representation of the object.
     *
     * @return    a String representation of the object
     */
    public override string ToString()
    {
      string result = Name +
                      "\n\t" + ze +
                      "\n\t" + zs;
      return result;
    }

    /// <summary>
    /// Gets the size of the node.
    /// </summary>
    public int Size
    {
      get
      {
        return node.Node.Size;
      }
    }


    /// <summary>
    /// Gets the thickness of the border.
    /// </summary>
    public int BorderSize
    {
      get
      {
        return node.Node.BorderSize;
      }
    }

    /// <summary>
    /// Gets the image which is displayed inside the node.
    /// </summary>
    public Image Image
    {
      get
      {
        return node.Node.Image;
      }
    }

  }
}
