using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;

namespace HyperTreeControl
{
  class HtDrawNodeComposite: HtDrawNode 
  {
    private HtModelNodeComposite node      = null; // encapsulated HtModelNode
    private List<HtDrawNode>              children  = null; // children of this node
    private Dictionary<HtDrawNode, HtGeodesic> geodesics = null; // geodesics linking the children


  /* --- Constructor --- */

    /**
     * Constructor.
     *
     * @param father    the father of this node
     * @param node      the encapsulated HTModelNode
     * @param model     the drawing model
     */
    HtDrawNodeComposite(HtDrawNodeComposite father, HtModelNodeComposite node, HtDraw model): base(father, node, model) 
    {
        
        this.node = node;
        this.children = new List<HtDrawNode>();
        this.geodesics = new Dictionary<HtDrawNode, HtGeodesic>();

        HtModelNode childNode = null;
        HtDrawNode child = null;
        HtDrawNode brother = null;
        bool first = true;
        bool second = false;
        foreach (HtModelNode __childNode in node.Children)
	{
            if (childNode.IsLeaf) {
                child = new HtDrawNode(this, childNode, model);
            } else {
                child = new HtDrawNodeComposite(this, (HtModelNodeComposite)childNode, model);
            }
            addChild(child);
            if (first) {
                brother = child;
                first = false;
                second = true;
            } else if (second) {
                child.Brother = brother;
                brother.Brother = child;
                brother = child;
                second = false;
            } else {
                child.Brother = brother;
                brother = child;
            }  
        }
    }


  /* --- Children --- */

    /**
     * Returns the children of this node, 
     * in an Enumeration.
     *
     * @return    the children of this node
     */
    public List<HtDrawNode> Children 
    {
        get
        {
        return this.children;
        }
    }

    /** 
     * Adds the HtDrawNode as a children.
     *
     * @param child    the child
     */
    void addChild(HtDrawNode child) {
        children.add(child);
        geodesics.put(child, new HtGeodesic(getCoordinates(), child.getCoordinates()));
    }


  /* --- Screen Coordinates --- */

    /**
     * Refresh the screen coordinates of this node
     * and recurse on children.
     *
     * @param sOrigin   the origin of the screen plane
     * @param sMax      the (xMax, yMax) point in the screen plane
     */ 
    void refreshScreenCoordinates(HtCoordS sOrigin, HtCoordS sMax) {
        super.refreshScreenCoordinates(sOrigin, sMax);
        HTDrawNode child = null;

        for (Iterator i = children(); i.hasNext(); ) {
            child = (HTDrawNode) i.next();
            child.refreshScreenCoordinates(sOrigin, sMax);
            HTGeodesic geod = (HTGeodesic) geodesics.get(child);
            if (geod != null) {
                geod.refreshScreenCoordinates(sOrigin, sMax);
            }
            
        }
    }


  /* --- Drawing --- */

    /**
     * Draws the branches from this node to 
     * its children.
     *
     * @param g    the graphic context
     */
    void drawBranches(Canvas g) {
        HtDrawNode child = null;

        for (Iterator i = children(); i.hasNext(); ) {
            child = (HTDrawNode) i.next();
            HTGeodesic geod = (HTGeodesic) geodesics.get(child);
            if (geod != null) {
                geod.draw(g);
            }
            child.drawBranches(g); 
        }
    }

    /**
     * Draws this node.
     *
     * @param g    the graphic context
     */
    void drawNodes(Canvas g) {
        if (fastMode == false) {
            super.drawNodes(g);
        
            HTDrawNode child = null;
            for (Iterator i = children(); i.hasNext(); ) {
                child = (HTDrawNode) i.next();
                child.drawNodes(g);
            }
        }
    }

    /**
     * Returns the minimal distance between this node
     * and his father, his brother, and his children.
     *
     * @return    the minimal distance
     */
    int getSpace() {
        int space = super.getSpace();
        
        if (! children.isEmpty()) {
            HTDrawNode child = (HTDrawNode) children.get(0);
            HTCoordS zC = child.getScreenCoordinates();      
            int dC = zs.getDistance(zC);
            
            if (space == -1) {
                return dC;
            } else {
                return Math.min(space, dC);
            }
        } else {
            return space;
        }
    }
    

  /* --- Translation --- */

    /**
     * Translates this node by the given vector.
     *
     * @param t    the translation vector
     */
    void translate(HTCoordE t) {
        super.translate(t);

        HTDrawNode child = null;
        for (Iterator i = children(); i.hasNext(); ) {
            child = (HTDrawNode) i.next();
            child.translate(t);
            HTGeodesic geod = (HTGeodesic) geodesics.get(child);
            if (geod != null) {
                geod.rebuild();
            }
        }

    }

    /**
     * Transform this node by the given transformation.
     *
     * @param t    the transformation
     */
    void transform(HTTransformation t) {
        super.transform(t);

        HTDrawNode child = null;
        for (Iterator i = children(); i.hasNext(); ) {
            child = (HTDrawNode) i.next();
            child.transform(t);
            HTGeodesic geod = (HTGeodesic) geodesics.get(child);
            if (geod != null) {
                geod.rebuild();
            }
        }
    }

    /**
     * Ends the translation.
     */
    void endTranslation() {
        super.endTranslation();

        HTDrawNode child = null;
        for (Iterator i = children(); i.hasNext(); ) {
            child = (HTDrawNode) i.next();
            child.endTranslation();
        }
    }

    /**
     * Restores the hyperbolic tree to its origin.
     */
    void restore() {
        super.restore();

        HTDrawNode child = null;
        for (Iterator i = children(); i.hasNext(); ) {
            child = (HTDrawNode) i.next();
            child.restore();
            HTGeodesic geod = (HTGeodesic) geodesics.get(child);
            if (geod != null) {
                geod.rebuild();
            }
        }

    }

    /**
     * Sets the fast mode, where nodes are no more drawed.
     *
     * @param mode    setting on or off.
     */
    void fastMode(boolean mode) {
        super.fastMode(mode);
        
        HTDrawNode child = null;
        for (Iterator i = children(); i.hasNext(); ) {
            child = (HTDrawNode) i.next();
            child.fastMode(mode);
        }
    }


  /* --- Node searching --- */

    /**
     * Returns the node (if any) whose screen coordinates' zone
     * contains thoses given in parameters.
     *
     * @param zs    the given screen coordinate
     * @return      the searched HTDrawNode if found;
     *              <CODE>null</CODE> otherwise
     */
    HTDrawNode findNode(HTCoordS zs) {
        HTDrawNode result = super.findNode(zs);
        if (result != null) {
            return result;
        } else {
            HTDrawNode child = null;
            for (Iterator i = children(); i.hasNext(); ) {
                 child = (HTDrawNode) i.next();
                 result = child.findNode(zs);
                 if (result != null) {
                     return result;
                 }
            }
            return null;
        }
    }


  /* --- ToString --- */

    /**
     * Returns a string representation of the object.
     *
     * @return    a String representation of the object
     */
    public String toString() {
        String result = super.toString();
        HTDrawNode child = null;
        result += "\n\tChildren :";
        for (Iterator i = children(); i.hasNext(); ) {
            child = (HTDrawNode) i.next();
            result += "\n\t-> " + child.getName();
        }
        return result;
    }

  }
}
