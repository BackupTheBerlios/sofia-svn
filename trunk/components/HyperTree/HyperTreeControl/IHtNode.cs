using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Windows.Media;
using System.Windows.Controls;


namespace HyperTreeControl
{


  /// <smmary> The HtNode interface should be implemented by object that are node of the tree that want to be displayed in the TreeMap.  
  ///If you have already a tree structure, just implements this interface in node of the tree.
  /// </summary>
  public interface IHtNode
  {
    /// <summary> Returns the children of this node in an Enumeration.
    ///If this object does not have children, it should return an empty Enumeration.
    ///All objects contained in the Enumeration should implements IHtNode.
    ///<returns>an Enumeration containing childs of this node</returns>
    /// </summary>    
    List<IHtNode> Children { get; }

    /// <summary> Checks if this node is a leaf or not. 
    ///A node could have no children and still not be a leaf.
    /// <returns>True if this node is a leaf; false otherwise.</returns>
    /// </summary>
    bool IsLeaf { get; }

    /// <summary> Gets the name of this node.
    ///Used to display a label in the hyperbolic tree.     
    /// </summary>
    string Name { get; }

    /// <summary> Gets the color of the node.
    ///Used in the drawing of the node label.
    /// </summary>
    Color Color { get; }

    /// <summary> Gets the preferred size of the node. Reasonable values are from 0 to 10.
    /// </summary>
    int Size { get; }

    /// <summary> Gets the preferred width of the border of the node.
    ///Reasonable values are from 1 to 4.
    /// </summary>
    /// <returns>The border width.</returns>
    int BorderSize { get; }

    /// <summary> Gets the image (icon) which should be displayed insidethe node. 
    /// null is interpreted as no image.
    /// </summary>
    Image Image { get; }
  }

}

