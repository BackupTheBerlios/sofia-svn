using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Input;
using System.Windows.Controls;
using System.Windows;

namespace HyperTreeControl
{
  interface IHtView
  {   
    void startMouseListening();

    void stopMouseListening();

    void repaint();

    int Height { get; }

    int Width { get; }

    Rect getInsets();

    IHtNode getNodeUnderTheMouse(MouseEventHandler mouseEventHandler);

    void translateToOrigin(IHtNode node);

    void setImage(Image image);

    void addMouseListener(MouseEventHandler mouseEventHandler);
  }
}
