using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Input;
using System.Windows.Controls;
using System.Windows;

namespace HyperTreeControl
{
  public interface IHtView
  {   
    void StartMouseListening();

    void StopMouseListening();

    void Repaint();

    int Height { get; }

    int Width { get; }

    Rect Insets {get;}

    IHtNode getNodeUnderTheMouse(MouseEventHandler mouseEventHandler);

    void translateToOrigin(IHtNode node);

    void setImage(Image image);

    void addMouseListener(MouseEventHandler mouseEventHandler);
  }
}
