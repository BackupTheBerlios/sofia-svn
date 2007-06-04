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

        Rect Insets { get; }

        IHtNode GetNodeUnderTheMouse(MouseEventHandler mouseEventHandler);

        void TranslateToOrigin(IHtNode node);

        Image Image { get; set; }

        void AddMouseListener(MouseEventHandler mouseEventHandler);
    }
}
