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

        IHtNode GetNodeUnderTheMouse(MouseEventArgs e);

        void TranslateToOrigin(IHtNode node);

        Image Image { get; set; }        
    }
}
