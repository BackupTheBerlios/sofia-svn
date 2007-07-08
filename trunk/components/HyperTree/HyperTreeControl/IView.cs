using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Input;
using System.Windows.Controls;
using System.Windows;

namespace HyperTreeControl
{
    /// <summary> The IView is used by specific renderers of the hyperbolic tree.
    /// </summary>
    public interface IView
    {
        void StartMouseListening();

        void StopMouseListening();

        void Repaint();

        int Height { get; }

        int Width { get; }

        Image Image { get; set; }        
    }
}
