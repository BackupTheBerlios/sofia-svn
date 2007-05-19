using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

using Microsoft.Practices.CompositeUI;


namespace Sofia.Shell
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>

    public partial class SofiaShellWindow : System.Windows.Window
    {

        #region fields

        private WorkItem workItem;

        #endregion

        #region properties

        public Menu Menu
        {
            get
            {
                return _menu;
            }
        }

        public MenuItem FileMenuItem
        {
            get
            {
                return _fileMenuItem;
            }
        }

        #endregion

        public SofiaShellWindow()
        {
            InitializeComponent();
        }
    }
}