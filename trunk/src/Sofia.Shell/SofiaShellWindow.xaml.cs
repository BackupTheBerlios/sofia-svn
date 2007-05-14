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

using CompositeUI = Microsoft.Practices.CompositeUI;


namespace Sofia.Shell
{
  /// <summary>
  /// Interaction logic for Window1.xaml
  /// </summary>

  public partial class Window1 : System.Windows.Window
  {

      #region fields

      private CompositeUI.WorkItem workItem;

      #endregion

      public Window1()
    {
      InitializeComponent();
    }

  }
}