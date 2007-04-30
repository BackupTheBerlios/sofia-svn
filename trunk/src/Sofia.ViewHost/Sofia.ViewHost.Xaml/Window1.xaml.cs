using System;
using System.IO;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Navigation;
using Sofia.Plugins;
using Sofia.DesignPatterns;

namespace Sofia
{
  public partial class Window1 : IViewHost, IObserver
	{
		public Window1()
		{
			this.InitializeComponent();
			
			// Insert code required on object creation below this point.
		}

    #region IObserver Members

    public void Update(object sender, object notification)
    {
      throw new Exception("The method or operation is not implemented.");
    }

    #endregion

    #region IViewHost Members

    public void ShowView(Sofia.Mvc.IView view, Sofia.Mvc.ViewDestination destination)
    {
      throw new Exception("The method or operation is not implemented.");
    }

    public void ShowToolBar(Sofia.Mvc.IView view, int row)
    {
      throw new Exception("The method or operation is not implemented.");
    }

    public void Save()
    {
      throw new Exception("The method or operation is not implemented.");
    }

    public void New()
    {
      throw new Exception("The method or operation is not implemented.");
    }

    #endregion
  }
}