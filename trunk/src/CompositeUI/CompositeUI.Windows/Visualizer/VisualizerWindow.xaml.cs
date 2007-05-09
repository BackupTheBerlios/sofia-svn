using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using Microsoft.Practices.CompositeUI.Windows.Workspaces;

namespace Microsoft.Practices.CompositeUI.Windows.Visualizer
{
	/// <summary>
	/// Interaction logic for VisualizerWindow.xaml
	/// </summary>

	public partial class VisualizerWindow : System.Windows.Window
	{

		public VisualizerWindow()
		{
			InitializeComponent();
			//doing this in XAML causes issues with a non-fixed assembly version
			TabWorkspace tabWorkspace = new TabWorkspace();
			tabWorkspace.Name = "MainWorkspace";
			Content = tabWorkspace;
		}

		protected override void OnClosing(CancelEventArgs e)
		{
			e.Cancel = true;
			WindowState = WindowState.Minimized;
			base.OnClosing(e);
		}
	}
}