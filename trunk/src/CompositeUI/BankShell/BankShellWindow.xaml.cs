using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.Commands;
using Microsoft.Practices.CompositeUI.Utility;
using Microsoft.Practices.CompositeUI.Services;
using Microsoft.Practices.CompositeUI.EventBroker;
using Microsoft.Practices.CompositeUI.Windows.Workspaces;
using Microsoft.Practices.ObjectBuilder;
using Microsoft.Practices.CompositeUI.SmartParts;

namespace BankShell
{
	/// <summary>
	/// Interaction logic for Window1.xaml
	/// </summary>

	public partial class BankShellWindow : System.Windows.Window
	{
		private WorkItem workItem;
		private IWorkItemTypeCatalogService workItemTypeCatalog;

		public Menu Menu
		{
			get
			{
				return _menu;
			}
		}

		public StatusBar StatusBar
		{
			get
			{
				return _statusBar;
			}
		}

		public MenuItem FileMenuItem
		{
			get
			{
				return _fileMenuItem;
			}
		}

		public BankShellWindow()
		{
			InitializeComponent();
		}

		[InjectionConstructor]
		public BankShellWindow(WorkItem workItem, IWorkItemTypeCatalogService workItemTypeCatalog)
			: this()
		{
			this.workItem = workItem;
			this.workItemTypeCatalog = workItemTypeCatalog;
		}

		[CommandHandler("FileExit")]
		public void OnFileExit(object sender, EventArgs e)
		{
			Close();
		}

		[CommandHandler("HelpAbout")]
		public void OnHelpAbout(object sender, EventArgs e)
		{
			MessageBox.Show(this, "WPF Bank Teller QuickStart Version 1.0 (by Kent Boogaart)", "About");
		}

		[EventSubscription("topic://BankShell/statusupdate", Thread = ThreadOption.UserInterface)]
		public void OnStatusUpdate(object sender, DataEventArgs<string> e)
		{
			_statusBarItem.Content = e.Data;
		}

		private void contentWorkspace_SmartPartActivating(object sender, WorkspaceCancelEventArgs e)
		{
			//uncomment this code to see how you can use the SmartPartActivating event to optionally cancel smart part activations
			//NOTE: IWorkspace.SmartPartActivating is not a standard part of CAB, so note that you will be tying yourself to this implementation
			//of CAB. I had to add this event myself for my own benefit

			//if (MessageBox.Show("Navigate to '" + e.SmartPart + "'?", "Confirm Navigation", MessageBoxButton.YesNo, MessageBoxImage.Question) == MessageBoxResult.No)
			//{
			//    e.Cancel = true;
			//}
		}
	}
}