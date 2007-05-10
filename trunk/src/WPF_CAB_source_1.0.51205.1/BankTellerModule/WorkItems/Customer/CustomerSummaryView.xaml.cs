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
using System.Windows.Navigation;
using System.Windows.Shapes;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.SmartParts;
using Microsoft.Practices.ObjectBuilder;
using BankTellerCommon;

namespace BankTellerModule
{
	/// <summary>
	/// Interaction logic for CustomerSummaryView.xaml
	/// </summary>
	[SmartPart]
	public partial class CustomerSummaryView : System.Windows.Controls.UserControl
	{
		private CustomerSummaryController controller;

		[CreateNew]
		public CustomerSummaryController Controller
		{
			set
			{
				controller = value;
			}
		}

		public CustomerSummaryView()
		{
			InitializeComponent();
//			controller.WorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.CUSTOMERCONTEXT, this.customerContextMenu);
		}

		private void OnSave(object sender, EventArgs e)
		{
			controller.Save();
		}

		internal void FocusFirstTab()
		{
			this.tabbedWorkspace1.SelectedItem = this.tabbedWorkspace1.Items[0];
		}
	}
}