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
	/// Interaction logic for CustomerDetailView.xaml
	/// </summary>
	[SmartPart]
	public partial class CustomerDetailView : System.Windows.Controls.UserControl
	{
		private WorkItem parentWorkItem;
		private CustomerDetailController controller;
		
		[ServiceDependency]
		public WorkItem ParentWorkItem
		{
			set
			{
				parentWorkItem = value;
			}
		}

		// The Customer state is stored in our parent work item
		[State]
		public Customer Customer
		{
			set
			{
				DataContext = value;
			}
		}

		// We use our controller so we can show the comments page
		[CreateNew]
		public CustomerDetailController Controller
		{
			set
			{
				controller = value;
			}
		}
		
		public CustomerDetailView()
		{
			InitializeComponent();
		}

		private void OnShowComments(object sender, EventArgs e)
		{
			controller.ShowCustomerComments();
		}
	}
}