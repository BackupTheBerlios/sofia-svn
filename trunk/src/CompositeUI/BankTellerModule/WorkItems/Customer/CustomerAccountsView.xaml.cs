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
	/// Interaction logic for CustomerAccountsView.xaml
	/// </summary>
	[SmartPart]
	public partial class CustomerAccountsView : System.Windows.Controls.UserControl, IBuilderAware
	{
		private Customer customer;
		private CustomerAccountService accountService;

		// The Customer state is stored in our parent work item
		[State]
		public Customer Customer
		{
			set
			{
				customer = value;
			}
		}

		// Make sure our required CustomerAccountService is available
		[ServiceDependency]
		public CustomerAccountService AccountService
		{
			set
			{
				accountService = value;
			}
		}

		public CustomerAccountsView()
		{
			InitializeComponent();
		}

		void IBuilderAware.OnBuiltUp(string id)
		{
			DataContext = accountService.GetByCustomerID(customer.ID);
		}

		void IBuilderAware.OnTearingDown()
		{
		}
	}
}