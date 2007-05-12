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
using Microsoft.Practices.CompositeUI.Commands;
using Microsoft.Practices.CompositeUI.SmartParts;
using BankTellerCommon;
using Microsoft.Practices.CompositeUI.Utility;
using Microsoft.Practices.ObjectBuilder;

namespace BankTellerModule
{
	/// <summary>
	/// Interaction logic for CustomerQueueView.xaml
	/// </summary>
	[SmartPart]
	public partial class CustomerQueueView : System.Windows.Controls.UserControl
	{
		private CustomerQueueController myController;

		// We need our controller, so that we can work with a customer when
		// the user clicks on one
		[CreateNew]
		public CustomerQueueController MyController
		{
			set
			{
				myController = value;
			}
		}

		public CustomerQueueView()
		{
			InitializeComponent();
		}


		// In addition to offering a button to get the next customer from the
		// queue, we also support a menu item to do the same thing. Because
		// the signature for both methods (button click handle, command handler)
		// we use this single method to do both.

		[CommandHandler(CommandConstants.ACCEPT_CUSTOMER)]
		public void OnAcceptCustomer(object sender, EventArgs e)
		{
			Customer customer = myController.GetNextCustomerInQueue();

			if (customer == null)
			{
				MessageBox.Show("There are no more customers in the queue.", "Bank Teller", MessageBoxButton.OK, MessageBoxImage.Exclamation);
				return;
			}

			listCustomers.Items.Add(customer);
		}

		private void OnCustomerSelectionChanged(object sender, EventArgs e)
		{
			Customer customer = listCustomers.SelectedItem as Customer;

			if (customer != null)
			{
				myController.WorkWithCustomer(customer);
			}
		}
	}
}