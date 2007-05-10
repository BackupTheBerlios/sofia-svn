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
using BankTellerCommon;

namespace BankTellerModule
{
	/// <summary>
	/// Interaction logic for CustomerHeaderView.xaml
	/// </summary>
	[SmartPart]
	public partial class CustomerHeaderView : System.Windows.Controls.UserControl
	{
		[State]
		public Customer Customer
		{
			set
			{
				DataContext = value;
			}
		}

		public CustomerHeaderView()
		{
			InitializeComponent();
		}

	}
}