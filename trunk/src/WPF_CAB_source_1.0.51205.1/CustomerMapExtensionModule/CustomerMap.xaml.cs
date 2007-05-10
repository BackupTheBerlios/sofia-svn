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

namespace CustomerMapExtensionModule
{
	/// <summary>
	/// Interaction logic for CustomerMap.xaml
	/// </summary>
	[SmartPart]
	public partial class CustomerMap : System.Windows.Controls.UserControl
	{
		private Customer customer;
		private bool mapLoaded = false;

		const string mapUrlFormat = "http://maps.msn.com/home.aspx?strt1={0}&city1={2}&stnm1={3}&zipc1={4}";

		[State(StateConstants.CUSTOMER)]
		public Customer Customer
		{
			set
			{
				customer = value;
			}
		}

		public CustomerMap()
		{
			InitializeComponent();
			Loaded += new RoutedEventHandler(CustomerMap_Loaded);
		}

		void  CustomerMap_Loaded(object sender, RoutedEventArgs e)
		{
			if (IsVisible && !mapLoaded)
			{
				LoadMap();
				mapLoaded = true;
			}
		}

		private void LoadMap()
		{
			if (customer != null)
			{
				string url = string.Format(mapUrlFormat, customer.Address1, customer.Address2, customer.City, customer.State, customer.ZipCode);
				browser.Navigate(Uri.EscapeUriString(url));
			}
		}
	}
}