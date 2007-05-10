using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Microsoft.Practices.CompositeUI.SmartParts;

namespace BankTellerModule
{
	/// <summary>
	/// Interaction logic for UserInfoView.xaml
	/// </summary>
	[SmartPart]
	public partial class UserInfoView : System.Windows.Controls.UserControl
	{
		public UserInfoView()
		{
			InitializeComponent();
			_label.Content = string.Format("User: {0}", Thread.CurrentPrincipal.Identity.Name);
		}

	}
}