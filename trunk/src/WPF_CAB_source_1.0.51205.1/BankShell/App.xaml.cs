using System;
using System.Windows;
using System.Data;
using System.Xml;
using System.Configuration;

namespace BankShell
{
	/// <summary>
	/// Interaction logic for App.xaml
	/// </summary>

	public partial class App : System.Windows.Application
	{
		protected override void OnStartup(StartupEventArgs e)
		{
			base.OnStartup(e);

			//start the CAB app up, which will result in our root window being shown
//			new BankShellApplication().Run();
		}
	}
}