using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.Windows;
using BankTellerCommon;

namespace BankShell
{
	public sealed class BankShellApplication : ApplicationShellApplication<WorkItem, App>
	{
		protected override void AfterShellCreated()
		{
			base.AfterShellCreated();
			
			//create the main window
			BankShellWindow bankShellWindow = RootWorkItem.Items.AddNew<BankShellWindow>();
			Shell.MainWindow = bankShellWindow;
			MenuItem fileItem = bankShellWindow.FileMenuItem;

			RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.MAINMENU, bankShellWindow.Menu);
			RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.MAINSTATUS, bankShellWindow.StatusBar);
			RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.FILE, bankShellWindow.Menu);
			RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.FILEDROPDOWN, fileItem);

			// Load the menu structure from App.config
			UIElementBuilder.LoadFromConfig(RootWorkItem);

			//add commands that map to WPF commands (the LoadFromConfig method isn't smart enough to do this yet)
			MenuItem menuItem = new MenuItem();
			menuItem.Header = "_Undo";
			menuItem.Command = ApplicationCommands.Undo;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);
			menuItem = new MenuItem();
			menuItem.Header = "_Redo";
			menuItem.Command = ApplicationCommands.Redo;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);

			RootWorkItem.UIExtensionSites["Edit"].Add(new Separator());

			menuItem = new MenuItem();
			menuItem.Header = "Cu_t";
			menuItem.Command = ApplicationCommands.Cut;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);
			menuItem = new MenuItem();
			menuItem.Header = "_Copy";
			menuItem.Command = ApplicationCommands.Copy;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);
			menuItem = new MenuItem();
			menuItem.Header = "_Paste";
			menuItem.Command = ApplicationCommands.Paste;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);

			RootWorkItem.UIExtensionSites["Edit"].Add(new Separator());

			menuItem = new MenuItem();
			menuItem.Header = "Select _All";
			menuItem.Command = ApplicationCommands.SelectAll;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);

			//show the main window
			bankShellWindow.Show();
		}

		#region Unhandled Exception

		public override void OnUnhandledException(object sender, UnhandledExceptionEventArgs e)
		{
			Exception ex = e.ExceptionObject as Exception;

			if (ex != null)
			{
				MessageBox.Show(BuildExceptionString(ex));
			}
			else
			{
				MessageBox.Show("An Exception has occured, unable to get details");
			}

			Environment.Exit(0);
		}

		private string BuildExceptionString(Exception exception)
		{
			string errMessage = string.Empty;

			errMessage += exception.Message + Environment.NewLine + exception.StackTrace;

			while (exception.InnerException != null)
			{
				errMessage += BuildInnerExceptionString(exception.InnerException);
				exception = exception.InnerException;
			}

			return errMessage;
		}

		private string BuildInnerExceptionString(Exception innerException)
		{
			string errMessage = string.Empty;

			errMessage += Environment.NewLine + " InnerException ";
			errMessage += Environment.NewLine + innerException.Message + Environment.NewLine + innerException.StackTrace;

			return errMessage;
		}

		#endregion
	}




	/*
	public sealed class BankShellApplication : WindowShellApplication<WorkItem, BankShellWindow>
	{
		protected override void AfterShellCreated()
		{
			base.AfterShellCreated();

			MenuItem fileItem = Shell.FileMenuItem;

			RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.MAINMENU, Shell.Menu);
			RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.MAINSTATUS, Shell.StatusBar);
			RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.FILE, Shell.Menu);
			RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.FILEDROPDOWN, fileItem);

			// Load the menu structure from App.config
			UIElementBuilder.LoadFromConfig(RootWorkItem);

			//add commands that map to WPF commands (the LoadFromConfig method isn't smart enough to do this yet)
			MenuItem menuItem = new MenuItem();
			menuItem.Header = "_Undo";
			menuItem.Command = ApplicationCommands.Undo;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);
			menuItem = new MenuItem();
			menuItem.Header = "_Redo";
			menuItem.Command = ApplicationCommands.Redo;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);

			RootWorkItem.UIExtensionSites["Edit"].Add(new Separator());

			menuItem = new MenuItem();
			menuItem.Header = "Cu_t";
			menuItem.Command = ApplicationCommands.Cut;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);
			menuItem = new MenuItem();
			menuItem.Header = "_Copy";
			menuItem.Command = ApplicationCommands.Copy;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);
			menuItem = new MenuItem();
			menuItem.Header = "_Paste";
			menuItem.Command = ApplicationCommands.Paste;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);

			RootWorkItem.UIExtensionSites["Edit"].Add(new Separator());

			menuItem = new MenuItem();
			menuItem.Header = "Select _All";
			menuItem.Command = ApplicationCommands.SelectAll;
			RootWorkItem.UIExtensionSites["Edit"].Add(menuItem);
		}

		#region Unhandled Exception

		public override void OnUnhandledException(object sender, UnhandledExceptionEventArgs e)
		{
			Exception ex = e.ExceptionObject as Exception;

			if (ex != null)
			{
				MessageBox.Show(BuildExceptionString(ex));
			}
			else
			{
				MessageBox.Show("An Exception has occured, unable to get details");
			}

			Environment.Exit(0);
		}

		private string BuildExceptionString(Exception exception)
		{
			string errMessage = string.Empty;

			errMessage += exception.Message + Environment.NewLine + exception.StackTrace;

			while (exception.InnerException != null)
			{
				errMessage += BuildInnerExceptionString(exception.InnerException);
				exception = exception.InnerException;
			}

			return errMessage;
		}

		private string BuildInnerExceptionString(Exception innerException)
		{
			string errMessage = string.Empty;

			errMessage += Environment.NewLine + " InnerException ";
			errMessage += Environment.NewLine + innerException.Message + Environment.NewLine + innerException.StackTrace;

			return errMessage;
		}

		#endregion
	}
	*/
}
