using System;
using System.Collections.Generic;
using System.Configuration;
using System.Text;
using System.Windows.Controls;
using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.UIElements;
using Microsoft.Practices.CompositeUI.Windows;

namespace BankShell
{
	/// <summary>
	/// This is a temporary implementation that will be replaced with something
	/// richer when we move it into the framework.
	/// </summary>
	public static class UIElementBuilder
	{
		// Loads the menu items from App.config and put them into the menu strip, hooking
		// up the menu URIs for command dispatching.
		public static void LoadFromConfig(WorkItem workItem)
		{
			ShellItemsSection section = (ShellItemsSection) ConfigurationManager.GetSection("shellitems");

			foreach (MenuItemElement menuItem in section.MenuItems)
			{
				MenuItem uiMenuItem = menuItem.ToMenuItem();

				workItem.UIExtensionSites[menuItem.Site].Add(uiMenuItem);

				if (menuItem.Register)
				{
					workItem.UIExtensionSites.RegisterSite(menuItem.RegistrationSite, uiMenuItem);
				}

				if (!string.IsNullOrEmpty(menuItem.CommandName))
				{
					workItem.Commands[menuItem.CommandName].AddInvoker(uiMenuItem, "Click");
				}
			}
		}
	}
}
