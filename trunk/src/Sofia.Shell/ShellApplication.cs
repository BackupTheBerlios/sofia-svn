using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.Practices.CompositeUI.Windows;
using Microsoft.Practices.CompositeUI;
using System.Windows.Controls;


namespace Sofia.Cab.Shell
{
    public sealed class ShellApplication : ApplicationShellApplication<WorkItem, App>
    {
        protected override void AfterShellCreated()
        {
            base.AfterShellCreated();

            //create the main window
            ShellWindow sofiaShellWindow = RootWorkItem.Items.AddNew<ShellWindow>();
            Shell.MainWindow = sofiaShellWindow;

            MenuItem fileItem = sofiaShellWindow.FileMenuItem;

            //RootWorkItem.UIExtensionSites.RegisterSite("MainMenu", sofiaShellWindow.Menu);
            //RootWorkItem.UIExtensionSites.RegisterSite(UIExtensionConstants.FILE, bankShellWindow.Menu);

            // Load the menu structure from App.config
            //UIElementBuilder.LoadFromConfig(RootWorkItem);

        }
    }
}
