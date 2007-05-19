using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.Practices.CompositeUI.Windows;
using Microsoft.Practices.CompositeUI;
using System.Windows.Controls;

using SCAB_Common = Sofia.Cab.Common;


namespace Sofia.Cab.Shell
{
    public sealed class ShellApplication : ApplicationShellApplication<WorkItem, App>
    {
        protected override void AfterShellCreated()
        {
            base.AfterShellCreated();

            //create the main window
            ShellWindow shellWindow = RootWorkItem.Items.AddNew<ShellWindow>();
            Shell.MainWindow = shellWindow;
            MenuItem fileItem = shellWindow.FileMenuItem;

            RootWorkItem.UIExtensionSites.RegisterSite(SCAB_Common.UIExtensionConstants.MAINMENU, shellWindow.Menu);
            RootWorkItem.UIExtensionSites.RegisterSite(SCAB_Common.UIExtensionConstants.FILE, shellWindow.Menu);
            RootWorkItem.UIExtensionSites.RegisterSite(SCAB_Common.UIExtensionConstants.FILEDROPDOWN, fileItem);

            // Load the menu structure from App.config
            UIElementBuilder.LoadFromConfig(RootWorkItem);

            //show the main window
            shellWindow.Show();
        }
    }
}
