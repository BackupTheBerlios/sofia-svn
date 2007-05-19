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
using System.Windows.Shapes;

using Microsoft.Practices.CompositeUI;
using Microsoft.Practices.CompositeUI.Services;
using Microsoft.Practices.ObjectBuilder;
using Microsoft.Practices.CompositeUI.Commands;


namespace Sofia.Cab.Shell
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>

    public partial class ShellWindow : System.Windows.Window
    {

        #region fields

        private WorkItem workItem;
        private IWorkItemTypeCatalogService workItemTypeCatalog;

        #endregion

        #region properties

        public Menu Menu
        {
            get
            {
                return _menu;
            }
        }

        public MenuItem FileMenuItem
        {
            get
            {
                return _fileMenuItem;
            }
        }

        #endregion

        #region ctor

        public ShellWindow()
        {
            InitializeComponent();
        }

        [InjectionConstructor]
        public ShellWindow(WorkItem workItem, IWorkItemTypeCatalogService workItemTypeCatalog)
            : this()
        {
            this.workItem = workItem;
            this.workItemTypeCatalog = workItemTypeCatalog;
        }

        #endregion

        #region Command handlers

        [CommandHandler("FileExit")]
        public void OnFileExit(object sender, EventArgs e)
        {
            Close();
        }

        #endregion
    }
}