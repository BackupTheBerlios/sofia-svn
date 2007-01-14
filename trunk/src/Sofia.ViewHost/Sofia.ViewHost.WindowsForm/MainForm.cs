using System;
using System.Collections.Generic;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Reflection;

using Sofia.ViewHost.WindowsForm.Properties;

using Sofia.Plugins;
using Sofia.Mvc;

#if GTK
using Sofia.Plugins.Gtk;
#else
using Sofia.Plugins.WindowsForm;
#endif

namespace Sofia.ViewHost.WindowsForm
{
    public partial class MainForm : ViewHostBase
    {

        Hashtable _ViewTabs;

        public MainForm()
            : base()
        {
            InitializeComponent();
            _ViewTabs = new Hashtable();
            PluginManager.AutoRegister();

            //Insertion du plugin SearchBar
            IPlugin plugin = PluginManager["Sofia.Plugins.Core.Search"];
            Insert(plugin.CreateView(), "TabControl");
        }

        #region Méthodes héritées

        public override void Insert(IView view, string destination)
        {
            Control control = view.Control as Control;
            control.Dock = DockStyle.Fill;

            if (destination.Equals("TabControl", StringComparison.OrdinalIgnoreCase))
            {
                TabPage tabPage = new TabPage(view.Tags[0]);
                tabPage.Name = view.ContentId.ToString("N");
                tabPage.Controls.Add(control);
                tabPage.Tag = view;
                _Pages.Controls.Add(tabPage);
            }

            if (view.Toolbar != null)
            {
                Control toolbar = view.Toolbar as Control;
                TopToolStripPanel.Controls.Add(toolbar);
            }
        }

        public override void Save()
        {
            IView view = _Pages.SelectedTab.Tag as IView;
            view.SaveTo(ViewFormat.Xml);
        }

        public override void New()
        {
            IPlugin plugin = PluginManager["Sofia.Plugins.General.Contact"];
            IView view = plugin.CreateView();
            view.ContentId = Guid.NewGuid();
            Insert(view, "TabControl");
        }

        #endregion

        #region UI

        private void enregistrerToolStripButton_Click(object sender, EventArgs e)
        {
            Save();
        }

        private void enregistrerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Save();
        }

        private void nouveauToolStripMenuItem_Click(object sender, EventArgs e)
        {
            New();
        }

        private void nouveauToolStripButton_Click(object sender, EventArgs e)
        {
            New();
        }

        #endregion

    }
}