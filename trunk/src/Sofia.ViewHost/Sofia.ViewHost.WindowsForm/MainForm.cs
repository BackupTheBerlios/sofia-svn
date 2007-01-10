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

        public MainForm() : base()
        {
            InitializeComponent();
            _ViewTabs = new Hashtable();
            PluginManager.AutoRegister();            
        }

        #region Méthodes héritées

        public override void Insert(IPlugin plugin, string destination)
        {
            base.Insert(plugin, destination);

            if (plugin == null)
                return;

            //Test
            TabPage tabPage = new TabPage("Contact");
            tabPage.Name = plugin.View.ContentId;
            Control control = plugin.View.Control as Control;
            control.Dock = DockStyle.Fill;
            tabPage.Controls.Add(control);
            _Pages.Controls.Add(tabPage);

            _ViewTabs[plugin.View.ContentId] = plugin.View;
        }

        public override void Save()
        {
            IView view = _ViewTabs[_Pages.SelectedTab.Name] as IView;
            view.Save(ViewFormat.Xml);
        }

        public override void New()
        {
            IPlugin plugin = PluginManager["Sofia.Plugins.General.Contact"];
            plugin.CreateView("Sofia.Plugins.General.Contact");
            plugin.View.ContentId = Guid.NewGuid();
            Insert(plugin, "Main");
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