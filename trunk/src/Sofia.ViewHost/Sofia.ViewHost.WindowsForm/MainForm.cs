using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Reflection;

using Sofia.ViewHost.WindowsForm.Properties;

using Sofia.Plugins;

#if GTK
using Sofia.Plugins.Gtk;
#else
using Sofia.Plugins.WindowsForm;
#endif

namespace Sofia.ViewHost.WindowsForm
{
    public partial class MainForm : ViewHostBase
    {

        public MainForm() : base()
        {
            InitializeComponent();          
            PluginManager.AutoRegister();            
        }

        public void NewPlugin(IPlugin plugin, string destination)
        {
            plugin.View.ContentId = Guid.NewGuid();
            Insert(plugin, destination);
        }

        #region Méthodes héritées

        public override void Insert(IPlugin plugin, string destination)
        {
            base.Insert(plugin, destination);

            if (plugin == null)
                return;

            //Test
            TabPage tabPage = new TabPage("Contact");
            tabPage.Name = plugin.GetType().FullName;
            Control control = plugin.View.Control as Control;
            control.Dock = DockStyle.Fill;
            tabPage.Controls.Add(control);
            _Pages.Controls.Add(tabPage);
        }

        public override void Save()
        {
            IPlugin plugin = PluginManager[_Pages.SelectedTab.Name];
            plugin.Controller.Save();
        }

        public override void New()
        {
            NewPlugin(PluginManager["Sofia.Plugins.General.Contact.Plugin"], "");
        }

        #region

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
    }
}