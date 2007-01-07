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

        public MainForm() : base(Settings.Default.PluginsPath)
        {
            InitializeComponent();          
            PluginManager.AutoRegister();

            NewPlugin(PluginManager["Sofia.Plugins.General.Contact.Plugin"], "");
        }

        public void NewPlugin(IPlugin plugin, string destination)
        {
            plugin.View.ContentId = Guid.NewGuid();
            Insert(plugin, destination);
        }

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

        private void toolStripButton1_Click(object sender, EventArgs e)
        {            
            IPlugin plugin = PluginManager[_Pages.SelectedTab.Name];
            plugin.Controller.Save();
        }
    }
}