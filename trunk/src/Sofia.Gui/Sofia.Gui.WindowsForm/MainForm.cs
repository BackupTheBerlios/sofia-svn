using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Reflection;

using Sofia.Gui.WindowsForm.Properties;

using Sofia.Core.Plugins;

#if GTK
using Sofia.Core.Plugins.Gtk;
#else
using Sofia.Core.Plugins.WindowsForm;
#endif

namespace Sofia.Gui.WindowsForm
{
    public partial class MainForm : ViewHostBase
    {

        public MainForm() : base(Settings.Default.PluginsPath)
        {
            InitializeComponent();
            PluginManager.AutoRegister();
        }
    }
}