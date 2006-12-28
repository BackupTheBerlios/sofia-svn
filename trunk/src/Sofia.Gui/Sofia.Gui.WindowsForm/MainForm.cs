using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Reflection;

using Sofia.Core.Plugins;

#if GTK
using Sofia.Core.Plugins.Gtk;
#else
using Sofia.Core.Plugins.WindowsForms;
#endif

namespace Sofia.Gui.WindowsForm
{
    public partial class MainForm : Form, IViewHost
    {
        public MainForm()
        {
            InitializeComponent();


        }

        #region Implémentation de l'interface

        public void Insert(IPlugin plugin, string destination)
        {
            throw new NotImplementedException();
        }       


        #endregion

    }
}