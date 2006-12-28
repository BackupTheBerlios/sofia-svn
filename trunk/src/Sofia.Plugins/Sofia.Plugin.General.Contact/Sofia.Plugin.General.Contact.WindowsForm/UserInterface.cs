using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

#if GTK
using Sofia.Core.Plugins.Gtk;
#else
using Sofia.Core.Plugins.WindowsForms;
#endif

namespace Sofia.Plugin.General.Contact.WindowsForm
{
    public partial class UserInterface : ViewBase
    {
        public UserInterface()
        {
            InitializeComponent();
        }
    }
}
