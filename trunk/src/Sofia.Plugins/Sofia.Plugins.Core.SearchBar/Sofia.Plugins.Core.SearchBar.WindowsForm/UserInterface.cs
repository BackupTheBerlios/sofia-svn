using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

using Sofia.Plugins.WindowsForm;

namespace Sofia.Plugins.General.Contact.WindowsForm
{
    public partial class UserInterface : ViewBase
    {
        public UserInterface()
        {
            InitializeComponent();
        }

        public override void LoadFrom(string raw, Sofia.Mvc.ViewFormat viewFormat)
        {
        }

        public override string ContentXml
        {
            get
            {
                return "";
            }
        }


        public override bool IsMasterView
        {
            get
            {
                return false;
            }
        }

        public override string ContentSummary
        {
            get
            {
                return "";
            }
        }

        public override string[] Tags
        {
            get
            {
                return new string[] { };
            }
        }



    }
}
