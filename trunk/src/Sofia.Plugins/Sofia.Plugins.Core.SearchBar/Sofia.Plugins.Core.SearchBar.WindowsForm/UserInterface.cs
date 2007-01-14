using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

using Sofia.Plugins.WindowsForm;
using Sofia.Mvc;

namespace Sofia.Plugins.Core.Search.WindowsForm
{
    public partial class UserInterface : ViewBase
    {
        public UserInterface()
        {
            InitializeComponent();
        }

        public UserInterface(IModel model, IController controller)
            : base(model, controller)
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
                return new string[] { "Recherche" };
            }
        }

        public override object Toolbar
        {
            get
            {
                return _ToolbarSearch;
            }
        }
    }
}
