using System;
using System.Collections.Generic;
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

        public override void LoadFromXml(string rawXml)
        {
            throw new NotImplementedException();
        }

        public override string SaveToXml()
        {
            Document document = new Document();
            document.Nom = textBox1.Text;
            document.Prenom = "test";

            return Serialize(document);
        }


    }
}
