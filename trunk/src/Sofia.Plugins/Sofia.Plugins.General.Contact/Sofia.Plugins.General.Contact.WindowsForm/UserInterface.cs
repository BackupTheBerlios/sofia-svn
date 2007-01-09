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

            IsMasterView = true;
        }

        public override void LoadFromXml(string rawXml)
        {
            Document document = (Document)Deserialize(rawXml, typeof(Document));

            textBox1.Text = document.Nom;
            textBox2.Text = document.Prenom;
        }

        public override string SaveToXml()
        {
            Document document = new Document();
            document.Nom = textBox1.Text;
            document.Prenom = textBox2.Text;

            return Serialize(document);
        }

        public override string ContentSummary
        {
            get
            {
                return string.Format("{0} {1}", textBox1.Text, textBox2.Text);
            }
        }

        public override string[] Tags
        {
            get
            {
                return new string[] { "Contact", textBox1.Text };
            }
        }



    }
}
