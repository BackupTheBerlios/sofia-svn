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

        public override void  Load(string raw, Sofia.Mvc.ViewFormat viewFormat)
        {
            Document document = (Document)Deserialize(rawXml, typeof(Document));

            textBox1.Text = document.Nom;
            textBox2.Text = document.Prenom;
        }

        public virtual string ContentXml
        {
            get
            {
                Document document = new Document();
                document.Nom = textBox1.Text;
                document.Prenom = textBox2.Text;
                return Serialize(document);
            }
        }


        public override bool IsMasterView
        {
            get
            {
                return true;
            }
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
