using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Windows.Forms;
using System.Xml.Serialization;

using Sofia.Core.Plugins;

namespace Sofia.Core.Plugins.WindowsForm
{
    public class ViewBase: UserControl, IView
    {
        #region Constructeur

        public ViewBase() : base() { }

        public ViewBase(IController controller): base()
        {
            _Controller = controller;
        }

        #endregion

        #region Implémentation de l'interface IView

        IController _Controller;

        public IController Controller
        {
            get
            {
                return _Controller;
            }
        }

        public virtual void LoadFromXml(string rawXml)
        {
            throw new NotSupportedException();
        }

        public virtual string SaveToXml()
        {
            throw new NotSupportedException();
        }

        public object Control
        {
            get
            {
                return this;
            }
        }

        #endregion

        public string Serialize(object document)
        {
            XmlSerializer xmlSerializer = new XmlSerializer(document.GetType());
            StringBuilder sb = new StringBuilder();
            TextWriter writer = new StringWriter(sb);
            xmlSerializer.Serialize(writer, document);
            return sb.ToString();
        }


    }
}
