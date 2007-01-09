using System;
using System.Collections;
using System.Text;
using System.IO;
using System.Windows.Forms;
using System.Xml.Serialization;

using Sofia.Mvc;

namespace Sofia.Plugins.WindowsForm
{
    public class ViewBase: UserControl, IView
    {
        #region Constructeur

        public ViewBase() : base() { }

        #endregion

        #region Implémentation de l'interface IView

        IController _Controller;
        Guid _ContentId;
        bool _IsMasterView;

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

        public IController Controller
        {
            get
            {
                return _Controller;
            }
            set
            {
                _Controller = value;
            }
        }

        public Guid ContentId
        {
            get
            {
                return _ContentId;
            }
            set
            {
                _ContentId = value;
            }

        }

        public virtual string ContentSummary
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public bool IsMasterView
        {
            get
            {
                return _IsMasterView;
            }

            set
            {
                _IsMasterView = value;
            }
        }

        public virtual string[] Tags
        {
            get
            {
                throw new NotImplementedException();
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

        public object Deserialize(string rawXml, Type documentType)
        {
            XmlSerializer xmlSerializer = new XmlSerializer(documentType);
            TextReader reader = new StringReader(rawXml);
            return xmlSerializer.Deserialize(reader);
        }


    }
}
