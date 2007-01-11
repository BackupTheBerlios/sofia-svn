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
        IModel _Model;
        Guid _ContentId;
        int _Index;

        public virtual void LoadFrom(string raw, ViewFormat viewFormat)
        {
            throw new NotImplementedException();
        }

        public virtual void SaveTo(ViewFormat viewFormat)
        {
            _Model.UpdateDocument(ContentId.ToString("N"), ContentSummary, ContentXml, IsMasterView, Tags);
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

        public IModel Model
        {
            get
            {
                return _Model;
            }
            set
            {
                _Model = value;
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

        public virtual string ContentXml
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public virtual bool IsMasterView
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public virtual string[] Tags
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public virtual int Index
        {
            get
            {
                return _Index;
            }

            set
            {
                _Index = value;
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
