using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Windows.Forms;
using System.Xml.Serialization;

using Sofia.Mvc;
using Sofia.DesignPatterns;

namespace Sofia.Plugins.WindowsForm
{
    public class ViewBase : UserControl, IView, IObservable
    {
        IController _Controller;
        IModel _Model;
        Guid _ContentId;
        List<IObserver> _Observers;
        int _Index;

        #region Constructeur

        public ViewBase()
            : base() {
                InitializeComponent();
        }

        public ViewBase(IModel model, IController controller)
            : base()
        {
            _Observers = new List<IObserver>();
            _Model = model;
            _Controller = controller;
            _Controller.Add(this);
        }

        #endregion


        #region Implémentation de l'interface IView

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

        public virtual object Toolbar
        {
            get
            {
                return null;
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
                return "ViewBase.ContentSummary";
            }
        }

        public virtual string ContentXml
        {
            get
            {
                return "ViewBase.ContentXml";
            }
        }

        public virtual bool IsMasterView
        {
            get
            {
                return false;
            }
        }

        public virtual string[] Tags
        {
            get
            {
                return new string[] { "ViewBase.Tags" };
            }
        }

        public int Index
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

        private void InitializeComponent()
        {
            this.SuspendLayout();
            // 
            // ViewBase
            // 
            this.Name = "ViewBase";
            this.Size = new System.Drawing.Size(168, 175);
            this.ResumeLayout(false);

        }


        #region IObservable Members
       

        public void RegisterObserver(IObserver o)
        {
            if (!_Observers.Contains(o))
                _Observers.Add(o);
        }

        public void UnregisterObserver(IObserver o)
        {
            _Observers.Remove(o);
        }

        public void NotifyObservers(object notification)
        {
            foreach (IObserver o in _Observers)
            {
                o.Update(this, notification);
            }
        }

        #endregion
    }
}
