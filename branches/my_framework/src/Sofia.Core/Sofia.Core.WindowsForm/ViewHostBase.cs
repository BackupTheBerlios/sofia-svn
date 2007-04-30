using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

using Sofia.Plugins;
using Sofia.Mvc;
using Sofia.DesignPatterns;

namespace Sofia.Plugins.WindowsForm
{
    public class ViewHostBase: Form, IViewHost, IObserver
    {
        #region Contructor

        /// <summary>
        /// Constructor
        /// </summary>
        public ViewHostBase()
            : base()
        {
            _PluginManager = new PluginManager();
        }

        #endregion

        #region Propriétés

        PluginManager _PluginManager;
        public PluginManager PluginManager
        {
            get
            {
                return _PluginManager;
            }
        }

        #endregion

        #region IViewHost Members

        public virtual void ShowView(IView view, ViewDestination destination)
        {
            throw new NotSupportedException();   
        }

        public virtual void ShowToolBar(IView view, int row)
        {
            throw new NotSupportedException();
        }

        public virtual void Save()
        {
            throw new NotSupportedException();
        }

        public virtual void New()
        {
            throw new NotSupportedException();
        }

        #endregion

        #region IObserver Members

        public virtual void Update(object sender, object notification)
        {
            throw new NotSupportedException();
        }

        #endregion
    }
}
