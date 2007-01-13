using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

using Sofia.Plugins;
using Sofia.Mvc;

namespace Sofia.Plugins.WindowsForm
{
    public class ViewHostBase: Form, IViewHost
    {
        #region Contructeurs

        /// <summary>
        /// Constructeur
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

        #region Implémentation de l'interface

        public virtual void Insert(IView view, string destination)
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

    }
}
