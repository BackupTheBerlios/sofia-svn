using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

using Sofia.Core.Plugins;

namespace Sofia.Core.Plugins.WindowsForm
{
    public class ViewHostBase: Form, IViewHost
    {
        #region Contructeurs

        /// <summary>
        /// Constructeur par d�faut pour Form
        /// </summary>
        public ViewHostBase() { }

        /// <summary>
        /// Constructeur
        /// </summary>
        /// <param name="pluginsPath">Emplacement des plugins</param>
        public ViewHostBase(string pluginsPath)
            : base()
        {
            _PluginManager = new PluginManager(pluginsPath);
        }

        #endregion

        #region Propri�t�s

        PluginManager _PluginManager;
        public PluginManager PluginManager
        {
            get
            {
                return _PluginManager;
            }
        }

        #endregion

        #region Impl�mentation de l'interface

        public virtual void Insert(IPlugin plugin, string destination)
        {
            
        }


        #endregion

    }
}
