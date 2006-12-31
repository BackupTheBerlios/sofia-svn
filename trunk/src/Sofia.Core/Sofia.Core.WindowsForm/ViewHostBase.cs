using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

using Sofia.Core.Plugins;
using Sofia.Core.Plugins.WindowsForm;

namespace Sofia.Core.Plugins.WindowsForm
{
    public class ViewHostBase: Form, IViewHost
    {
        #region Contructeur

        public ViewHostBase(string pluginsPath)
            : base()
        {
            _PluginManager = new PluginManager(pluginsPath);
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

        public void Insert(IPlugin plugin, string destination)
        {
            throw new NotImplementedException();
        }


        #endregion

    }
}
