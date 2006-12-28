using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Sofia.Core.Plugins
{
    class PluginManager
    {
        #region Constructeur

        /// <summary>
        /// Constructeur
        /// </summary>
        public PluginManager(string pluginsPath)
        {
            _Plugins = new List<IPlugin>();

            _PluginPath = pluginsPath;
        }

        #endregion

        List<IPlugin> _Plugins;
        string _PluginPath;

        /// <summary>
        /// Ajoute un plugin dans la liste des plugins
        /// </summary>
        /// <param name="plugin">Un objet qui implémente IPlugin</param>
        public void Add(IPlugin plugin)
        {
            if (!_Plugins.Contains(plugin))
                _Plugins.Add(plugin);
        }

        /// <summary>
        /// Accès aux objets plugins de la liste des plugins
        /// </summary>
        /// <param name="id">Identifiant du plugin</param>
        /// <returns>Un objet plugin</returns>
        public IPlugin this[string id]
        {
            get
            {
                return _Plugins.Find(delegate(IPlugin plugin) { return plugin.Id == id; });
            }
        }

        public void AutoRegister()
        {
            DirectoryInfo dir = new DirectoryInfo(_PluginPath);
            FileInfo[]      files;
            files = dir.GetFiles("*.dll");

            foreach (FileInfo file in files)
            {
                IPlugin plugin = (IPlugin)CreateObjectInstance(file.Name, typeof(IPlugin));
                IController controller = (IController)CreateObjectInstance("Sofia.Plugin.General.Contact.dll", typeof(IController));
                IView view = (IView)CreateObjectInstance("Sofia.Plugin.General.Contact.WindowsForm.dll", typeof(IView));
                

            }

        }

    }
}
