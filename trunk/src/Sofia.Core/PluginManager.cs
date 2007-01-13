using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

using Sofia.Reflection;

namespace Sofia.Plugins
{
    public class PluginManager
    {
        #region Constructeur

        /// <summary>
        /// Constructeur
        /// </summary>
        public PluginManager()
        {
            _Plugins = new List<IPlugin>();
        }

        #endregion

        List<IPlugin> _Plugins;

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
        /// <example>fullname : Sofia.Plugins.General.Contact</example>
        public IPlugin this[string fullName]
        {
            get
            {
                return _Plugins.Find(delegate(IPlugin plugin) { return plugin.GetType().FullName == fullName + ".Plugin"; });
            }
        }

        public void AutoRegister()
        {
            DirectoryInfo dir = new DirectoryInfo(AppDomain.CurrentDomain.BaseDirectory);
            FileInfo[]      files;
            files = dir.GetFiles("Sofia.Plugins.*.dll");

            foreach (FileInfo file in files)
            {
                IPlugin plugin = (IPlugin)InstanceFactory.CreateInstanceFrom(file.FullName, typeof(IPlugin), null, Type.EmptyTypes);
                if (plugin != null)
                    Add(plugin);
            }

        }

    }
}
