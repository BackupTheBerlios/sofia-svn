using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

using Sofia.Reflection;
using Sofia.Mvc;

namespace Sofia.Plugins
{
    public class PluginBase : IPlugin
    {
        IController _Controller;
        IView _View;
        IModel _Model;

#if GTK
        private static string UILibraryName = ".Gtk";
#else
        private static string UILibraryName = "WindowsForm";
#endif

        public PluginBase()
        {
        }

        #region Implémentation de l'interface

        public IView View
        {
            get
            {
                return _View;
            }

            set
            {
                _View = value;
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

        public void CreateView(string pluginName)
        {
            //Instanciation de la vue           
            string path = AppDomain.CurrentDomain.BaseDirectory;
            string assemblyName = path + pluginName + ".dll";
            assemblyName = assemblyName.Insert(assemblyName.Length - 4, "." + UILibraryName);
            _View = (IView)InstanceFactory.CreateInstanceFrom(assemblyName, typeof(IView), null);
        }


        public virtual string Description
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        #endregion

    }

}
