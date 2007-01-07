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

#if GTK
        private static string UILibraryName = ".Gtk";
#else
        private static string UILibraryName = ".WindowsForm";
#endif

        public PluginBase()
        {
            //Instanciation de la vue            
            string assemblyName = Assembly.GetCallingAssembly().Location;
            assemblyName = assemblyName.Insert(assemblyName.Length - 4, "." + UILibraryName);
            _View = (IView)InstanceFactory.CreateInstanceFrom(assemblyName, typeof(IView));
        }

        #region Implémentation de l'interface

        IController _Controller;
        IView _View;
        IModel _Model;

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

        public virtual string Description
        {
            get
            {
                throw new NotSupportedException();
            }
        }

        #endregion

    }

}
