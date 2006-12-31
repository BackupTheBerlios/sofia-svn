using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

using Sofia.Core.Reflection;

namespace Sofia.Core.Plugins
{
    public class PluginBase : IPlugin
    {
        public PluginBase()
        {
            //Instanciation de la vue            
            string assemblyName = Assembly.GetCallingAssembly().Location;
            assemblyName = assemblyName.Insert(assemblyName.Length - 4, ".WindowsForm");
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

        public string Description
        {
            get
            {
                throw new NotSupportedException();
            }
        }

        #endregion

    }

}
