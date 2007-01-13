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
        IModel _Model;

        string _PluginAssembly;
        string _ViewAssembly;

#if GTK
        private static string UILibraryName = ".Gtk";
#else
        private static string UILibraryName = "WindowsForm";
#endif        

        public PluginBase() {

            _PluginAssembly = this.GetType().Assembly.CodeBase;
            _ViewAssembly = _PluginAssembly.Insert(_PluginAssembly.Length - 4, "." + UILibraryName);

            CreateModel();
            CreateController();
        }

        #region Implémentation de l'interface

        public void CreateModel()
        {
            _Model = new Model();
        }

        public void CreateController()
        {
            _Controller = (IController)InstanceFactory.CreateInstanceFrom(_PluginAssembly, typeof(IController), new object[] { _Model }, new Type[] { typeof(IModel) });
        }

        public IView CreateView()
        {
            return (IView)InstanceFactory.CreateInstanceFrom(_ViewAssembly, typeof(IView), new object[] { _Model, _Controller }, new Type[] { typeof(IModel), typeof(IController) });    
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
