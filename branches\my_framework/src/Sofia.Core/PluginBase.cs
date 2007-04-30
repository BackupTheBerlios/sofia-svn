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
        #region private fields

        private IController _controller;
        private IModel _model;

        private string _pluginAssembly = string.Empty;
        private string _viewAssembly = string.Empty;
        
        //#if GTK
        // private static string UILibraryName = "Gtk";
        //#endif
        //#if XAML
        private static string _uiLibraryName = "Xaml";
        //#endif 
        //#if WINDOWSFORM
        //private static string UILibraryName = "WindowsForm";
        //#endif        

        #endregion

        #region contructor

        public PluginBase()
        {
            _pluginAssembly = this.GetType().Assembly.CodeBase;
            _viewAssembly = _pluginAssembly.Insert(_pluginAssembly.Length - 4, "." + _uiLibraryName);

            CreateModel();
            CreateController();
        }

        #endregion

        #region Implémentation de l'interface

        public void CreateModel()
        {
            _model = new Model();
        }

        public void CreateController()
        {
            _controller = (IController)InstanceFactory.CreateInstanceFrom(_pluginAssembly, typeof(IController), new object[] { _model }, new Type[] { typeof(IModel) });
        }

        public IView CreateView()
        {
            return (IView)InstanceFactory.CreateInstanceFrom(_viewAssembly, typeof(IView), new object[] { _model, _controller }, new Type[] { typeof(IModel), typeof(IController) });
        }

        public virtual string Description
        {
            get
            {
                throw new NotSupportedException();
            }
        }

        public IController Controller
        {
            get
            {
                return _controller;
            }
        }

        #endregion

    }

}
