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
        List<IView> _Views;

#if GTK
        private static string UILibraryName = ".Gtk";
#else
        private static string UILibraryName = "WindowsForm";
#endif

        public PluginBase()
        {
            _Views = new List<IView>();
        }

        #region Implémentation de l'interface

        public IView View
        {
            get
            {
                return _Views[_Views.Count - 1];
            }

            set
            {
                _Views.Add(value);
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

        public virtual void AddView()
        {
            //Instanciation de la vue 
            string path = AppDomain.CurrentDomain.BaseDirectory;
            string assemblyName = this.GetType().Assembly.CodeBase;
            assemblyName = assemblyName.Insert(assemblyName.Length - 4, "." + UILibraryName);
            _View = (IView)InstanceFactory.CreateInstanceFrom(assemblyName, typeof(IView), null);
            _Views.Add(_View);
            _View.Index = _Views.IndexOf(_View);
        }

        public IView GetView(string contentId)
        {
            return _Views.Find(delegate(IView view) { return view.ContentId.ToString("N") == contentId; });
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
