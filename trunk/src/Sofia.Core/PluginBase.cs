using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

namespace Sofia.Core.Plugins
{
    public class PluginBase
    {
        IController _Controller;
        IView _View;
        IModel _Model;

        public PluginBase()
        {
           _View = CreateView(Assembly.GetCallingAssembly());
        }

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

        private IView CreateView(Assembly assembly)
        {
            Type type = typeof(IView);

            string viewAssemblyName = assembly.FullName + "WindowsForm";
            assembly.Location
            Assembly viewAssembly = Assembly.LoadFrom(viewAssemblyName + ".dll");

            foreach (Type exported in assembly.GetExportedTypes())
                if (type.IsAssignableFrom(exported))
                {
                    ConstructorInfo Constructor = exported.GetConstructor(Type.EmptyTypes);
                    if (Constructor != null)
                        return (IView)Constructor.Invoke(null);
                }
            return null;

        }
    }
}
