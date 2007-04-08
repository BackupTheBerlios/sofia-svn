using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

using Sofia.Reflection;
using System.Collections.ObjectModel;

namespace Sofia.Plugins
{
    public class PluginManager : KeyedCollection<string, IPlugin>

    {
        public void AutoRegister()
        {
            DirectoryInfo dir = new DirectoryInfo(AppDomain.CurrentDomain.BaseDirectory);
            FileInfo[]      files;
            files = dir.GetFiles("Sofia.Plugins.*.dll");

            foreach (FileInfo file in files)
            {
                IPlugin plugin = (IPlugin)InstanceFactory.CreateInstanceFrom(file.FullName, typeof(IPlugin), null, Type.EmptyTypes);
                if (plugin != null)
                    this.Add(plugin);
            }

        }

        protected override string GetKeyForItem(IPlugin item)
        {
            return item.GetType().FullName;
        }
    }
}
