using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Core.Plugins;

namespace Sofia.Plugin.General.Contact
{
    class Plugin : PluginBase
    {
        public Plugin() : base() 
        {            
            Controller = new Controller();            
        }
    }
}
