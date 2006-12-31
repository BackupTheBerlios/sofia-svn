using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Core.Plugins;

namespace Sofia.Plugin.General.Contact
{
    public class Plugin : PluginBase
    {
        public Plugin() : base() 
        {            
            Controller = new Controller();
        }

        public string Description
        {
            get
            {
                return "Gestion des contacts";
            }
        }


    }
}
