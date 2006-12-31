using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Core.Plugins;

namespace Sofia.Plugins.General.Contact
{
    public class Plugin : PluginBase
    {
        public Plugin() : base() 
        {            
            Controller = new Controller();
        }

        public override string Description
        {
            get
            {
                return "Gestion des contacts";
            }
        }


    }
}
