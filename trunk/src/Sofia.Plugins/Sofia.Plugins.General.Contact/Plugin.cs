using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Core.Plugins;
using Sofia.Model;

namespace Sofia.Plugins.General.Contact
{
    public class Plugin : PluginBase
    {
        public Plugin() : base() 
        {            
            Controller = new Controller();
            Model = new Model();
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
