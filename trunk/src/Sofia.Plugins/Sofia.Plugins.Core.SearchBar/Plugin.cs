using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Plugins;
using Sofia.Mvc;

namespace Sofia.Plugins.Core.Search
{
    public class Plugin : PluginBase
    {
        public Plugin() : base() { }
        
        public override string Description
        {
            get
            {
                return "Recherche";
            }
        }


    }
}
