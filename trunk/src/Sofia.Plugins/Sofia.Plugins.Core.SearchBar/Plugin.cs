using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Plugins;
using Sofia.Mvc;

namespace Sofia.Plugins.Core.SearchBar
{
    public class Plugin : PluginBase
    {
        public Plugin() : base() 
        {            
            Controller = new Controller();
            Model = new Model();

            //Liaison Controleur/Modèle
            Controller.Model = Model;

        }

        public override void AddView()
        {
            base.AddView();
            View.Controller = Controller;
            View.Model = Model;
        }

        public override string Description
        {
            get
            {
                return "Barre de recherche rapide";
            }
        }


    }
}
