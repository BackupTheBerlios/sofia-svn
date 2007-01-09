using System;
using System.Collections.Generic;
using System.Text;

using Sofia.Plugins;
using Sofia.Mvc;

namespace Sofia.Plugins.General.Contact
{
    public class Plugin : PluginBase
    {
        public Plugin() : base() 
        {            
            Controller = new Controller();
            Model = new Model();

            //Liaison Controleur/Modèle
            Controller.Model = Model;

            //Liaison Vue/Controleur
            CreateView("Sofia.Plugins.General.Contact");
            View.Controller = Controller;
            Controller.View = View;
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
