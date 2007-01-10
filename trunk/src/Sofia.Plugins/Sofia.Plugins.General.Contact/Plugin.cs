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

        }

        public override void AddView()
        {
            base.AddView();
            View.Controller = Controller;
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
