
using System;
using System.Xml;

namespace Sofia.Core
{
	
	public interface IView
    {
    	//Composant graphique de la vue
        Gtk.HBox VisualComponent { get; }
        
        //Controleur de la vue
        IController Controller { get; set; }
        
        //Texte associé à la vue destiné à l'affichage
        string Caption { get; }
        
        //Texte fourni au dossier dans le cas d'un document maître
        string MasterCaption { get; }
        
        //Zone de la vue principale où peut s'insérer la vue
        // ex : default, left, etc.
        string Destination { get; }
        
        //Identifiant de document chargé dans la vue
        string DocumentID {get; set; }
        
        //Représentation XML de la vue
        void LoadFromXML(string xml);
        string SaveToXML();
        
        void Initialize();
        
        void AddToolbarItem(ToolbarItem item);

    }
      
}
