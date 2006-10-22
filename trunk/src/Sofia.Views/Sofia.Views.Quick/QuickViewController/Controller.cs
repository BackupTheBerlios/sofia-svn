
using System;
using System.Collections;

using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	///<summary>
	///Controleur de la vue
	///</summary>
	public class Controller : BaseController
	{
	
		CommandReceiver commandReceiver;
		
		public Controller()
		{				
		  	//Création des commandes
		  	commandReceiver = new CommandReceiver();
		  	CommandManager.RegisterCommand(new NewCommand(commandReceiver, this, "New", "", "", "", ""));
		  	CommandManager.RegisterCommand(new ToggleRecentFilterCommand(commandReceiver, "ToggleRecent", "Documents récents", Stock.RecentIcon, "", "Affiche les documents récemment consultés ou créés."));
		  	CommandManager.RegisterCommand(new ToggleFavoritesFilterCommand(commandReceiver, "ToggleFavorites", "Documents favoris", Stock.FavoritesIcon, "", "Affiche les documents favoris."));
		  	CommandManager.RegisterCommand(new ToggleTrashFilterCommand(commandReceiver, "ToggleTrash", "Documents récents", Stock.TrashIcon, "", "Affiche les documents placés dans la corbeille."));
		  	
		  	commandReceiver.View
		}
		
		/// <summary>
		/// Implémentationde l'interface 
		/// </summary>
		public override IView View {
			get { return commandReceiver.View; }
		}

		public override void NotifyRequest(string request)
		{
			Console.WriteLine("QuickView est notifié d'une requête au modèle : " + request);
			Sofia.Core.XmlTools.XPathNavigatorFacade xpn = new Sofia.Core.XmlTools.XPathNavigatorFacade();
			xpn.LoadXML(request);
    		ArrayList objects = xpn.GetAttributes("//Request", "object");
    		
    		for (int i = 0; i < objects.Count; i++) {
  			
     				string obj = objects[i].ToString();
     		    
     				if ((obj == "MasterDocument") || (obj == "Document"))
     				{
     		    		
     					
     		    		//Test d'ajout de noeud dans le treeview
     		    		
     		    		
     				}
    		}
     		    		
     		    			
		} 
		
	}
	
}
