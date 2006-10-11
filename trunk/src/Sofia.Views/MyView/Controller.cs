
using System;
using Sofia.Core;

namespace Sofia.Views.MyView
{
	
	///<summary>
	///Controleur de la vue
	///</summary>
	public class Controller : BaseController
	{
		CommandReceiver commandReceiver;
			
		public Controller()
		{	
		  	//Création du receiver des commandes
		  	commandReceiver = new CommandReceiver();
		  	
		  	//Création des commandes
		  	CommandManager.RegisterCommand(new NewCommand(commandReceiver, this, "New", "Nouveau", Gtk.Stock.New, "", "Description de la commande"));
		}
		
		/// <summary>
		/// Implémentationde l'interface 
		/// </summary>
		public override IView View {
			get { return commandReceiver.View; }
		}
		
		public override void NotifyRequest(string request)
		{
			Console.WriteLine("La vue est notifié d'une requête au modèle : " + request);
		} 


			
	}
	
}
