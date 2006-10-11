
using System;
using Sofia.Core;

namespace Sofia.Views.StartBarView
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
		}
		
		/// <summary>
		/// Implémentationde l'interface 
		/// </summary>
		public override IView View {
			get { return commandReceiver.View; }
		}

		public override void NotifyRequest(string request)
		{
			Console.WriteLine("StartBarView est notifié d'une requête au modèle : " + request);
		} 
		
	}
	
}
