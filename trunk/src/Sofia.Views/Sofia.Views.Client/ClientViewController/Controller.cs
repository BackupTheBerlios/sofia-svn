
using System;
using Sofia.Core;

namespace Sofia.Views.ClientView
{
	///<summary>
	///Controleur de la vue "Client"
	///</summary>
	public class Controller : BaseController
	{
		CommandReceiver commandReceiver;
	
		public Controller()
		{	
		  	//Création du receiver des commandes
		  	commandReceiver = new CommandReceiver();
		  	
		  	//Création des commandes
		  	CommandManager.RegisterCommand(new NewCommand(commandReceiver, this, "New", "Nouveau client", Stock.NewIcon, "", "Créer un nouveau client dans un nouveau dossier"));
		}
		
		/// <summary>
		/// Implémentationde l'interface 
		/// </summary>
		public override IView View {
			get { return commandReceiver.View; }
		}

		
	}
	
}
