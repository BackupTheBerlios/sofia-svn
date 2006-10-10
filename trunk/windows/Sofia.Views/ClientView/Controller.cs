
using System;
using Sofia.Core;

namespace Sofia.Views.ClientView
{
	///<summary>
	///Controleur de la vue "Client"
	///</summary>
	public class Controller : BaseController
	{
		CommandReceiver cmdRcv;
		CommandManager cmdMgr;
	
		public Controller()
		{	
		  	//Création du gestionnaire de commande
		  	cmdMgr = new CommandManager();
		  	
		  	//Création du receiver des commandes
		  	cmdRcv = new CommandReceiver();
		  	
		  	//Création des commandes
		  	cmdMgr.RegisterCommand(new NewCommand(cmdRcv, this, "New", "Nouveau client", Gtk.Stock.New, "", "Créer un nouveau client dans un nouveau dossier"));
		}
		
		///<summary>
		/// Get GuiActions
		///</summary>
    	public override ICommandReceiver CommandReceiver
		{
			get { return (ICommandReceiver)cmdRcv; }
		}		
		
		public override CommandManager CommandManager	
		{	
			get { return cmdMgr; } 
		}
		
	}
	
}
