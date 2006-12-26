
using System;
using Sofia.Core;

namespace Sofia.Views.ClientView
{
	
	///<summary>
	///Commande d'instanciation de la vue
	///</summary>
	public class NewCommand : BaseCommand
	{
	
		CommandReceiver commandReceiver;
		IController controller;
		
		public  NewCommand(CommandReceiver commandReceiver, IController controller) : base ("New", "Nouveau client", Stock.NewIcon, "", "Créer un nouveau client dans un nouveau dossier") 
		{
			this.commandReceiver  = commandReceiver;
			this.controller = controller;
		}
		
		public override void Execute(string parameters)
		{
			commandReceiver.CreateGui(controller);
		}
		
	}

	
}
