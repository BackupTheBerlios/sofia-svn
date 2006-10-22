
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
		
		public  NewCommand(CommandReceiver commandReceiver, IController controller, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description) 
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
