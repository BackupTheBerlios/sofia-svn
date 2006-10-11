
using System;
using Sofia.Core;

namespace Sofia.Views.WelcomeView
{
	
	public class NewCommand : BaseCommand
	{
		GuiActions guiActions;
		IController controller;
		
		public  NewCommand(GuiActions guiActions, IController controller, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description) 
		{
			this.guiActions  = guiActions;
			this.controller = controller;
		}
		
		public override void Execute()
		{
			guiActions.CreateGui(controller);
		}
		
	}
	
}
