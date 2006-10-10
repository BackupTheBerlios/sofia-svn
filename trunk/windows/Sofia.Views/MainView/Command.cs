
using System;
using Sofia.Core;

namespace Sofia.Views.MainView
{
	///<summary>
	///Commande de création de l'interface graphique de la vue
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
	
	///<summary>
	///Commande permettant de créer une nouvelle vue
	///</summary>
	public class NewViewCommand : BaseCommand
	{
	
		CommandReceiver commandReceiver;
		string viewName;
		
		public NewViewCommand(CommandReceiver commandReceiver, string viewName, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description)
		{
			this.commandReceiver = commandReceiver;
			this.viewName = viewName;
		}
		
		public override void Execute(string parameters)
		{
			if (viewName.Length != 0) {
				commandReceiver.NewView(viewName);
			}
	    }
	 }
	 
	///<summary>
	///Commande de sauvegarde d'une vue
	///</summary>
	public class SaveCurrentViewCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		
		public SaveCurrentViewCommand(CommandReceiver commandReceiver, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description)
		{
			this.commandReceiver = commandReceiver;
		}
		
		public override void Execute(string parameters)
		{
			commandReceiver.SaveCurrentView();
	    }
	 }
	 
	///<summary>
	///Commande permettant d'ouvrir une vue et son document
	///</summary>
	public class OpenViewCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
				
		public OpenViewCommand(CommandReceiver commandReceiver, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description)
		{
			this.commandReceiver = commandReceiver;
		}
		
		public override void Execute(string parameters)
		{
    		base.Execute(parameters);
    		
     		string name = ParamList["name"].ToString();
     		string documentid = ParamList["documentid"].ToString();
     		
			commandReceiver.OpenView(name, documentid);
	    }
	 }

	
}
