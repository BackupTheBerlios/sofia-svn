
using System;
using Sofia.Core;

namespace Sofia.Views.MainView
{
	///<summary>
	///Commande de création de l'interface graphique de la vue
	///</summary>
	public class NewCommand : BaseCommand
	{	
		CommandReceiver cmdRcv;
		IController controller;
		
		public  NewCommand(CommandReceiver cmdRcv, IController controller, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description) 
		{
			this.cmdRcv  = cmdRcv;
			this.controller = controller;
		}
		
		public override void Execute(string parameters)
		{			
			cmdRcv.CreateGui(controller);
		}
		
	}
	
	///<summary>
	///Commande permettant de créer une nouvelle vue
	///</summary>
	public class NewViewCommand : BaseCommand
	{
	
		CommandReceiver cmdRcv;
		string viewName;
		
		public NewViewCommand(CommandReceiver cmdRcv, string viewName, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description)
		{
			this.cmdRcv = cmdRcv;
			this.viewName = viewName;
		}
		
		public override void Execute(string parameters)
		{
			if (viewName.Length != 0) {
				cmdRcv.NewView(viewName);
			}
	    }
	 }
	 
	///<summary>
	///Commande de sauvegarde d'une vue
	///</summary>
	public class SaveCurrentViewCommand : BaseCommand
	{
		CommandReceiver cmdRcv;
		
		public SaveCurrentViewCommand(CommandReceiver cmdRcv, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description)
		{
			this.cmdRcv = cmdRcv;
		}
		
		public override void Execute(string parameters)
		{
			cmdRcv.SaveCurrentView();
	    }
	 }
	 
	///<summary>
	///Commande permettant d'ouvrir une vue et son document
	///</summary>
	public class OpenViewCommand : BaseCommand
	{
	
		CommandReceiver cmdRcv;
				
		public OpenViewCommand(CommandReceiver cmdRcv, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description)
		{
			this.cmdRcv = cmdRcv;
		}
		
		public override void Execute(string parameters)
		{
    		base.Execute(parameters);
    		
     		string name = ParamList["name"].ToString();
     		string documentid = ParamList["documentid"].ToString();;
     		
			cmdRcv.OpenView(name, documentid);
	    }
	 }

	
}
