
using System;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
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
	
	public class ShowDocumentsCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
				
		public ShowDocumentsCommand(CommandReceiver commandReceiver, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description) 
		{			
			this.commandReceiver  = commandReceiver;
		}
		
		public override void Execute(string parameters)
		{
			commandReceiver.ShowDocuments();
		}
		
	}
	
	
	///<summary>
	///Filtre document récents
	///</summary>
	public class ToggleRecentFilterCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		
		public ToggleRecentFilterCommand(CommandReceiver commandReceiver, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description) 
		{			
			this.commandReceiver  = commandReceiver;
		}
		
		public override void Execute(string parameters)
		{
			commandReceiver.ToggleRecentFilter(true);
		}
		
		public override void UnExecute()
		{
			commandReceiver.ToggleRecentFilter(false);
		}
		
	}

	///<summary>
	///Filtre document favoris
	///</summary>
	public class ToggleFavoritesFilterCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		
		public ToggleFavoritesFilterCommand(CommandReceiver commandReceiver, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description) 
		{			
			this.commandReceiver  = commandReceiver;
		}
		
		public override void Execute(string parameters)
		{
			commandReceiver.ToggleFavoritesFilter(true);
		}
		
		public override void UnExecute()
		{
			commandReceiver.ToggleFavoritesFilter(false);
		}
		
	}

	///<summary>
	///Filtre documents supprimés
	///</summary>
	public class ToggleTrashFilterCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		
		public ToggleTrashFilterCommand(CommandReceiver commandReceiver, string id, string text, string icon, string accelKey, string description) : base (id, text, icon, accelKey, description) 
		{			
			this.commandReceiver  = commandReceiver;
		}
		
		public override void Execute(string parameters)
		{
			commandReceiver.ToggleTrashFilter(true);
		}
		
		public override void UnExecute()
		{
			commandReceiver.ToggleTrashFilter(false);
		}
		
	}
		
	
}
