
using System;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	public class NewCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		IController controller;
		
		public  NewCommand(CommandReceiver commandReceiver, IController controller) : base ("New", "", "", "", "") 
		{			
			this.commandReceiver  = commandReceiver;
			this.controller = controller;
		}
		
		public override void Execute(string parameters)
		{
			commandReceiver.CreateGui(controller);
		}
		
	}
	
	public class ApplySearchCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		ToolbarButton _ToolbarButton;
				
		public ApplySearchCommand(CommandReceiver commandReceiver) : base ("ApplySearch", "Appliquer", Stock.ApplyIcon, "", "Rechercher dans tous les documents") 
		{			
			this.commandReceiver  = commandReceiver;
			_ToolbarButton = new ToolbarButton(this);
		}
		
		public override ToolbarItem ToolbarItem 
		{
			get { return _ToolbarButton; }
		}
		
		public override void Execute(string parameters)
		{
			commandReceiver.ApplySearch();
		}
		
	}
	
	
	///<summary>
	///Filtre document récents
	///</summary>
	public class ToggleRecentFilterCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		ToolbarToggle _ToolbarToggle;
		
		public ToggleRecentFilterCommand(CommandReceiver commandReceiver) : base ("ToggleRecent", "Documents récents", Stock.RecentIcon, "", "Affiche les documents récemment consultés ou créés.") 
		{			
			this.commandReceiver  = commandReceiver;
			_ToolbarToggle = new ToolbarToggle(this);
		}
		
		public override ToolbarItem ToolbarItem 
		{
			get { return _ToolbarToggle; }
		}
				
		public override void Execute(string parameters)
		{
//			Manager.GetCommand("ToggleFavorite").UnExecute();
//			Manager.GetCommand("ToggleTrash").UnExecute();
			
			commandReceiver.ToggleRecentFilter(true);
		}
		
		public override void UnExecute()
		{
			_ToolbarToggle.Button.Active = false;
			commandReceiver.ToggleRecentFilter(false);
		}
		
	}

	///<summary>
	///Filtre document favoris
	///</summary>
	public class ToggleFavoritesFilterCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		ToolbarToggle _ToolbarToggle;
		
		public ToggleFavoritesFilterCommand(CommandReceiver commandReceiver) : base ("ToggleFavorites", "Documents favoris", Stock.FavoritesIcon, "", "Affiche les documents favoris.") 
		{			
			this.commandReceiver  = commandReceiver;
			_ToolbarToggle = new ToolbarToggle(this);
		}
		
		public override ToolbarItem ToolbarItem 
		{
			get { return _ToolbarToggle; }
		}
		
		public override void Execute(string parameters)
		{
//			Manager.GetCommand("ToggleRecent").UnExecute();
//			Manager.GetCommand("ToggleTrash").UnExecute();
			
			commandReceiver.ToggleFavoritesFilter(true);
		}
		
		public override void UnExecute()
		{
			_ToolbarToggle.Button.Active = false;
			commandReceiver.ToggleFavoritesFilter(false);
		}
		
	}

	///<summary>
	///Filtre documents supprimés
	///</summary>
	public class ToggleTrashFilterCommand : BaseCommand
	{
		CommandReceiver commandReceiver;
		ToolbarToggle _ToolbarToggle;

		public ToggleTrashFilterCommand(CommandReceiver commandReceiver) : base ("ToggleTrash", "Documents récents", Stock.TrashIcon, "", "Affiche les documents placés dans la corbeille.") 
		{			
			this.commandReceiver  = commandReceiver;
			_ToolbarToggle = new ToolbarToggle(this);
		}
		
		public override ToolbarItem ToolbarItem 
		{
			get { return _ToolbarToggle; }
		}
		
		public override void Execute(string parameters)
		{
//			Manager.GetCommand("ToggleFavorites").UnExecute();
//			Manager.GetCommand("ToggleRecent").UnExecute();
			
			commandReceiver.ToggleTrashFilter(true);
		}
		
		public override void UnExecute()
		{
			_ToolbarToggle.Button.Active = false;
			commandReceiver.ToggleTrashFilter(false);
		}
		
	}
		
	
}
