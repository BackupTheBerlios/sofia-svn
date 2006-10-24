
using System;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	/// <summary>
	/// Classe receiver 
	/// </summary>
	public class CommandReceiver : BaseCommandReceiver
	{
		ViewGui viewGui;
		
		public override IView View { get { return (IView)viewGui; } }
		
		/// <summary>
		/// Création de l'interface graphique 
		/// </summary>
		public void CreateGui(IController controller)
		{	
			viewGui = new ViewGui();
			viewGui.Controller = controller;
		  	viewGui.Initialize();
		  	
		  	View.AddToolbarItem(controller.CommandManager.GetCommand("ApplySearch").ToolbarItem);
		  	View.AddToolbarItem(new ToolbarSeparator());
		  	View.AddToolbarItem(controller.CommandManager.GetCommand("ToggleRecent").ToolbarItem);
		  	View.AddToolbarItem(controller.CommandManager.GetCommand("ToggleFavorites").ToolbarItem);
		  	View.AddToolbarItem(controller.CommandManager.GetCommand("ToggleTrash").ToolbarItem);
		}

		/// <summary>
		/// Affichage de documents 
		/// </summary>
		public void ApplySearch()
		{			
			TreeViewBuilder treeViewBuilder = new TreeViewBuilder(viewGui.TreeViewDoc);
			
			//Requête
			viewGui.Controller.RequestModel(xmlDoc.ToString());
		}
		
		public void ToggleRecentFilter(bool active)
		{
		
		}
		
		public void ToggleFavoritesFilter(bool active)
		{
			
		}
		
		public void ToggleTrashFilter(bool active)
		{
			
		}

	}
	
}
