
using System;
using Sofia.Core;
using Sofia.Core.Model;

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
		}
		
		public void ShowDocuments()
		{			
			TreeViewBuilder treeViewBuilder = new TreeViewBuilder(viewGui.TreeViewRecent);
			
		}

	}
	
}
