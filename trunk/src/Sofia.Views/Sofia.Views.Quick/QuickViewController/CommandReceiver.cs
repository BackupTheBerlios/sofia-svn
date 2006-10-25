
using System;
using System.Collections;
using System.Xml;

using Sofia.Core;

using Sofia.Core.XmlTools;

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
			XmlElement eField;
			XmlDocumentFacade xmlDoc = new XmlDocumentFacade("<Requests/>");
			XmlElement eRequest = xmlDoc.AddNode(null, "Request", "");
   			xmlDoc.AddAttributeNode(eRequest, "operation", "Select");
   			xmlDoc.AddAttributeNode(eRequest, "object", "Document");
   			
 			XmlElement eFields = xmlDoc.AddNode(eRequest, "Fields", "");
 			
 			eField = xmlDoc.AddNode(eFields, "Field", "test test");
 			xmlDoc.AddAttributeNode(eField, "name", "caption");
 			
			Console.WriteLine(xmlDoc.ToString());
		
			IList dossiers = viewGui.Controller.RequestModel(xmlDoc.ToString());
			treeViewBuilder.Fill(dossiers);
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
