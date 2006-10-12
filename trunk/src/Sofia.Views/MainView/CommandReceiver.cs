
using System;
using System.Xml;

using Sofia.Core;
using Sofia.Core.XmlTools;

namespace Sofia.Views.MainView
{
	
	/// <summary>
	/// Classe receiver 
	/// </summary>
	public class CommandReceiver : BaseCommandReceiver
	{
		ViewGui viewGui;
		
		/// <summary>
		/// Création de l'interface graphique 
		/// </summary>
		public void CreateGui(IController controller)
		{						
			viewGui = new ViewGui();
			viewGui.Controller = controller;
			viewGui.Initialize();
		}
		
		///<summary>
		///Ajout d'un bouton dans la barre d'outils principale
		///</summary>
		public void AddToolItem(Gtk.ToolItem item)
		{
       		if (item == null) 
	   			throw new InvalidOperationException("Impossible d'ajouter un élément nul dans la barre d'outils principale");
 
			viewGui.ToolbarMain.Insert(item, 0);
		}
		
		///<summary>
		///Récupération de controleur de la vue viewName
		///</summary>
		public IController GetController(string viewName) {
		   	IController controller = viewGui.Controller.LoadController(viewName);
       		if (controller == null) 
	   			throw new InvalidOperationException("L'assembly n'a pas défini de controleur.");
	   		return controller;
	   	}

		///<summary>
		///Insertion d'une vue dans une zone de la vue principale
		///</summary>
		public void NewView(string viewName)
		{
			string key = "none";
		
    		//Récupération du controleur de la vue
	    	IController controller = GetController(viewName);
	   			
	       	//Commande d'initialisation de l'interface graphique.
	       	//Permet d'instancier l'interface graphique de la vue à partir du controleur
	       	controller.ExecuteCommand("New", "");
	       			
         	//Récupération de l'interface graphique de la vue
       		IView view = controller.View;
       		if (view == null)
       			throw new InvalidOperationException("L'interface graphique de la vue n'a pas pu être créée.");
	       			
	       	//Debug
			Console.WriteLine("Appel commande NewView : " + view.Caption + " dans " + view.Destination);
			
			//Récupération du control de la vue
			Gtk.HBox hbox = view.VisualComponent;			
			if (hbox == null)
				throw new InvalidOperationException("La vue n'expose pas son interface graphique.");
			
			//positionnement dans le Notebook principal
	       	if (view.Destination.Equals("default")) {	       		
	       		viewGui.NotebookViews.AppendPage(hbox, new Gtk.Label(view.Caption));
	       		key = (viewGui.NotebookViews.NPages - 1).ToString();
	       		view.Initialize();
	       		hbox.ShowAll();
	       	}
	       		
	       	//positionnement latéral gauche
	       	if (view.Destination.Equals("left")) {
	       		key = "left";
	       		viewGui.HpanedMain.Pack1(hbox, true, false);
	       		hbox.ShowAll();
	       	}
	       	
	       	//Ajout dans la liste des controleurs
	        viewGui.Controller.RegisterController(key, controller);
	    }
		
	    ///<summary>
		///Sauvegarde du document de la vue
		///</summary>
		public void SaveCurrentView()
		{
    		//Récupération du controleur de la vue active
    		string key = viewGui.NotebookViews.CurrentPage.ToString();
    		IController controller = (IController) viewGui.Controller.RegisteredControllers[key];
    		
    		if (controller == null)
    			throw new InvalidOperationException(string.Format("Pas de controleur associé à la clé {0}", key));
    		
	    	string idDoc = controller.View.DocumentID;
	    	
	    	//Si l'identifiant de document est nul alors c'est un nouveau document, donc un nouveau dossier
	    	//dans le cas d'un document maitre
	    	if (idDoc.Length == 0) {
	    	}
	    	
			//Créer un nouveau dossier
			XmlElement eField;
			XmlDocumentFacade xmlDoc = new XmlDocumentFacade("<Requests/>");
			XmlElement eRequest = xmlDoc.AddNode(null, "Request", "");
   			xmlDoc.AddAttributeNode(eRequest, "operation", "Insert");
   			xmlDoc.AddAttributeNode(eRequest, "object", "MasterDocument");
   			
 			XmlElement eFields = xmlDoc.AddNode(eRequest, "Fields", "");
 			
 			eField = xmlDoc.AddNode(eFields, "Field", DateTime.Now.ToString());
 			xmlDoc.AddAttributeNode(eField, "name", "creation");
 			
 			eField = xmlDoc.AddNode(eFields, "Field", controller.View.Caption);
 			xmlDoc.AddAttributeNode(eField, "name", "caption");
 			
 			string docContent = controller.View.SaveToXML();
 			Console.WriteLine("Contenu du document à sauvegarder : " + docContent);
 			eField = xmlDoc.AddNode(eFields, "Field", docContent);
 			xmlDoc.AddAttributeNode(eField, "name", "content");

			Console.WriteLine(xmlDoc.ToString());
		
   			 viewGui.Controller.RequestModel(xmlDoc.ToString());
  		}
  		
  	    ///<summary>
		///Ouverture d'une vue
		///</summary>
		public void OpenView(string name, string documentid)
		{
  		}

  		
  	}
	
}
