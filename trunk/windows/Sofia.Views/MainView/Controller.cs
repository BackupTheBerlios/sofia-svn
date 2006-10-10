

using System;
using Gtk;
using System.Reflection;
using System.IO;
using System.Collections;
using Sofia.Core;
using Sofia.Core.XmlTools;

namespace Sofia.Views.MainView
{



	/// <summary>
	/// Controleur de la vue principale
	/// </summary>	
	public class Controller : Core.BaseController
	{
		CommandReceiver cmdRcv;
		CommandManager cmdMgr;
		
  		ArrayList views;
		
		public Controller()	{
			//Initisalisation de l'application
			Application.Init();			
			
		  	//Création du gestionnaire de commande
		  	cmdMgr = new CommandManager();
		  	
		  	//Création du receiver des commandes
		  	cmdRcv = new CommandReceiver();
		  	 		  	
		  	//Création des commandes
		  	cmdMgr.RegisterCommand(new NewCommand(cmdRcv, this, "New", "", "", "", ""));
		  	cmdMgr.RegisterCommand(new SaveCurrentViewCommand(cmdRcv, "SaveCurrentView", "Sauver",  Gtk.Stock.Save, "", "Enregistrer les modifications apportées dans le document actif."));

		  	RegisterNewViewCommands();
		  	
		  	//Affichage de l'interface graphique principale
		  	ExecuteCommand("New", "");
		  	
		  	//Création des bouton d'instanciation de vues
		  	CreateToolButtons();

		  	//Boucle principale de l'application
		  	Application.Run ();
		}		

		/// <summary>
		/// Retourne les actions de l'interface graphique de la vue
		/// </summary>
    	public override ICommandReceiver CommandReceiver {
			get { return (ICommandReceiver)cmdRcv; }
		}
		
		/// <summary>
		/// Retourne le gestionnaire d'actions de la vue
		/// </summary>
		public override CommandManager CommandManager { 
			get { return cmdMgr; } 
		}
    	
		/// <summary>
		/// Chargement de l'assembly d'une vue 
		/// </summary>
		public override IController LoadController(string ident)
    	{   	
      		AssemblyName assemblyName = new AssemblyName();

      		assemblyName.Name = ident;
      		assemblyName.CodeBase = String.Concat("file:///", Directory.GetCurrentDirectory());

			Console.WriteLine("Chargement de " + assemblyName.CodeBase + "/" + assemblyName.FullName + ".dll...");
      		Assembly a = Assembly.Load(assemblyName);
      	
      		
      		//object o = a.CreateInstance("Sofia.Views." + ident + ".Controller");
      		//return (o as IController);      
      		return null;
    	}
    	
		/// <summary>
		/// Enregistrement d'une vue
		/// </summary>
    	public void RegisterNewViewCommand(string viewName)
    	{
    		//Récupération du controleur de la vue
	    	IController newController = LoadController(viewName);
       		if (newController == null) 
	   			throw new InvalidOperationException("L'assembly n'a pas défini de controleur.");
       		
         	//Récupère les propriété de la commande New dans la vue	
	       	string cmdId = string.Format("New_{0}", viewName);
	       	CommandProperties cmdProp = newController.CommandManager.GetProperties("New");
       		if (cmdProp == null) 
	   			throw new InvalidOperationException(string.Format("La commande {0} n'existe pas dans la vue {1}",  cmdId, viewName));

			//Enregistre la commande d'insertion de l'interface graphique dans la vue principale
	       	ICommand cmd = new NewViewCommand(cmdRcv, viewName, cmdId, cmdProp.Text, cmdProp.Icon, cmdProp.AccelKey, cmdProp.Description);
	       	cmdMgr.RegisterCommand(cmd);
	       	
	   	}
	   	
    	///<summary>
    	///Lecture du fichier de configuration des vues
    	///</summary>
    	public void RegisterNewViewCommands()
    	{
    		string configDir = Directory.GetCurrentDirectory() + "/";
    		
    		//Chargement du fichier XML
     		XPathNavigatorFacade xpn= new XPathNavigatorFacade();
     		xpn.LoadFromFile(configDir + "sofia.config");
     		
     		//Récupération de la configuration active
     		ArrayList activeConf = xpn.GetAttributes("/Configurations", "active");     		
     		if (activeConf.Count == 0)
     			throw new ArgumentException ("Pas de configuration active définie. Veuillez vérifier le fichier de configuration.");
     			
     		//Récupération des vues de la configuration active	
   			string xPath = String.Format("//Configuration[@name='{0}']/Views//View", activeConf[0]);
   			views = xpn.GetAttributes(xPath, "name");

     		if (views.Count == 0)
     			throw new ArgumentException ("Aucune vue définie. Veuillez vérifier le fichier de configuration.");
   			
   			//Enregistrement des commandes d'ouverture
  			IEnumerator enumerator = views.GetEnumerator(); 
  			//while (enumerator.MoveNext()) {
    			//RegisterNewViewCommand(enumerator.Current.ToString());
    		//}
    		
	  	}
	  	
	   	///<summary>
	   	///Ajoute les boutons associés aux vues
	   	///</smmary>
	   	public void CreateToolButtons()
	   	{	
	   		ICommand cmd;
	   		ToolbarButton button;
	   		
	   		//Ajout des boutons de création des vues
	   		foreach (string ident in views) {
	   			string cmdId = string.Format("New_{0}",  ident);	
	   			cmd = this.CommandManager.GetCommand(cmdId);	   			
	   			if (cmd != null) {	   			   			
		       		button = new ToolbarButton(cmd);		       				
		       		cmdRcv.AddToolItem(button);
		       	}		   	
		     }

			//Séparateur
			cmdRcv.AddToolItem(new SeparatorToolItem());
		     
		     //Ajout du bouton de sauvegarde de la vue
	        cmd = this.CommandManager.GetCommand("SaveCurrentView");
       		button = new ToolbarButton(cmd);
       		button.Clicked += new EventHandler(OnSaveCurrentViewClicked);
       		cmdRcv.AddToolItem(button);
	     
		 }

	   	///<summary>
	   	///
	   	///</smmary>
 		protected void OnSaveCurrentViewClicked (object sender, EventArgs a)
		{
			ToolbarButton button = (ToolbarButton) sender;
			button.Command.Execute("");
		}

		 
	}
	
}
