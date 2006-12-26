

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
		CommandReceiver commandReceiver;
		
  		ArrayList views;
		
		public Controller()	{
			//Initisalisation de l'application
			Application.Init();			
			
		  	//Création du receiver des commandes
		  	commandReceiver = new CommandReceiver();
		  	 		  	
		  	//Création des commandes
		  	CommandManager.RegisterCommand(new NewCommand(commandReceiver, this, "New", "", "", "", ""));
		  	CommandManager.RegisterCommand(new SaveCurrentViewCommand(commandReceiver, "SaveCurrentView", "Sauver",  Gtk.Stock.Save, "", "Enregistrer les modifications apportées dans le document actif."));

		  	RegisterNewViewCommands();
		  	
		  	//Affichage de l'interface graphique principale
		  	ExecuteCommand("New", "");
		  	
		  	//Création des bouton d'instanciation de vues
		  	CreateToolButtons();
		  	
		  	//Boucle principale de l'application
		  	Application.Run ();
		}		
		
		/// <summary>
		/// Implémentationde l'interface 
		/// </summary>
		public override IView View {
			get { return commandReceiver.View; }
		}

		/// <summary>
		/// Chargement de l'assembly d'une vue 
		/// </summary>
		public override IController LoadController(string ident)
    	{   	
      		AssemblyName assemblyName = new AssemblyName();

      		assemblyName.Name = ident + "Controller";
      		assemblyName.CodeBase = String.Concat("file:///", Directory.GetCurrentDirectory());

			Console.WriteLine("Chargement de " + assemblyName.CodeBase + "/" + assemblyName.FullName + ".dll...");
      		Assembly assembly = Assembly.Load(assemblyName);
      		
      		IController controller = (IController) assembly.CreateInstance("Sofia.Views." + ident + ".Controller");
      		
      		if (controller == null) 
	   			throw new ArgumentNullException(string.Format("L'assembly {0} n'a pas défini de controleur.",  assemblyName.FullName));
	   		else
     			return controller;      		
    	}
    	
		/// <summary>
		/// Enregistrement d'une vue
		/// </summary>
    	public void RegisterNewViewCommand(string viewName)
    	{
    		//Récupération du controleur de la vue
	    	IController controller = LoadController(viewName);
       		if (controller == null) 
	   			throw new ArgumentNullException("L'assembly n'a pas défini de controleur.");
	   			
         	//Récupère les propriété de la commande New dans la vue	
	       	string cmdId = string.Format("New_{0}", viewName);
	       	CommandProperties commandProperties = controller.CommandManager.GetProperties("New");
       		if (commandProperties == null) 
	   			throw new ArgumentNullException(string.Format("La commande {0} n'existe pas dans la vue {1}",  cmdId, viewName));

			//Enregistre la commande d'insertion de l'interface graphique dans la vue principale
	       	ICommand command = new NewViewCommand(commandReceiver, viewName, cmdId, commandProperties.Text, commandProperties.Icon, commandProperties.AccelKey, commandProperties.Description);	       	
	       	CommandManager.RegisterCommand(command);
	       	
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
  			while (enumerator.MoveNext()) {
    			RegisterNewViewCommand(enumerator.Current.ToString());
    		}
    		
	  	}
	  	
	   	///<summary>
	   	///Ajoute les boutons associés aux vues
	   	///</smmary>
	   	public void CreateToolButtons()
	   	{	
	   		ICommand command;
	   		ToolbarButton button;
	   		
	   		//Ajout des boutons de création des vues
	   		foreach (string ident in views) {
	   			string cmdId = string.Format("New_{0}",  ident);	
	   			command = CommandManager.GetCommand(cmdId);	   			
	   			if (command != null) {	   			   			
		       		button = new ToolbarButton(command);		       				
		       		commandReceiver.AddToolItem(button);
		       	}		   	
		     }

			//Séparateur
			commandReceiver.AddToolItem(new ToolbarSeparator());
		     
		     //Ajout du bouton de sauvegarde de la vue
	        command = CommandManager.GetCommand("SaveCurrentView");
       		button = new ToolbarButton(command);
       		button.Clicked += new EventHandler(OnSaveCurrentViewClicked);
       		commandReceiver.AddToolItem(button);
	     
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
