
using System;
using System.Collections;

using Sofia.Core;

using com.db4o;

namespace Sofia.Core
{
	
	public class BaseController : IController
	{
		//Gestionnaire de commandes	
		CommandManager commandManager;
		
		//Modèle
		ModelFactory modelFactory;

		//Vues enregistrées auprès de controleur
		Hashtable registeredControllers;
		
		public BaseController() {				  	
		  	registeredControllers = new Hashtable();
		  	commandManager = new CommandManager();		  	
		  	modelFactory = new ModelFactory("Model");
		}

		#region implémentation de l'interface
		
		public virtual CommandManager CommandManager {
			get { return commandManager; } 
		}
		
		public ObjectSet RequestModel(string request)
		{
			ObjectSet objectSet = modelFactory.SendRequest(request);
			NotifyRequest(request);
			
			return objectSet;			
		}
		
		public virtual IView View {
			get { throw new NotSupportedException(); }
		}
		
		public virtual Hashtable RegisteredControllers {
			get { return registeredControllers; }
		}
		
		public virtual void ExecuteCommand(string ident, string parameters)	
		{
			if (CommandManager  == null)			
				throw new InvalidOperationException ("Pas de gestionnaire de commande affecté à la commande : " + ident);
			CommandManager.GetCommand(ident).Execute(parameters);
		}
		
		///<summary>
		///Chargement d'un controleur depuis une assembly
		///</summary>
		public virtual IController LoadController(string ident) 
		{
			throw new NotSupportedException();
		}
		
		///<summary>
		///Enregistre une vue
		///</summary>
		public void RegisterController(string ident, IController controller) 
		{
			registeredControllers.Add(ident, controller);
		}
		
		//<summary>
		///Permet de notifier une requête effectuée sur le modèle à l'ensemble des vues enregistrées
		///</summary>
		public virtual void NotifyRequest(string request)
		{
			IDictionaryEnumerator enumerator = registeredControllers.GetEnumerator(); 
  			while (enumerator.MoveNext()) {
  				IController controller = enumerator.Value as IController;
  				string key = enumerator.Key.ToString();
				if (controller == null)
					throw new ArgumentNullException ("Impossible de notifier une requête : controleur nul pour l'instance de la vue " + key);
				else
					controller.NotifyRequest(request);
    		}
		}
					
		#endregion
	}
	
}
