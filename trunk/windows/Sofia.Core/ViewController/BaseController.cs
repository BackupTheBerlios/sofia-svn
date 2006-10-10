
using System;
using System.Collections;

namespace Sofia.Core
{
	
	public class BaseController : IController
	{
	
		//Vues enregistrées auprès de controleur
		Hashtable registeredControllers;
		
		public BaseController() {				  	
		  	registeredControllers = new Hashtable();
		}

		#region implémentation de l'interface
		
		public virtual ICommandReceiver CommandReceiver
		{
			get { return null; }
		}
		
		public virtual CommandManager CommandManager 
		{
			get { return null; } 
		}
		
		public virtual Hashtable RegisteredControllers
		{
			get { return registeredControllers; }
		}
		
		public virtual void ExecuteCommand(string ident, string parameters)	{
			if (CommandManager  == null)
				throw new InvalidOperationException ("Pas de gestionnaire de commande affecté à la commande : " + ident);
			CommandManager.GetCommand(ident).Execute(parameters);
		}
		
		///<summary>
		///Chargement d'un controleur depuis une assembly
		///</summary>
		public virtual IController LoadController(string ident) {
			return null;
		}
		
		///<summary>
		///Enregistre une vue dans la liste des vues
		///</summary>
		public void RegisterController(string ident, IController controller) {
			registeredControllers.Add(ident, controller);
		}
					
		#endregion
	}
	
}
