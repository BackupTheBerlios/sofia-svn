
using System;
using System.Collections;

using Sofia.Core.Commands;

namespace Sofia.Core.Plugins
{
	
	public class BaseController : IController
	{
		//Gestionnaire de commandes	
		CommandManager _CommandManager;
		
		public BaseController() {
            _CommandManager = new CommandManager();  	
		}

		#region implémentation de l'interface
		
		public virtual CommandManager CommandManager {
			get { return _CommandManager; } 
		}	
		
		public virtual IView View {
			get { throw new NotSupportedException(); }
		}
		
		public virtual void ExecuteCommand(string ident, string parameters)	
		{
			if (_CommandManager  == null)			
				throw new InvalidOperationException ("Pas de gestionnaire de commande affecté à la commande : " + ident);
			_CommandManager.CommandByName(ident).Execute(parameters);
		}
		
	
		#endregion
	}
	
}
