
using System;
using System.Collections;

namespace Sofia.Core
{
	
	public interface IController
    {
    	//Classe qui implémente des actions sur l'interface graphiques
      	ICommandReceiver CommandReceiver { get; }
      	
      	//Gestionnaire de commandes
      	CommandManager CommandManager { get; }
      	
      	//Controleurs enregistrés auprès de ce controleur
      	Hashtable RegisteredControllers { get; }
      	
      	//Execute une commande
      	void ExecuteCommand(string ident, string parameters);
      	
      	//Charge le controleur depuis l'assembly ident.dll
      	IController LoadController(string ident);
      	
      	//Enregistre un controller auprès de ce controller
      	void RegisterController(string ident, IController controller);
      	
    }
	
}
