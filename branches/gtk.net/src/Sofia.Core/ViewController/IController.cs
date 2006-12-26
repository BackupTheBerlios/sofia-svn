
using System;
using System.Collections;

namespace Sofia.Core
{
	
	public interface IController
    {
      	//Gestionnaire de commandes
      	CommandManager CommandManager { get; }
      	
      	//Vue
      	IView View { get; }
      	
      	//Controleurs enregistrés auprès de ce controleur
      	Hashtable RegisteredControllers { get; }
      	
      	//Execute une commande
      	void ExecuteCommand(string ident, string parameters);
      	
      	//Envoie une requête au modèle
      	IList RequestModel(string request);
      	
      	//Charge le controleur depuis l'assembly ident.dll
      	IController LoadController(string ident);
      	
      	//Enregistre un controller auprès de ce controller
      	void RegisterController(string ident, IController controller);
      	
      	//Mise à jour des données de la vue associée au controlleur
      	void NotifyRequest(string request);
      	
    }
	
}
