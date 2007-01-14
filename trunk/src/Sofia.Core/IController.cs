
using System;
using System.Collections;

using Sofia.Commands;

namespace Sofia.Mvc
{
	
	public interface IController
    {
      	/// <summary>
      	/// Execute une commande
      	/// </summary>
      	/// <param name="ident">Identifiant de la commande</param>
      	/// <param name="parameters">Paramètres de la commande</param>
      	void ExecuteCommand(string ident, string parameters);

        IView Find(string contentId);
        IView Find(int index);
        void Add(IView view);
     	      	
    }
	
}
