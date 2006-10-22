
using System;
using Sofia.Core;

namespace Sofia.Views.ClientView
{
	
	/// <summary>
	/// Classe receiver 
	/// </summary>
	public class CommandReceiver : BaseCommandReceiver
	{
		ViewGui viewGui;
		
		public override IView View { 
			get {	return (IView) viewGui; }
		}
		
		/// <summary>
		/// Création de l'interface graphique 
		/// </summary>
		public void CreateGui(IController controller)
		{			
			viewGui = new ViewGui();
		  	viewGui.Controller = controller;
		  	viewGui.Initialize();
		}

	}
	
}
