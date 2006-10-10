
using System;
using Sofia.Core;

namespace Sofia.Views.WelcomeView
{
	
	/// <summary>
	/// Classe receiver 
	/// </summary>
	public class GuiActions : BaseGuiActions
	{
		ViewGui viewGui;
		
		public override IView View { get { return (IView)viewGui; } }
		
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
