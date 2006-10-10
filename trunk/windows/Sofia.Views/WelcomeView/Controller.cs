
using System;
using Sofia.Core;

namespace Sofia.Views.WelcomeView
{
	
	///<summary>
	///Controleur de la vue
	///</summary>
	public class Controller : BaseController
	{
		GuiActions guiActions;
		CommandManager cmdMgr;
	
		public Controller()
		{	
		  	//Création du gestionnaire de commande
		  	cmdMgr = new CommandManager();
		  	
		  	//Création du receiver des commandes
		  	guiActions = new GuiActions();
		  	
		  	//Création des commandes
		  	cmdMgr.RegisterCommand(new NewCommand(guiActions, this, "New", "", "", "", ""));
		}
		
		#region inherited members
		
		///<summary>
		/// Get GuiActions
		///</summary>
    	public override IGuiActions GuiActions
		{
			get { return (IGuiActions)guiActions; }
		}		
		
		public override CommandManager CommandManager	{	get { return cmdMgr; } }
		
		#endregion
		
		public static string TimeSinceEdited(int timestamp)
		{
			DateTime fldtime = (new DateTime (1970, 1, 1, 0, 0, 0, 0)).AddSeconds(timestamp);
			TimeSpan sincelast = DateTime.UtcNow - fldtime;

			if (sincelast.Days > 1)
			{
				return sincelast.Days + " jours";
			}
			else if (sincelast.Days == 1)
			{
				return "Un jour";
			}
			else if (sincelast.Hours > 1)
			{
				return sincelast.Hours + " heures";
			}
			else if (sincelast.Hours == 1)
			{
				return "Une heure";
			}
			else if (sincelast.Minutes > 0)
			{
				return sincelast.Minutes + " minutes";
			}
			else
			{
				return "Moins d'une minute";
			}
		}
	}
	
}
