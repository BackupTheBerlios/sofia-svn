
using System;
using Gtk;
using Glade;
using Sofia.Core;

namespace Sofia.Views.StartBarView
{
	
	public class ViewGui : BaseView
	{
		//[Glade.WidgetAttribute] Gtk.Expander expanderShortcuts;
		[Glade.WidgetAttribute] Gtk.Expander expanderRecentFolders;
		[Glade.WidgetAttribute] Gtk.VBox vboxMain;
		
		public ViewGui () : base("gui.glade", "ViewGui")
		{
		
			#region Création dynamique de controles
			
			//Création du conteneur des dossiers récents
			//expanderRecentFolders = new Expander("Dossiers récents");
			//vboxMain.PackStart(expanderRecentFolders, false, true, 0);
			//expanderRecentFolders.Show();
			//((Box.BoxChild)(vboxMain[expanderRecentFolders])).Position = 2;
			
			
			//Création du conteneur des raccourcis de vues
			//expanderShortcuts = new Expander("Actions courantes");
			//vboxMain.PackEnd(expanderShortcuts, false, true, 0);
			//expanderShortcuts.Show();
			//((Box.BoxChild)(vboxMain[expanderShortcuts])).Position = 3;
			
			#endregion
		}
		
		public override string Caption { 
			get { return "Demarrage rapide"; } 
		}
		
		public override string Destination {
			get { return "left"; } 
		}
		
		#region Accesseurs controles
		
		public Expander ExpanderRecentFolders {
			get { return expanderRecentFolders; }
		}		
		
		#endregion
				
	}
	
	
}
