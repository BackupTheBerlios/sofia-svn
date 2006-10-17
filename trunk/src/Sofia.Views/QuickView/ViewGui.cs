
using System;
using Gtk;
using Glade;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	public class ViewGui : BaseView
	{
		//[Glade.WidgetAttribute] Gtk.Expander expanderShortcuts;
		[Glade.WidgetAttribute] Gtk.TreeView treeviewRecent;
		
		public ViewGui () : base("QuickView.gui.glade", "ViewGui", "QuickView")
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
		
		public Gtk.TreeView TreeViewRecent {
			get { return treeviewRecent; }
		}		

		
		#endregion
				
	}
	
	
}
