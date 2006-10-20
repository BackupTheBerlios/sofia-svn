
using System;
using Gtk;
using Glade;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	public class ViewGui : BaseView
	{		
		[Glade.WidgetAttribute] Gtk.TreeView treeviewRecent;
		[Glade.WidgetAttribute] Gtk.Image imageSearch;
		[Glade.WidgetAttribute] Gtk.Image imageHistory;
		[Glade.WidgetAttribute] Gtk.Image imageFavorites;
		[Glade.WidgetAttribute] Gtk.Image imageTrash;
		[Glade.WidgetAttribute] Gtk.ToolButton toolbuttonApplySearch;
		
		public ViewGui () : base("QuickView.gui.glade", "ViewGui", "QuickView")
		{
		
			#region Création dynamique de controles
			
			if (imageSearch != null)
				imageSearch.FromFile = "stock-find-and-replace.png";
			
			if (imageHistory != null)
				imageHistory.FromFile = "stock-loading-icon.png";		
			
			if (imageFavorites != null)
				imageFavorites.FromFile = "stock-bookmark.png";	
			
			if (imageTrash != null)
				imageTrash.FromFile = "stock-trash-empty.png";	
			
			Gtk.Widget applySearch = toolbuttonApplySearch.IconWidget;
			if (applySearch != null)
				((Gtk.Image) applySearch).FromFile = "stock-apply.png";
			
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
