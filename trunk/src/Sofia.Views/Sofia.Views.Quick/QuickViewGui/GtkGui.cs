
using System;
using System.Collections;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	public class ViewGui : BaseView, IViewGui
	{		
		[Glade.WidgetAttribute] Gtk.TreeView treeviewDoc;
		[Glade.WidgetAttribute] Gtk.ToolButton btnApply;
		[Glade.WidgetAttribute] Gtk.ToggleToolButton btnRecent;
		[Glade.WidgetAttribute] Gtk.ToggleToolButton btnFavorites;
		[Glade.WidgetAttribute] Gtk.ToggleToolButton btnTrash;
		
		ArrayList toggled;
		
		public ViewGui () : base("gui.glade", "ViewGui", "QuickView")
		{
		
			#region Création dynamique de controles
			
			
			#endregion
			
			toggled = new ArrayList();
			toggled.Add(btnRecent);
			toggled.Add(btnFavorites);
			toggled.Add(btnTrash);
		}
		
		#region Surcharge de BaseView
		
		public override string Caption 
		{
			get { return "Demarrage rapide"; } 
		}
		
		public override string Destination 
		{
			get { return "left"; } 
		}
		
		#endregion
		
		#region Implémentation de l'interface IViewGui
		
		public Gtk.TreeView TreeViewDoc 
		{
			get { return treeviewDoc; }
		}
		
		public Gtk.ToggleToolButton BtnRecent
		{
			get { return btnRecent; }
		}		

		public Gtk.ToggleToolButton BtnFavorites
		{
			get { return btnFavorites; }
		}
		
		public Gtk.ToggleToolButton BtnTrash
		{
			get { return btnTrash; }
		}		

		#endregion
						
	}
	
	
}
