
using System;
using System.Collections;
using Gtk;
using Glade;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	public class ViewGui : BaseView
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
		
		#region Implémentation de l'interface
		
		public override string Caption 
		{
			get { return "Demarrage rapide"; } 
		}
		
		public override string Destination 
		{
			get { return "left"; } 
		}
		
		#endregion
		
		#region Accesseurs controles
		
		public Gtk.TreeView TreeViewDoc 
		{
			get { return treeviewDoc; }
		}		

		#endregion
		
		private void OnToggled (object sender, EventArgs args)	
		{
			foreach (Gtk.ToggleToolButton btn in toggled) 
			{
				if (btn != sender)
					( (Gtk.ToggleToolButton) btn ).Active = false;
			}
		}
				
	}
	
	
}
