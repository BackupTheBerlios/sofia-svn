
using System;
using Gtk;
using Glade;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	public class ViewGui : BaseView
	{		
		[Glade.WidgetAttribute] Gtk.TreeView treeviewDoc;
		
		//[Glade.WidgetAttribute] Gtk.ToolButton toolbuttonApplySearch;
		
		public ViewGui () : base("gui.glade", "ViewGui", "QuickView")
		{
		
			#region Création dynamique de controles
			
			
			#endregion
		}
		
		public override string Caption { 
			get { return "Demarrage rapide"; } 
		}
		
		public override string Destination {
			get { return "left"; } 
		}
		
		#region Accesseurs controles
		
		public Gtk.TreeView TreeViewDoc {
			get { return treeviewDoc; }
		}		

		
		#endregion
				
	}
	
	
}
