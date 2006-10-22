
using System;
using System.Collections;
using Sofia.Core;

namespace Sofia.Views.QuickView
{
	
	public class ViewGui : BaseView, IViewGui
	{		
		[Glade.WidgetAttribute] Gtk.TreeView treeviewDoc;
		[Glade.WidgetAttribute] Gtk.Toolbar toolbarFilters;
		
	
		public ViewGui () : base("gui.glade", "ViewGui", "QuickView")
		{
		
			#region Création dynamique de controles
			
			
			#endregion
			
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
		
		public override void AddToolbarCommand(ICommand command, bool toggled)
		{
			button = new ToolbarButton(command);		       				
		       		commandReceiver.AddToolItem(button);
			toolbarFilters.Insert(item, 0);
		}
		
		#endregion
		
		#region Implémentation de l'interface
		
		public Gtk.TreeView TreeViewDoc 
		{
			get { return treeviewDoc; }
		}
		
		///<summary>
		///Ajout d'un bouton dans la barre d'outils principale
		///</summary>
		public void AddFilterButton(ToolbarItem item)
		{
       		if (item == null) 
	   			throw new InvalidOperationException("Impossible d'ajouter un élément nul dans la barre d'outils des filtres");
 
			toolbarFilters.Insert(item, 0);
		}
		

		#endregion
						
	}
	
	
}
