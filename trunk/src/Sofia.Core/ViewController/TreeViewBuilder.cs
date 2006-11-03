
using System;
using System.Collections;

using Sofia.Core.Model;

namespace Sofia.Core

{
	
	public class TreeViewBuilder
	{
		
		private Gtk.TreeView _TreeView;
		private Gtk.TreeStore _TreeStore; 
				
		Gtk.TreeViewColumn _ObjectColumn;
		
		Gtk.CellRendererText _ObjectRenderer;
		
		public TreeViewBuilder (Gtk.TreeView treeView)
		{
			_TreeView = treeView;	
			_TreeStore = new Gtk.TreeStore (typeof (Gdk.Pixbuf),	typeof (Dossier));
			
			SetRenderers();
			SetColumns();

			SetDataFunctions();
			
			_TreeView.Model = _TreeStore;
 		}
		
		private void SetRenderers()
		{
			_ObjectRenderer = new Gtk.CellRendererText ();
		}
		
		private void SetColumns()
		{
		    _TreeView.AppendColumn ("Icon", new Gtk.CellRendererPixbuf (), "pixbuf", 0);  
			_ObjectColumn =  _TreeView.AppendColumn ("Document", _ObjectRenderer, "text", 1);			
		}
		
		private void SetDataFunctions()
		{
			_ObjectColumn.SetCellDataFunc (_ObjectRenderer, new Gtk.TreeCellDataFunc (RenderObject));
		}
		
		public void Fill(IList dossiers)
		{
			//Gtk.Image folderImage = new Gtk.Image(Gtk.Stock.Directory, Gtk.IconSize.Menu);
			Gdk.Pixbuf folderImage = Gdk.Pixbuf.LoadFromResource("stock-folder.png");
			Gdk.Pixbuf documentImage = Gdk.Pixbuf.LoadFromResource("stock-generic.png");
			
			foreach (Dossier dossier in dossiers) 
			{				
				Gtk.TreeIter iter = _TreeStore.AppendValues(folderImage, dossier);
				
				foreach (Document document in dossier.Documents) 
				{
					//TODO : Déterminer l'icone en fonction du document					
					_TreeStore.AppendValues(iter, documentImage, document);
				}
			}
		}
		
		private void RenderObject (Gtk.TreeViewColumn column, Gtk.CellRenderer cell, Gtk.TreeModel model, Gtk.TreeIter iter)
		{
			object obj = model.GetValue (iter, 1);
			Type dossierType = typeof (Dossier);
			Type documentType = typeof (Document);
			
			if ( dossierType.IsInstanceOfType(obj) ) {
				Dossier dossier = (Dossier) obj;
 				(cell as Gtk.CellRendererText).Text = dossier.Caption;
 			} else 
 			    if ( documentType.IsInstanceOfType(obj) ) {
 					Document document = (Document) obj;
 					(cell as Gtk.CellRendererText).Text = document.Caption;
 				}
		}		
		
	}
	
}
