
using System;

using com.db4o;

using Sofia.Core.Model;

namespace Model
{
	
	public class TreeViewBuilder
	{
		
		private Gtk.TreeView treeView;
		private Gtk.TreeStore treeStore; 
				
		Gtk.TreeViewColumn objectColumn;
		
		Gtk.CellRendererText objectRenderer;
		
		public TreeViewBuilder (Gtk.TreeView treeView)
		{
			this.treeView = treeView;	
			treeStore = new Gtk.TreeStore (typeof (Gdk.Pixbuf),	typeof (Dossier));
			
			SetRenderers();
			SetColumns();
			SetDataFunctions();
			
			treeView.Model = treeStore;
 		}
		
		private void SetRenderers()
		{
			objectRenderer = new Gtk.CellRendererText ();
		}
		
		private void SetColumns()
		{
			treeView.AppendColumn ("Icon", new Gtk.CellRendererPixbuf (), "pixbuf", 0);  
			objectColumn =  treeView.AppendColumn ("Document", objectRenderer, "text", 1);			
		}
		
		private void SetDataFunctions()
		{
			objectColumn.SetCellDataFunc (objectRenderer, new Gtk.TreeCellDataFunc (RenderObject));
		}
		
		public void Fill(ObjectSet dossiers)
		{
			Gdk.Pixbuf folderPixbuf = new Gdk.Pixbuf (Gtk.Stock.Directory);			
			foreach (Dossier dossier in dossiers) 
			{				
				Gtk.TreeIter iter = treeStore.AppendValues(folderPixbuf, dossier);
				
				foreach (Document document in dossier.Documents) 
				{
					//TODO : Déterminer l'icone en fonction du document
					Gdk.Pixbuf documentPixbuf = new Gdk.Pixbuf (Gtk.Stock.File);
					treeStore.AppendValues(iter, documentPixbuf, document);
				}
			}
		}
		
		private void RenderObject (Gtk.TreeViewColumn column, Gtk.CellRenderer cell, Gtk.TreeModel model, Gtk.TreeIter iter)
		{
			if (model.IterHasChild(iter)) {
				Dossier dossier = (Dossier) model.GetValue (iter, 1);
 				(cell as Gtk.CellRendererText).Text = dossier.Caption;
 			} else {
 				Document document = (Document) model.GetValue(iter, 1);
 				(cell as Gtk.CellRendererText).Text = document.Caption;
 			}
		}		
		
	}
	
}
