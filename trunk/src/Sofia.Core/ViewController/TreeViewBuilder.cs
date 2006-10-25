
using System;
using System.Collections;

using Sofia.Core.Model;

namespace Sofia.Core

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
		
		public void Fill(IList dossiers)
		{
			//Gtk.Image folderImage = new Gtk.Image(Gtk.Stock.Directory, Gtk.IconSize.Menu);
			Gdk.Pixbuf folderImage = Gdk.Pixbuf.LoadFromResource("stock-folder.png");
			Gdk.Pixbuf documentImage = Gdk.Pixbuf.LoadFromResource("stock-generic.png");
			
			foreach (Dossier dossier in dossiers) 
			{				
				Gtk.TreeIter iter = treeStore.AppendValues(folderImage, dossier);
				
				foreach (Document document in dossier.Documents) 
				{
					//TODO : Déterminer l'icone en fonction du document					
					treeStore.AppendValues(iter, documentImage, document);
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
