
using System;
using System.Collections;

using Sofia.Core.Model;

namespace Sofia.Core

{
	
	public class TreeViewBuilder
	{
		
		internal const int TextColumn = 0;
		internal const int OpenIconColumn = 1;
		internal const int ClosedIconColumn = 2;
		internal const int DataItemColumn = 3;
		internal const int FilledColumn = 4;
		
		private Gtk.TreeView _TreeView;
		private Gtk.TreeStore _TreeStore;
		
		internal Gtk.TreeViewColumn _CompleteColumn;
		internal Gtk.CellRendererText _TextRender;
		
		Hashtable _Icons;
				
		
		public TreeViewBuilder (Gtk.TreeView treeView)
		{
			_Icons = new Hashtable ();
			_TreeView = treeView;	
			_TreeStore = new Gtk.TreeStore (typeof (string), typeof (Gdk.Pixbuf), typeof (Gdk.Pixbuf), typeof (object), typeof(bool));
			
			_TreeView.HeadersVisible = false;
			_TreeView.SearchColumn = 0;
			_TreeView.EnableSearch = true;
			
			_CompleteColumn = new Gtk.TreeViewColumn ();
			_CompleteColumn.Title = "column";

			Gtk.CellRendererPixbuf pixRender = new Gtk.CellRendererPixbuf ();
			_CompleteColumn.PackStart (pix_render, false);
			_CompleteColumn.AddAttribute (pix_render, "pixbuf", OpenIconColumn);
			_CompleteColumn.AddAttribute (pix_render, "pixbuf-expander-open", OpenIconColumn);
			_CompleteColumn.AddAttribute (pix_render, "pixbuf-expander-closed", ClosedIconColumn);

			_TextRender = new Gtk.CellRendererText ();
				
			_CompleteColumn.PackStart (_TextRender, true);
			_CompleteColumn.AddAttribute (_TextRender, "markup", TextColumn);
	
			_TreeView.AppendColumn (_CompleteColumn);
					
			SetDataFunctions();
			
			treeView.Model = treeStore;
 		}
		

		public Gdk.Pixbuf GetIcon (string id)
		{
			Gdk.Pixbuf icon = _Icons [id] as Gdk.Pixbuf;
			if (icon == null) {
				icon = _TreeView.RenderIcon (id, Gtk.IconSize.Menu, "");
				_Icons [id] = icon;
			}
			return icon;
		}
		
		private void SetDataFunctions()
		{
			objectColumn.SetCellDataFunc (objectRenderer, new Gtk.TreeCellDataFunc (RenderObject));
		}
		
		public void Fill(IList dossiers)
		{			
			Gdk.Pixbuf folderImage = Gdk.Pixbuf.LoadFromResource("stock-folder.png");
			Gdk.Pixbuf documentImage = Gdk.Pixbuf.LoadFromResource("stock-generic.png");
			
			foreach (Dossier dossier in dossiers) 
			{				
				Gtk.TreeIter iter = treeStore.AppendValues(folderImage, dossier);
				
				foreach (Document document in dossier.Documents) 
				{
					//TODO : Déterminer l'icone en fonction du document					
					treeStore.AppendValues(iter, documentImage, document);
					Console.WriteLine("////////////////////////////////////// caption du doc : " + document.Caption);
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
