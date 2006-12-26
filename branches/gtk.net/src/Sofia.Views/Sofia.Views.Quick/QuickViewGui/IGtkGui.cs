
using System;

namespace Sofia.Views.QuickView
{
	
	public interface IViewGui
	{
		Gtk.TreeView TreeViewDoc { get; }
		Gtk.Entry EntrySearch { get; }

	}
	
}
