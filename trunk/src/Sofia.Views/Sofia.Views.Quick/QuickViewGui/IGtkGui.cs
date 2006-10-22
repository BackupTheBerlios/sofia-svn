
using System;

namespace Sofia.Views.QuickView
{
	
	public interface IViewGui
	{
		Gtk.TreeView TreeViewDoc { get; }
		Gtk.ToggleToolButton BtnRecent { get; }
		Gtk.ToggleToolButton BtnFavorites { get; }
		Gtk.ToggleToolButton BtnTrash { get; }
		
		void OnFilterButtonToggled (object obj, EventArgs args);
	}
	
}
