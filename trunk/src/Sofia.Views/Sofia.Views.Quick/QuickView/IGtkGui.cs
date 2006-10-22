
using System;

namespace Sofia.Views.QuickView
{
	
	public interface IGtkGui
	{
		Gtk.TreeView TreeViewDoc { get; }
		Gtk.ToggleToolButton BtnRecent { get; }
		Gtk.ToggleToolButton BtnFavorites { get; }
		Gtk.ToggleToolButton BtnTrash { get; }
	}
	
}
