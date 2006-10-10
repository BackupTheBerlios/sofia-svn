
using System;

namespace MainView
{
	
	public class NewFolderDialog : Gtk.Dialog
	{
		
		public NewFolderDialog()
		{
			Stetic.Gui.Build(this, typeof(MainView.NewFolderDialog));
		}
	}
	
}
