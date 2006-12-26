
using System;
using System.IO;
using Gtk;
using Glade;
using Assembly = System.Reflection.Assembly;

using Sofia.Core;

namespace Sofia.Views.MainView
{

	public class ViewGui : Gtk.Window
	{
		#region Glade Widgets
		
    	[WidgetAttribute] VBox vboxMain;
		[WidgetAttribute] HPaned hpanedMain;
		[WidgetAttribute] Toolbar toolbarMain;
		/*[WidgetAttribute]*/  Notebook notebookViews;
		
		#endregion
		
		Glade.XML glade;

		public ViewGui () : base ("")
		{
			string fullName = "MainView.gui.glade";
			
			Assembly a = Assembly.GetAssembly(this.GetType());			
			Console.WriteLine(a.CodeBase);
			
			if (!System.IO.File.Exists(a.CodeBase + "/" + fullName))
				fullName = "gui.glade";
			
			glade = new XML (Assembly.GetCallingAssembly (), fullName, "ViewGui", null);
			glade.Autoconnect (this);
			
			Window win = (Window) glade ["ViewGui"];
			win.Maximize();
			
			//Initialisation du menu principal
			MainUIManager uim = new MainUIManager (this);
			
			vboxMain.PackStart (uim.MenuBar, false, false, 0);
			((Gtk.Box.BoxChild)(vboxMain[uim.MenuBar])).Position = 0;
			
			//Pagecontrol des vues
			notebookViews = new Notebook ();
			hpanedMain.Pack2 (notebookViews, true, false);
			notebookViews.Show ();
			//notebookViews.RemovePage(0);

		}
		
		public void OnItemExitActivate (object o, EventArgs args)
        {
               Application.Quit ();
        }
		
		protected virtual void OnDeleteEvent (object o, Gtk.DeleteEventArgs args)
		{
			Application.Quit ();
 			args.RetVal = true;
		}

	
		IController controller;

  		public IController Controller 
  		{ 
  			get { return controller; }
  			set { controller = value; }
  		}
		
		public void Initialize () 
		{
			//Vues affichées par défaut à l'ouverture de l'application
			controller.ExecuteCommand("New_QuickView", "");
		}
                                     	
    	#region Accesseurs pour les controles
    	
    	public Gtk.Notebook NotebookViews { 
    		get { return notebookViews; } 
    	}
    	
    	public Gtk.VBox VboxMain { 
    		get { return vboxMain; } 
    	}
    	
    	public Gtk.HPaned HpanedMain { 
    		get { return hpanedMain; } 
    	}
    	
    	public Gtk.Toolbar ToolbarMain { 
    		get { return toolbarMain; } 
    	}
    	
    	#endregion

	}
	
}
