
using System;
using Gtk;

namespace Sofia.Views.MainView
{
	
	public class MainUIManager : Gtk.UIManager {

		private ViewGui viewGui;
		
		private Gtk.ActionGroup actions;

		public MainUIManager (ViewGui viewGui)
		{
			this.viewGui = viewGui;
			
			actions = new ActionGroup ("Actions");

			ActionEntry actionEntryQuit;
			actionEntryQuit = new ActionEntry("Quitter", Gtk.Stock.Quit,  null, "<control>Q", "Quitter l'environnement Sofia",   Quit);			

			ActionEntry[] entries = new ActionEntry [] {
				new ActionEntry ("Fichier", null,	"_Fichier", null, null, null),
				new ActionEntry ("Editer", null,	"Editio_n", null, null, null),
				new ActionEntry ("Rechercher", null,	"Recher_cher", null, null, null),
				new ActionEntry ("Aide", null, "_Aide",  null, null, null),
				actionEntryQuit,
				new ActionEntry ("Préférences", Gtk.Stock.Preferences, null, null, "Préférences",  Preferences),
				new ActionEntry ("RubriquesAide", Gtk.Stock.Help,  "Rubriques d'_aide",	 "F1", "Aide - Rubriques d'aide", Help),
				new ActionEntry ("APropos", Gtk.Stock.About, null, null, "A propos de l'environnement Sofia", About)
			};
			actions.Add (entries);

			InsertActionGroup(actions, 0);
			this.viewGui.AddAccelGroup(AccelGroup);
			uint id = 0;
			try {
				id = AddUiFromFile("MainUIDef.xml");
			}
			catch {
				RemoveUi(id);
			}
		}

		public Gtk.MenuBar MenuBar {
			get {
				return (Gtk.MenuBar)GetWidget ("/MenuBar");
			}
		}		

		private void Preferences (object obj, EventArgs args)
		{
			Console.WriteLine("Afficher les préférences");
		}

		private void Quit (object obj, EventArgs args)
		{
			Gtk.Application.Quit ();
		}

		private void Help (object obj, EventArgs args)
		{
			//Gnome.Url.Show ("http://www.sophrologik.fr");
			Console.WriteLine("Aide...");
		}

		private void About (object obj, EventArgs args)
		{
			//Gdk.Pixbuf logo = Beagle.Images.GetPixbuf ("system-search.png");
			/*
			string[] people = new string[] { "Lawrence-Albert Zémour<neurosoup@gmail.com>",
							 "Anne-Angélique Meuleman <sophrologik@gmail.com>" };
			
#pragma warning disable 612 // don't warn that Gnome.About is deprecated
			Gnome.About about = new Gnome.About ("Sofia",
							     "0.1",
							     "Copyright 2006-2007 Lawrence-Albert Zémour",
							     null, people, null, null,
							     logo);
			about.Run ();
			about.Dispose ();
#pragma warning restore 612
			*/
			Console.WriteLine("About...");
		}
		
	}
	
}
