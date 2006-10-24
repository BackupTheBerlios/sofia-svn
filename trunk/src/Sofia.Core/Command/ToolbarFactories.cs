
using System;
using Gtk;

namespace Sofia.Core
{
	
	/// <summary>
	/// Classe de base
	/// </summary>
	public class ToolbarItem : ToolItem
	{
		private ICommand cmd;
		
		public ToolbarItem(ICommand cmd) : base ()
		{
			this.cmd = cmd;
		}
		
		public ICommand Command {
			get { return cmd; }
		}
	}
	
	/// <summary>
	/// Séparateur
	/// </summary>
	public class ToolbarSeparator : ToolbarItem
	{
		private SeparatorToolItem separator;
		
		public ToolbarSeparator() : base (null)
		{
			separator = new SeparatorToolItem();
			this.Add(separator);
			separator.Show();
			this.ShowAll();
		}
	}
	
	/// <summary>
	/// Bouton
	/// </summary>
	public class ToolbarButton : ToolbarItem
	{
		
		private ToolButton button;
		
		public ToolbarButton(ICommand cmd) : base (cmd)
		{
			button = new ToolButton(cmd.Properties.Icon);
			button.Clicked += new EventHandler (OnClicked);
			button.Label = cmd.Properties.Text;
			this.Add(button);
			button.Show();
			this.ShowAll();
		}
		
		protected void OnClicked (object sender, EventArgs a)
		{
			if (Clicked != null)
				Clicked (this, EventArgs.Empty);
			else
				Command.Execute("");
		}
		
		public event EventHandler Clicked;
		
	}
	
	/// <summary>
	/// Saisie
	/// </summary>
	public class ToolbarEntry : ToolbarItem
	{
		private Entry entry;

		public ToolbarEntry (ICommand cmd) : base (cmd)
		{
			entry = new Entry ();
			entry.Activated += new EventHandler (OnActivated);
			entry.Text = cmd.Properties.Text;
			this.Add (entry);
			entry.Show ();
			this.ShowAll ();
		}

		protected void OnActivated (object sender, EventArgs a)
		{
			if (Activated != null)
				Activated (this, EventArgs.Empty);
			else
				Command.Execute("");
		}
		
		public event EventHandler Activated;
	}
	
	/// <summary>
	/// Bouton 2 états
	/// </summary>
	public class ToolbarToggle : ToolbarItem
	{
		
		private ToggleToolButton button;
		
		public ToolbarToggle(ICommand cmd) : base (cmd)
		{
			button = new ToggleToolButton(cmd.Properties.Icon);
			button.Toggled += new EventHandler (OnToggled);
			button.Label = cmd.Properties.Text;
			this.Add(button);
			button.Show();
			this.ShowAll();
		}
		
		protected void OnToggled (object sender, EventArgs a)
		{
			if (Toggled != null)
				Toggled (this, EventArgs.Empty);
			else
				Command.Execute("");
		}
		
		public event EventHandler Toggled;
		
	}
	
}
