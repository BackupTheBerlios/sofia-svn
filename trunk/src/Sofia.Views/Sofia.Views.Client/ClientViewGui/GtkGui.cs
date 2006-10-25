using System;

using Sofia.Core;
using Sofia.Core.GladeTools;

namespace Sofia.Views.ClientView
{
	
	public class ViewGui : BaseView
	{
	
		[Glade.WidgetAttribute] Gtk.Entry field_Nom;
		[Glade.WidgetAttribute] Gtk.Entry field_Prenom;
	
		public ViewGui () : base ("gui.glade", "ViewGui", "ClientView")
		{
		}
		
    	public override string Caption { 
    		get 
    		{    		
    			if ( (field_Nom.Text.Length == 0) || (field_Prenom.Text.Length == 0) )
    				return "Nouveau client";
    			else
    				return "Fiche d'identité de " + field_Nom.Text + " " + field_Prenom.Text; 
    		}
    	}
    	
    	public override string ToString() {
    		return "";
    	}
    	
    	public override string Destination {
			get { return "default"; } 
		}
    	
    	public override string SaveToXML() 
    	{
    		
 		   	GladeTransform gladeTransform = new GladeTransform(XMLGlade);
   			return gladeTransform.ToString();
   		}

	}
	
}
