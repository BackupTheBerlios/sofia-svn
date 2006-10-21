using System;
using System.Text;
using System.Xml;

using Glade;

using Sofia.Core;
using Sofia.Core.XmlTools;

namespace Sofia.Views.ClientView
{
	
	public class ViewGui : BaseView
	{
	
		[WidgetAttribute] Gtk.Entry field_Nom;
		[WidgetAttribute] Gtk.Entry field_Prenom;
	
		public ViewGui () : base ("gui.glade", "ViewGui", "ClientView")
		{
		}
		
    	public override string Caption { 
    		get 
    		{    		
    			if ( (field_Nom.Text.Length == 0) || (field_Prenom.Text.Length == 0) )
    				return "Nouveau client";
    			else
    				return field_Nom + " " + field_Prenom; 
    		}
    	}
    	
    	public override string ToString() {
    		return "";
    	}
    	
    	public override string Destination {
			get { return "default"; } 
		}
    	
    	public override string SaveToXML() {
    	
    		XmlElement fields;
    		string fieldName;
    		string fieldValue;
    		string fieldPrefix = "field_";
    	
    		//récupération de tous les widgets à sauvegarder
   			Gtk.Widget[] widgets = XMLGlade.GetWidgetPrefix(fieldPrefix);
   			
   			//si plusieurs alors créer un noeud racine
   			if (widgets.Length > 0) {
   				fields = XmlDoc.AddNode(null, "Fields", "");
   			} else {
   				return base.XmlDoc.ToString();
   			}
   			
   			//parcours des widgets et constitution du flux
   			foreach (Gtk.Widget w in widgets) {
   			
   				//nom du champ
   				StringBuilder name = new StringBuilder(w.Name);
   				name.Remove(0, fieldPrefix.Length);
   				fieldName = name.ToString();
   				
   				//récupération de la valeur en fonction du type de widget
   				Type entryType = typeof(Gtk.Entry);  				
   				Type  textViewType = typeof(Gtk.TextView);
   				fieldValue = "";
   				
   				//Entry
   				if (entryType.IsInstanceOfType(w)) {
   					fieldValue = w.GetType().GetProperty("Text").GetValue(w, null).ToString();
   					Console.WriteLine("lecture entry = " + fieldValue);
   				}
   				
   				//TextView
   				if (textViewType.IsInstanceOfType(w)) {
   					Gtk.TextBuffer buffer = ((Gtk.TextBuffer)w.GetType().GetProperty("Buffer").GetValue(w, null));
   					fieldValue = buffer.GetType().GetProperty("Text").GetValue(buffer, null).ToString();
   					Console.WriteLine("lecture textview = " + fieldValue);
   				}
   				
   				XmlElement field = XmlDoc.AddNode(fields, "Field", fieldValue);
   				XmlDoc.AddAttributeNode(field, "name", fieldName);
				
   			}
   			
   			Console.WriteLine("content = " + XmlDoc.ToString());
   			
   			return XmlDoc.ToString();
   		}

	}
	
}
