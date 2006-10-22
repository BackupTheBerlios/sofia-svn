
using System;
using System.Xml;
using System.Text;

namespace Sofia.Core.GladeTools
{
	
	public class GladeTransform
	{
		
		private Glade.XML glade;
		XmlTools.XmlDocumentFacade xmlDoc;
		
		public GladeTransform(Glade.XML glade)
		{
			this.glade = glade;
			xmlDoc = new XmlTools.XmlDocumentFacade("<Document revision='0'/>");
		}
		
	    public override string ToString() 
	    {
	        	
    		XmlElement fields;
    		string fieldName;
    		string fieldValue;
    		string fieldPrefix = "field_";
    	
    		//récupération de tous les widgets à sauvegarder
   			Gtk.Widget[] widgets = glade.GetWidgetPrefix(fieldPrefix);
   			
   			//si plusieurs alors créer un noeud racine
   			if (widgets.Length > 0) {
   				fields = xmlDoc.AddNode(null, "Fields", "");
   			} else {
   				return xmlDoc.ToString();
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
   				
   				XmlElement field = xmlDoc.AddNode(fields, "Field", fieldValue);
   				xmlDoc.AddAttributeNode(field, "name", fieldName);
				
   			}
   			
   			Console.WriteLine("content = " + xmlDoc.ToString());
   			
   			return xmlDoc.ToString();
   		}

	}
	
}
