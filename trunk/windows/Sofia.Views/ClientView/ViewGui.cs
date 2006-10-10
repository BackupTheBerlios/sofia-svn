using System;
using System.Text;
using System.Xml;

using Sofia.Core;

namespace Sofia.Views.ClientView
{
	
	public class ViewGui : BaseView
	{
	
		
		public ViewGui () : base ("gui.glade", "ViewGui")
		{
		}
		
    	public override string Caption { get { return "Client"; } }
    	
    	public override string ToString() {
    		return "";
    	}
    	
    	public override XmlDocument SaveToXML() {
    	
    		XmlElement fields;
    		string widgetPrefix = "field_";
    	
    		//récupération de tous les widgets à sauvegarder
   			Gtk.Widget[] widgets = XMLGlade.GetWidgetPrefix(widgetPrefix);
   			
   			//si plusieurs alors créer un noeud racine
   			if (widgets.Length > 0) {
   				fields = XmlDoc.CreateElement("Fields");
   			} else {
   				return base.XmlDoc;
   			}
   			
   			//parcours des widgets et constitution du flux
   			foreach (Gtk.Widget w in widgets) {
   			
   				//mise en place du noeud
   				XmlElement field = XmlDoc.CreateElement("Field");
   				XmlAttribute attr = XmlDoc.CreateAttribute("name");
   				StringBuilder name = new StringBuilder(w.Name);
   				name.Remove(1, widgetPrefix.Length);
   				attr.Value = name.ToString();
   				field.SetAttributeNode(attr);
   				
   				//récupération de la valeur en fonction du type de widget
   				Type entryType = typeof(Gtk.Entry);  				
   				Type  textViewType = typeof(Gtk.TextView);
   				
   				//Entry
   				if (entryType.IsInstanceOfType(w)) {
   					field.InnerText = w.GetType().GetProperty("Text").GetValue(w, null).ToString();
   				}
   				
   				//TextView
   				if (textViewType.IsInstanceOfType(w)) {
   					Gtk.TextBuffer buffer = ((Gtk.TextBuffer)w.GetType().GetProperty("Buffer").GetValue(w, null));
   					field.InnerText = buffer.GetType().GetProperty("Text").GetValue(buffer, null).ToString();
   				}
   				
   				//ajout du noeud
   				fields.AppendChild(field);   				
   			}
   			
   			//ajout des champs au document xml
   			XmlDoc.DocumentElement.FirstChild.AppendChild(fields); 	
   			
   			return XmlDoc;
   		}

	}
	
}
