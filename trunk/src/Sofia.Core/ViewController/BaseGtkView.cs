
using System;
using Gtk;

using System.Xml;

using Glade;
using Assembly = System.Reflection.Assembly;

namespace Sofia.Core
{
	
	public class BaseView : HBox, IView
	{
		
		Glade.XML glade;
		string widgetName;
		XmlTools.XmlDocumentFacade xmlDoc;
		
		#region implémentation de l'interface
		
		IController controller;
		
    	public HBox VisualComponent { 
    		get { return this; } 
    	}
    	
    	public IController Controller 	{ 
    		get { return controller; }
    		set { controller = value; }
    	}
    	
    	public virtual string Caption { 
    		get { throw new NotSupportedException(); } 
    	}
    	
    	public virtual void Initialize () {
    	}

		public virtual string Destination { 
			get { return "default"; } 
		}
		
		public virtual string DocumentID { 
			get { throw new NotSupportedException(); } 
		}
		
		public virtual  void LoadFromXML(string xml) {
		}
		
        public virtual string SaveToXML() {
        	return xmlDoc.ToString();
        }
        
        public virtual void AddToolbarItem(ToolbarItem item)
        {
        	throw new NotSupportedException();	
        }
		
    	#endregion
    	
		private BaseView (string widgetName) : base (false, 0)
		{
			this.widgetName = widgetName;
			xmlDoc = new XmlTools.XmlDocumentFacade("<Document revision='0'/>");			
		}

		protected BaseView (string resourceName, string widgetName, string nameSpace) : this (widgetName)
		{					
			string fullName = nameSpace + '.' + resourceName;
			
			Assembly a = Assembly.GetCallingAssembly();
			
			if (!System.IO.File.Exists(a.CodeBase + "/" + fullName))
				fullName = resourceName;
						
			glade = new XML (Assembly.GetCallingAssembly (), resourceName, widgetName, null);
			Init ();
		}

		void Init ()
		{
			glade.Autoconnect (this);
			
			Window win = (Window) glade [widgetName];
			win.Visible = false;
			Widget child = win.Child;
			
			child.Reparent (this);
			win.Destroy ();
		}
		
		protected XmlTools.XmlDocumentFacade XmlDoc 
		{
			get { return xmlDoc; }
		}
		
		protected Glade.XML XMLGlade
		{
			get { return glade; }
		}

	}    
	
}
