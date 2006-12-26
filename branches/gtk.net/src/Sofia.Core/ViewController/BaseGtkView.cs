
using System;
using Gtk;

using System.Xml;

using Glade;
using Assembly = System.Reflection.Assembly;

namespace Sofia.Core
{
	
	public class BaseView : HBox, IView
	{
		
		Glade.XML _Glade;
		string _WidgetName;
		XmlTools.XmlDocumentFacade _XmlDoc;
		
		#region implémentation de l'interface
		
		IController _Controller;
		string _DocumentID;
		
    	public HBox VisualComponent { 
    		get { return this; } 
    	}
    	
    	public IController Controller 	{ 
    		get { return _Controller; }
    		set { _Controller = value; }
    	}
    	
    	public virtual string Caption { 
    		get { throw new NotSupportedException(); } 
    	}
    	
    	public virtual string MasterCaption { 
    		get { return Caption; } 
    	}

    	
    	public virtual void Initialize () {
    	}

		public virtual string Destination { 
			get { return "default"; } 
		}
		
		public virtual string DocumentID { 
			get 
			{ 
				Console.WriteLine("ici");
				if (_DocumentID.Length == 0)
				{
				Console.WriteLine("ici1");
					_DocumentID = Guid.NewGuid().ToString("N");
					Console.WriteLine("ici2");
				}
				return _DocumentID;
			}
			
			set 
			{ _DocumentID = value; }
		}
			
		
		public virtual void LoadFromXML(string xml) {
		}
		
        public virtual string SaveToXML() {
        	return _XmlDoc.ToString();
        }
        
        public virtual void AddToolbarItem(ToolbarItem item)
        {
        	throw new NotSupportedException();	
        }
		
    	#endregion
    	
		private BaseView (string _WidgetName) : base (false, 0)
		{
			this._WidgetName = _WidgetName;
			_XmlDoc = new XmlTools.XmlDocumentFacade("<Document revision='0'/>");
			_DocumentID = "";
		}

		protected BaseView (string resourceName, string _WidgetName, string nameSpace) : this (_WidgetName)
		{					
			string fullName = nameSpace + '.' + resourceName;
			
			Assembly a = Assembly.GetCallingAssembly();
			
			if (!System.IO.File.Exists(a.CodeBase + "/" + fullName))
				fullName = resourceName;
						
			_Glade = new XML (Assembly.GetCallingAssembly (), resourceName, _WidgetName, null);
			Init ();
		}

		void Init ()
		{
			_Glade.Autoconnect (this);
			
			Window win = (Window) _Glade [_WidgetName];
			win.Visible = false;
			Widget child = win.Child;
			
			child.Reparent (this);
			win.Destroy ();
		}
		
		protected XmlTools.XmlDocumentFacade XmlDoc 
		{
			get { return _XmlDoc; }
		}
		
		protected Glade.XML XMLGlade
		{
			get { return _Glade; }
		}

	}    
	
}
