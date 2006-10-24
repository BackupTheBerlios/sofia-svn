
using System;
using System.Collections;

namespace Sofia.Core
{
	
	public  class BaseCommand : ICommand
	{
				
		string id;
		CommandProperties properties;
		Hashtable paramList;
		
		public BaseCommand(string id, string text, string icon, string accelKey, string description, string type)
		{
		 	this.id = id;
		 	
		 	properties = new CommandProperties(text, icon, accelKey, description, type);
		 	paramList = new Hashtable();
		}
		
		public virtual void Execute(string parameters) 
		{
			XmlTools.XPathNavigatorFacade xpn= new XmlTools.XPathNavigatorFacade();
			xpn.LoadXML(parameters);
     		
     		ArrayList names = xpn.GetAttributes("/Params", "name");
     		ArrayList values = xpn.GetAttributes("/Params", "value");
     		
     		for (int i = 0; i < names.Count; i++) {
     			paramList[names[i].ToString()] = values[i].ToString();
     		}

		}
		
		public virtual void UnExecute() 
		{
		}
		
		public string Id { 
			get { return id; } 
		}
		
		public CommandProperties Properties { 
			get { return properties; } 
		}
		
		public Hashtable ParamList {
			get { return paramList; }
		}
		
	}
	
}
