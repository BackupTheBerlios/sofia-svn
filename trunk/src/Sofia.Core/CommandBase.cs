
using System;
using System.Collections;

namespace Sofia.Core.Commands
{
	
	public  class CommandBase : ICommand
	{
				
		string _Id;
		CommandProperties _Properties;
		Hashtable _ParamList;
		CommandManager _Manager;
		
		public CommandBase(string id, string text, string icon, string accelKey, string description)
		{		 	
		 	_Id = id;
		 	
		 	_Properties = new CommandProperties(text, icon, accelKey, description);
		 	_ParamList = new Hashtable();
		}
		
		public virtual void Execute(string parameters) 
		{			
			XmlTools.XPath xpn= new XmlTools.XPath();
			xpn.LoadXML(parameters);
     		
     		ArrayList names = xpn.GetAttributes("/Params", "name");
     		ArrayList values = xpn.GetAttributes("/Params", "value");
     		
     		for (int i = 0; i < names.Count; i++) {
     			_ParamList[names[i].ToString()] = values[i].ToString();
     		}

		}
		
		public virtual void UnExecute() 
		{
			throw new NotSupportedException();
		}
		
		public string Id { 
			get { return _Id; } 
		}
		
		public CommandProperties Properties { 
			get { return _Properties; } 
		}
		
		public CommandManager Manager { 
			get { return _Manager; } 
			set { _Manager = value; }
		}
		
		
		public Hashtable ParamList {
			get { return _ParamList; }
		}
		
	}
	
}
