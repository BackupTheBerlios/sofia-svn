
using System;
using System.Collections;

namespace Sofia.Core.Commands
{
	
	public  class CommandBase : ICommand
	{
				
		string _Id;
		CommandProperties _Properties;
		Hashtable _ParamList;
		CommandManager _CommandManager;
        ICommandReceiver _CommandReceiver;
		
		public CommandBase(string id, ICommandReceiver commandReceiver, string text, string icon, string accelKey, string description)
		{		 	
		 	_Id = id;
            _CommandReceiver = commandReceiver;
		 	_Properties = new CommandProperties(text, icon, accelKey, description);
		 	_ParamList = new Hashtable();
		}
		
		public virtual void Execute(string parameters) 
		{			
			Xml.XPath xpn= new Xml.XPath();
			xpn.LoadXML(parameters);
     		
     		ArrayList names = xpn.GetAttributes("/Params", "name");
     		ArrayList values = xpn.GetAttributes("/Params", "value");
     		
     		for (int i = 0; i < names.Count; i++) {
     			_ParamList[names[i].ToString()] = values[i].ToString();
     		}

            _CommandReceiver.Action(this);

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
		
		public CommandManager CommandManager { 
			get { return _CommandManager; } 
			set { _CommandManager = value; }
		}
		
		
		public Hashtable ParamList {
			get { return _ParamList; }
		}
		
	}
	
}
