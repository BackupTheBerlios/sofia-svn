
using System;
using System.Collections.Generic;

namespace Sofia.Commands
{
	
	public class CommandBase : ICommand
	{
				
		string _identifier;
		CommandProperties _properties;
		Dictionary<string, string> _paramList;
		CommandManager _commandManager;
        ICommandReceiver _commandReceiver;
		
		public CommandBase(string identifier, string text, string icon, string accelKey, string description)
		{		 	
		 	_identifier = identifier;
		 	_properties = new CommandProperties(text, icon, accelKey, description);
            _paramList = new Dictionary<string, string>();
		}
		
		public virtual void Execute(string parameters) 
		{			
			Xml.XPath xpn= new Xml.XPath();
			xpn.LoadXML(parameters);
     		
     		List<string> names = xpn.GetAttributes("/Params", "name");
     		List<string> values = xpn.GetAttributes("/Params", "value");
     		
     		for (int i = 0; i < names.Count; i++) {
     			_paramList[names[i]] = values[i];
     		}

            _commandReceiver.Action(this);

		}
		
		public virtual void UnExecute() 
		{
			throw new NotSupportedException();
		}
		
		public string Identifier { 
			get { return _identifier; } 
		}
		
		public CommandProperties Properties { 
			get { return _properties; } 
		}
		
		public CommandManager CommandManager { 
			get { return _commandManager; } 
			set { _commandManager = value; }
		}

        public ICommandReceiver CommandReceiver
        {
            get { return _commandReceiver; }
            set { _commandReceiver = value; }
        }
				
		public Dictionary<string, string> ParamList {
			get { return _paramList; }
		}
		
	}
	
}
