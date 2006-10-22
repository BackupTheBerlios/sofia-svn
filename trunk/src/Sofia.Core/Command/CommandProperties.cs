
using System;

namespace Sofia.Core
{
	
	public class CommandProperties
	{
		string text;
		string icon;
		string accelKey;
		string description;
		string type;
				
		public CommandProperties ()
		{
		}
		
		public CommandProperties (string text)
		{
			this.text = text;
		}
		
		public CommandProperties (string text, string icon, string accelKey, string description, string type)
		{
			this.text = text;
			this.icon = icon;
			this.accelKey = accelKey;
			this.description = description;
		}
		
		public string Text {
			get { return text; }
			set { text = value; }
		}
		
		public string Icon {
			get { return icon; }
			set { icon = value; }
		}
		
		public string AccelKey {
			get { return accelKey; }
			set { accelKey = value; }
		}
		
		public string Description {
			get { return description; }
			set { description = value; }
		}
		
		public string Type {
			get { return type; }
			set { type = value; }
		}
		
	}
	
}
