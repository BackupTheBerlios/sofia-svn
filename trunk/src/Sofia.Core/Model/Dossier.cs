
using System;

namespace Sofia.Core.Model
{
	
	public class Dossier
    {
        string caption;
        DateTime creation;
        
        public Dossier(DateTime creation, string caption)
        {
        	this.creation = creation;
            this.caption = caption;            
        }
      	
        public string Caption { 
        	get { return caption; } 
        }
        
        public DateTime Creation { 
        	get { return creation; } 
        }
        
        public override string ToString()
        {
            return string.Format("{0} {1}", caption, creation);
        }
    }
	
}
