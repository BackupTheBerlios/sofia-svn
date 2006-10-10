
using System;

namespace Sofia.Core.Model
{
	
	public class Dossier
    {
    	string folderId;
        string caption;
        DateTime creation;
        
        public Dossier(string folderId, DateTime creation, string caption)
        {
        	this.folderId = folderId;
        	this.creation = creation;
            this.caption = caption;            
        }
        
        public string FolderId { 
        	get { return folderId; } 
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
