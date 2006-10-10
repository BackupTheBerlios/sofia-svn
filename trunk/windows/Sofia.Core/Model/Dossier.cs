
using System;

namespace Sofia.Core.Model
{
	
	public class Dossier
    {
    	string documentId;
        string caption;
        DateTime creation;
        
        public Dossier(string documentId, DateTime creation, string caption)
        {
        	this.documentId = documentId;
        	this.creation = creation;
            this.caption = caption;            
        }
        
        public string DocumentId { 
        	get { return documentId; } 
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
