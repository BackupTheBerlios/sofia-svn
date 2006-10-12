
using System;
using System.Collections;

namespace Sofia.Core.Model
{
	
	public class Dossier
    {
        string caption;
        DateTime creation;
        IList dossiers;
        
        public Dossier(DateTime creation, string caption)
        {
        	this.creation = creation;
            this.caption = caption;
            dossiers = new ArrayList();
        }
        
        public void AddDocument(Document document)
        {
        	dossiers.Add(document);
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
