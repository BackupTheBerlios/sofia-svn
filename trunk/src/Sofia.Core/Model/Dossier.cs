
using System;
using System.Collections;

namespace Sofia.Core.Model
{

	[Serializable()]
	public class Dossier
    {
        string caption;
        DateTime creation;
        IList documents;
        
        public Dossier(DateTime creation, string caption)
        {
        	this.creation = creation;
            this.caption = caption;
            documents = new ArrayList();
        }
        
        public void AddDocument(Document document)
        {
        	documents.Add(document);
        }
      	
        public string Caption { 
        	get { return caption; } 
        }
        
        public DateTime Creation { 
        	get { return creation; } 
        }
        
        public IList Documents {
        	get
        	{
        		return documents;
        	}
        }
        
        public override string ToString()
        {
            return string.Format("{0} {1}", caption, creation);
        }
    }
	
}
