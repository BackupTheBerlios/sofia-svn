using System;

namespace Model
{
	/// <summary>
	/// Description of Document.
	/// </summary>
	public class Document
	{
		string caption;
        DateTime creation;
        
        public Document(DateTime creation, string caption)
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
