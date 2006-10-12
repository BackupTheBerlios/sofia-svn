using System;

namespace Sofia.Core.Model
{
	/// <summary>
	/// Description of Document.
	/// </summary>
	public class Document
	{
		string caption;
        DateTime creation;
        string content;
        
        public Document(DateTime creation, string caption, string content)
        {
        	this.creation = creation;
            this.caption = caption;
            this.content = content;
        }
      	
        public string Caption { 
        	get { return caption; } 
        }
        
        public DateTime Creation { 
        	get { return creation; } 
        }
        
        public string Content { 
        	get { return content; } 
        }
        
        public override string ToString()
        {
            return string.Format("{0} {1}", caption, creation);
        }
	}
}
