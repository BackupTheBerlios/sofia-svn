
using System;
using System.Collections;
using System.Xml;
using System.Xml.Serialization;
using System.IO;
using System.Text;

using com.db4o;
using com.db4o.query;

using Sofia.Core.XmlTools;

namespace Sofia.Core.Model
{
		
    public class Model : IModel
    {
		ObjectContainer db;
		XPathNavigatorFacade xpn;
		
		public Model()
		{
			xpn = new XPathNavigatorFacade();
		}
		
    	#region implémentation de l'interface
    	
    	public IList SendRequest(string request)
    	{
    		ArrayList objectSets = new ArrayList();    		
    		db = Db4o.OpenFile("sofia.yap");
    		try
    		{    	  		    			
    			xpn.LoadXML(request);
    			ArrayList operations = xpn.GetAttributes("//Request", "operation");
     			ArrayList objects = xpn.GetAttributes("//Request", "object");
     			
     			for (int i = 0; i < operations.Count; i++) {
    				string ope = operations[i].ToString();
     				string obj = objects[i].ToString();
     				ObjectSet objectSet = ProcessRequest(ope, obj);
     		        if (objectSet != null) 
     		        	objectSets.Add(objectSet);
    			}
    			
	   			if (objectSets.Count > 0) 
    			{	    	
	   				IList list = new ArrayList();
    				foreach(Dossier dossier in (ObjectSet) objectSets[0])
    				{
    					list.Add(dossier);
    				}
    	  			return list;
    	  		}
    	  		else
    	  		 	return null;
    	  		
    	  	}
    	  	finally
    	  	{
    	  		db.Close();
    	  	}
    	}
    	
    	#endregion
    	
    	private ObjectSet ProcessRequest(string  ope, string obj)
    	{
    		////////////////////////////////////////////Insertion
    		if (ope == "Insert") 
    		{
     		 		
     			if (obj == "MasterDocument") 
     			{
     		    	AddMasterDocument();
     		    	return null;
     		    }
				
    		}
    		
    		/////////////////////////////////////////////Sélection
    		if (ope == "Select") 
    		{
    			
    			if (obj == "Document")
    			{	
    				return GetDocument();
    			}
    			
    		}
    		
    		/////////////////////////////////////////////Else
    		return null;
    	}

		#region Prédicats
		
   		public class FolderPredicate : Predicate 
    	{
    		private string _Caption;
    		
    		public FolderPredicate(string caption)
    		{
    			_Caption = caption;
    		}
    		
    		public bool Match(Dossier dossier) 
    		{
    			Console.WriteLine(dossier.Caption);
        		return dossier.Caption.StartsWith(_Caption);
    		}
    	}

		#endregion
		
    	#region Méthodes d'accès aux données
    	
    	private string GetValue(string objectName, string attributeName)
    	{
    	 	string xValue = "//Request[@object='{0}']/Fields//Field[@name='{1}']";
    	 	return xpn.GetValue(string.Format(xValue, objectName, attributeName));
    	}
    	     	
       	private void AddMasterDocument()
    	{
    		string objectName = "MasterDocument";
     		string creation = GetValue(objectName, "creation");
     		string caption = GetValue(objectName, "caption");
     		string masterCaption = GetValue(objectName, "masterCaption");
     		string content = GetValue(objectName, "content");
    	
    		System.IFormatProvider frmt = new System.Globalization.CultureInfo("fr-FR", true);
			DateTime dt = DateTime.ParseExact(creation, "dd/MM/yyyy HH:mm:ss", frmt);
    		Dossier dossier = new Dossier(dt, masterCaption);
    		Document document = new Document(dt, caption, content);
			dossier.AddDocument(document);		
 		
    		db.Set(dossier);    		
    		Console.WriteLine("Dossier ajouté : " + dossier.ToString());
    	}
    	
    	private ObjectSet GetDocument()
    	{
    		string objectName = "Document";
     		string caption = GetValue(objectName, "caption");
     		//Dossier dossier = new Dossier(DateTime.Now, caption);
    		//return db.Get(dossier);
    		return db.Query(new FolderPredicate(caption));
		}
    	
    	#endregion
   	
    }
   
}
