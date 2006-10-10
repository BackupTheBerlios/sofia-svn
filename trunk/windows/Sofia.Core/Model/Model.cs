
using System;
using System.Collections;

using com.db4o;

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
    	
    	public ObjectSet SendRequest(string request)
    	{
    		db = Db4o.OpenFile("sofia.data");
    		try
    		{    	  		    			
    			xpn.LoadXML(request);
    			ArrayList operations = xpn.GetAttributes("//Request", "operation");
     			ArrayList objects = xpn.GetAttributes("//Request", "object");
     			
     			for (int i = 0; i < operations.Count; i++) {
     			
     				string ope = operations[i].ToString();
     				string obj = objects[i].ToString();
     		    
     		    	if (ope == "Insert") {
     		    		
     		    		if (obj == "Dossier") 
     		    			AddDossier();
							
    				}      				
    			}
    			
    	  		return null;
    	  		
    	  	}
    	  	finally
    	  	{
    	  		db.Close();
    	  	}
    	}
    	
    	#endregion
    	
    	private string GetValue(string objectName, string attributeName)
    	{
    	 	string xValue = "//Request[@object='{0}']/Fields//Field[@name='{1}']";
    	 	return xpn.GetValue(string.Format(xValue, objectName, attributeName));
    	}
    	
       	void AddDossier()
    	{
    		string objectName = "Dossier";
    		string folderId = GetValue(objectName, "folderid");
     		string creation = GetValue(objectName, "creation");
     		string caption = GetValue(objectName, "caption");
    	
    		System.IFormatProvider frmt = new System.Globalization.CultureInfo("fr-FR", true);
			DateTime dt = DateTime.ParseExact(creation, "dd/MM/yyyy HH:mm:ss", frmt);
    		Dossier dossier = new Dossier(folderId, dt, caption);
    		db.Set(dossier);
    		Console.WriteLine("Dossier ajouté : " + dossier.ToString());
    	}
    	
    }
   
}
