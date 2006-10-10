
using System;
using System.Collections;

using com.db4o;

using Sofia.Core.XmlTools;

namespace Sofia.Core.Model
{
		
    public class Model : IModel
    {
		ObjectContainer db;
		
    	#region implémentation de l'interface
    	
    	public ObjectSet SendRequest(string request)
    	{
    		db = Db4o.OpenFile("sofia.data");
    		try
    		{    	  		
    			XPathNavigatorFacade xpn = new XPathNavigatorFacade();
    			xpn.LoadXML(request);
    			ArrayList operations = xpn.GetAttributes("//Request", "operation");
     			ArrayList objects = xpn.GetAttributes("//Request", "object");
     			
     			for (int i = 0; i < operations.Count; i++) {
     			
     				string ope = operations[i].ToString();
     				string obj = objects[i].ToString();
     		    
					string xValue = "//Request[@object='{0}']/Fields//Field[@name='{1}']";
     		    
     		    	if (ope == "Insert") {
     		    		
     		    		if (obj == "Dossier")  {
     		    		
     		    			string documentId = xpn.GetValue(string.Format(xValue, obj, "documentid"));
     		    			string creation = xpn.GetValue(string.Format(xValue, obj, "creation"));
     		    			string caption = xpn.GetValue(string.Format(xValue, obj, "caption"));
							AddDossier(documentId, creation, caption);
    					}
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
    	
    	
    	void AddDossier(string documentId, string creation, string caption)
    	{
    		System.IFormatProvider frmt = new System.Globalization.CultureInfo("fr-FR", true);
			DateTime dt = DateTime.ParseExact(creation, "dd/MM/yyyy HH:mm:ss", frmt);
    		Dossier dossier = new Dossier(documentId, dt, caption);
    		db.Set(dossier);
    		Console.WriteLine("Dossier ajouté : " + dossier.ToString());
    	}
    	
    }
   
}
