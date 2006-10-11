
using System;
using System.Xml;
using System.Xml.XPath;
using System.Collections;
using System.IO;

namespace Sofia.Core.XmlTools
{
	
       /// <summary>
       /// Cette classe permet de retourner la valeur d'une expression XPAth
       /// </summary>
       public class XPathNavigatorFacade
       {
             XPathDocument xpathDoc;
             XPathNavigator xpathNavigator;
             
             public XPathNavigatorFacade()
             {
             }
             
             public void LoadFromFile(string xmlFileName)
             {
                    xpathDoc = new XPathDocument(xmlFileName);
             }
             
             public void LoadXML(string rawXML)
             {
             		StringReader sr = new StringReader(rawXML);
             	 	xpathDoc = new XPathDocument(sr);
             }

             /// <summary>
             /// Récupère la valeur de l'attribut du noeud recherché dans le fichier de configuration
             /// </summary>
             /// <param name="xPathString">Expression XPath de recherche du noeud</param>
             /// <param name="attribute">Attribut à rechercher</param>
             /// <returns>Une ArrayList contenant la liste des attributs recherchés</returns>
             public  ArrayList GetAttributes(string xPathString, string attribute)
             {

                    // Initilisation des variables                    
                    XPathNodeIterator xpathNodeIterator;
                    XPathExpression expr;

                    ArrayList attributes = new ArrayList();

                    // Parcours du fichier XML
                    try
                    {
                    	   xpathNavigator = xpathDoc.CreateNavigator();
                           expr = xpathNavigator.Compile(xPathString);
                           xpathNodeIterator = xpathNavigator.Select(expr);
 
                           while (xpathNodeIterator.MoveNext())
                           {
                                  // On récupère l'attribut
                                  attributes.Add(xpathNodeIterator.Current.GetAttribute(attribute, ""));
                           }
                    }
                    catch (Exception e)
                    {
                    }

                    return attributes;
             }
             
             public string GetValue(string xPathString) 
             {
             		xpathNavigator = xpathDoc.CreateNavigator();
                    XPathExpression expr = xpathNavigator.Compile(xPathString);
                    XPathNodeIterator xpathNodeIterator = xpathNavigator.Select(expr);
                    xpathNodeIterator.MoveNext();
                    return xpathNodeIterator.Current.Value;                           
             }

       }	
}
