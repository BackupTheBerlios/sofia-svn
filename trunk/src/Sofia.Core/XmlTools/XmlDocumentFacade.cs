
using System;
using System.Xml;

namespace Sofia.Core.XmlTools
{
	
	public class XmlDocumentFacade
	{
		private XmlDocument xmlDoc;
		
		public XmlDocumentFacade(string root)
		{
			xmlDoc = new XmlDocument();
			
			if (root.Length == 0)
				root = "<Document/>";
				
			xmlDoc.LoadXml(root);
		}
		
		public override string ToString()
		{
			return xmlDoc.InnerXml;
		}
		
		public XmlElement AddNode(XmlElement parent, string name, string value)
		{
			XmlElement e = xmlDoc.CreateElement(name);
			
			if (value.Length != 0)
				e.InnerText = value;
				
			if (parent == null)
				xmlDoc.DocumentElement.AppendChild(e);
			else
				parent.AppendChild(e);
				
			return e;
		}
		
		public XmlAttribute AddAttributeNode(XmlElement node, string name, string value)
		{
			XmlAttribute a = xmlDoc.CreateAttribute(name);
			a.Value = value;
			
			if (node == null)
				xmlDoc.DocumentElement.SetAttributeNode(a);
			else
			  node.SetAttributeNode(a);
			  
			 return a;
		}
				
	}
	
}
