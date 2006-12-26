
using System;
using System.IO;
using System.Text;
using System.Xml;
using System.Xml.Xsl;
using Sofia.Components.HtmlControl;
using Sofia.Core;


namespace Sofia.Views.WelcomeView
{
	
	public class ViewGui : BaseView
	{
		MozillaControl htmlControl;
		
		public ViewGui()
		{
			Stetic.Gui.Build(this, typeof(Sofia.Views.WelcomeView.ViewGui));
			
			//Ajout d'une page d'accueil avec un navigateur web
			htmlControl = new MozillaControl();
			this.Add(htmlControl);
			htmlControl.Show();
			
			string datadir = Directory.GetCurrentDirectory() + "/";
			
			// build simple xml which XSLT will process into XHTML
			string myxml = 	"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" +
							"<WelcomePage>" +
							"<ResourcePath>" + datadir + "</ResourcePath>" +
							BuildRecentFoldersXml() +
							"</WelcomePage>";
			
			XmlDocument inxml = new XmlDocument();
			inxml.LoadXml(myxml);

			XslTransform xslt = new XslTransform();
            xslt.Load(datadir + "WelcomePage.xsl");
			StringWriter fs = new StringWriter();
			xslt.Transform(inxml, null, fs, null);

			htmlControl.Html = fs.ToString();
			
		}
		
		public override string Caption { get { return "Accueil"; } }
		
		string BuildRecentFoldersXml()
		{
			StringBuilder content = new StringBuilder();
			//TODO : récupérer la liste des 5 derniers dossiers par exemple
			/*
				content.Append("<DossiersRecents>");
				foreach (Dossier dossier in resultSet)
				{
					content.Append("<Dossier>");
					content.Append("<Uri>" + dossier.Id + "</Uri>");
					content.Append("<Name>" + dossier.Nom + "</Name>");
					content.Append("<DateModified>" + controller.TimeSinceEdited(dossier.DateModifie) + "</DateModified>");
					content.Append("</Dossier>");
				}
				content.Append("</DossiersRecents>");
			*/
			
				content.Append("<DossiersRecents>");
					content.Append("<Dossier>");
					content.Append("<Uri>" + "test" + "</Uri>");
					content.Append("<Name>" + "Daladier" + "</Name>");
					content.Append("<DateModified>" + "12 jours" + "</DateModified>");
					content.Append("</Dossier>");
				content.Append("</DossiersRecents>");

			return content.ToString();
		}
		
		///<summary>
		///Initialisation de l'affichage du navigateur
		///</summary>
		public override void Initialize()
		{			
			htmlControl.DelayedInitialize();
			//object t = new object();
			//htmlControl.Navigate("http://www.google.com", ref t, ref t,ref t,ref t); 
		}

	}
	
}
