// project created on 19/07/2006 at 23:03
using System;
using System.Reflection;
using System.IO;
using Gtk;

namespace Sofia.Core
{
	class MainClass
	{
		public static void Main(string[] args)
		{	
			LoadMainView();	
		}
		
		///<summary>
		///Point d'entrÃ©e de l'application
		///La vue MainView contient la boucle principale
		///</summary>
		public static void LoadMainView()
	    {
			AssemblyName assemblyName = new AssemblyName();

      		assemblyName.Name = "MainView";
      		assemblyName.CodeBase = String.Concat("file:///", Directory.GetCurrentDirectory());

			Console.WriteLine("Chargement de " + assemblyName.CodeBase + "/" + assemblyName.FullName + ".dll...");
      		Assembly a = Assembly.Load(assemblyName);
      		Console.WriteLine("icicicici");
      		a.CreateInstance("Sofia.Views." + assemblyName.Name  + ".Controller"); 
      		Console.WriteLine(assemblyName.CodeBase + "/" + assemblyName.FullName + ".dll chargée et instanciée.");
 		}
 		
 		/*
 		static void ShowAssemblies(string Tag)
		{		   
			if (Tag != null)
				Console.WriteLine(Tag);
			Console.WriteLine(AppDomain.CurrentDomain.ToString());
			foreach (Assembly LoadedAssembly in AppDomain.CurrentDomain.GetAssemblies())
				Console.WriteLine("\t{0}", LoadedAssembly.GetName().Name);
		}
		*/

	}
}
