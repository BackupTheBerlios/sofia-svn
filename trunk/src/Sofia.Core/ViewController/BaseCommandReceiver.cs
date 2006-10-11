
using System;

namespace Sofia.Core
{
	
	/// <summary>
	/// Classe receiver 
	/// </summary>
	public class BaseCommandReceiver : ICommandReceiver
	{
		public BaseCommandReceiver()
		{
		}
		
		#region Implémentation de l'interface
		
		public virtual IView View 
		{ 
			get { return null; } 
		}
		
		#endregion 
	}
	
}
