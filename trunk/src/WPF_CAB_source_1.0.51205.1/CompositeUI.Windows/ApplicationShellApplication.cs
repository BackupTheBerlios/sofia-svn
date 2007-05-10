using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using System.Windows;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// Extends <see cref="WindowsApplication{TWorkItem, TShell}"/> to support WPF applications that use a WPF <see cref="Application"/>
	/// instance as their shell.
	/// </summary>
	/// <typeparam name="TWorkItem">
	/// The work item type.
	/// </typeparam>
	/// <typeparam name="TShell">
	/// The shell type.
	/// </typeparam>
	public abstract class ApplicationShellApplication<TWorkItem, TShell> : WindowsApplication<TWorkItem, TShell>
		where TWorkItem : WorkItem, new()
		where TShell : Application, new()
	{
		protected override void AfterShellCreated()
		{
			base.AfterShellCreated();

			//have to initialize the app - no way other than reflection?
			MethodInfo initializeMethod = typeof(TShell).GetMethod("InitializeComponent", BindingFlags.Public | BindingFlags.Instance);

			if (initializeMethod != null)
			{
				initializeMethod.Invoke(Shell, null);
			}
		}

		protected override void Start()
		{
			Shell.Run();
		}
	}
}
