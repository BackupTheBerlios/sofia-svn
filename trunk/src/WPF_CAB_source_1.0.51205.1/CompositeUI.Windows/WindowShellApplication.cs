using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// Extends <see cref="WindowsApplication{TWorkItem, TShell}"/> to support an application which uses a Windows Presentation
	/// Foundation <see cref="Window"/> as its shell.
	/// </summary>
	/// <typeparam name="TWorkItem">
	/// The type of the root application work item.
	/// </typeparam>
	/// <typeparam name="TShell">
	/// The type for the shell to use.
	/// </typeparam>
	public abstract class WindowShellApplication<TWorkItem, TShell> : WindowsApplication<TWorkItem, TShell>
		where TWorkItem : WorkItem, new()
		where TShell : Window, new()
	{
		/// <summary>
		/// Calls <see cref="Application.Run(Window)"/> to start the application.
		/// </summary>
		protected override void Start()
		{
			//running the application as per the commented out line below triggers an MDA:
			//FatalExecutionEngineError was detected
			//Message: The runtime has encountered a fatal error. The address of the error was at 0x5688d12b, on thread 0x10d8. The error code is 0x80131623. This error may be a bug in the CLR or in the unsafe or non-verifiable portions of user code. Common sources of this bug include user marshaling errors for COM-interop or PInvoke, which may corrupt the stack.
			//Application.Current.Run(Shell);

			//have to show dialog to ensure we block
			Shell.ShowDialog();
		}
	}
}
