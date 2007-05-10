using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Practices.CompositeUI.Utility;
using Microsoft.Practices.CompositeUI.Commands;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// Adapts between a WPF <see cref="FrameworkElement"/> and a CAB <see cref="Command"/>.
	/// </summary>
	public class FrameworkElementCommandAdapter : EventCommandAdapter<FrameworkElement>
	{
		public FrameworkElementCommandAdapter() 
			: base()
		{
		}

		public FrameworkElementCommandAdapter(FrameworkElement item, string eventName)
			: base(item, eventName)
		{
		}

		protected override void OnCommandChanged(Command command)
		{
			base.OnCommandChanged(command);

			foreach (FrameworkElement key in Invokers.Keys)
			{
				key.IsEnabled = (command.Status == CommandStatus.Enabled);
				key.Visibility = (command.Status == CommandStatus.Unavailable) ? Visibility.Collapsed : Visibility.Visible;
			}
		}
	}
}
