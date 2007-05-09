using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;

namespace Microsoft.Practices.CompositeUI.Windows.Activation
{
	/// <summary>
	/// An implementation of <see cref="IFrameworkElementActivationService"/>.
	/// </summary>
	public class FrameworkElementActivationService : IFrameworkElementActivationService
	{
		private WorkItem _workItem;

		/// <summary>
		/// The <see cref="WorkItem"/> where this service lives.
		/// </summary>
		[ServiceDependency]
		public WorkItem WorkItem
		{
			set
			{
				_workItem = value;
			}
		}

		public void FrameworkElementAdded(FrameworkElement frameworkElement)
		{
			frameworkElement.GotFocus += frameworkElement_GotFocus;
			frameworkElement.Unloaded += frameworkElement_Unloaded;
		}

		public void FrameworkElementRemoved(FrameworkElement frameworkElement)
		{
			frameworkElement.GotFocus -= frameworkElement_GotFocus;
			frameworkElement.Unloaded -= frameworkElement_Unloaded;
		}

		void frameworkElement_GotFocus(object sender, RoutedEventArgs e)
		{
			//don't deactivate if the user is just opening a menu (not sure this is correct in all cases)
			if (e.OriginalSource is MenuItem)
			{
				return;
			}

			if (_workItem != null)
			{
				e.Handled = true;
				_workItem.Activate();
			}
		}

		void frameworkElement_Unloaded(object sender, RoutedEventArgs e)
		{
			FrameworkElement frameworkElement = e.OriginalSource as FrameworkElement;
			frameworkElement.GotFocus -= frameworkElement_GotFocus;
			frameworkElement.Unloaded -= frameworkElement_Unloaded;
		}
	}
}
