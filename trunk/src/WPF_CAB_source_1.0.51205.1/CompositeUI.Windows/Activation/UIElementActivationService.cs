using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;

namespace Microsoft.Practices.CompositeUI.Windows.Activation
{
	public class UIElementActivationService : IUIElementActivationService
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

		public void UIElementAdded(UIElement uiElement)
		{
			uiElement.GotFocus += uiElement_GotFocus;
		}

		public void UIElementRemoved(UIElement uiElement)
		{
			uiElement.GotFocus -= uiElement_GotFocus;
		}

		void uiElement_GotFocus(object sender, RoutedEventArgs e)
		{
			if (_workItem != null)
			{
				_workItem.Activate();
			}
		}
	}
}
