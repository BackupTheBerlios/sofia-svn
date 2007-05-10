using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using Microsoft.Practices.CompositeUI.UIElements;

namespace Microsoft.Practices.CompositeUI.Windows.UIElements
{
	/// <summary>
	/// Implements <see cref="IUIElementAdapterFactory"/> for WPF applications.
	/// </summary>
	public class ItemsControlUIAdapterFactory : IUIElementAdapterFactory
	{
		public IUIElementAdapter GetAdapter(object uiElement)
		{
			//dunno if this is right yet
			Debug.Assert(uiElement is FrameworkElement);
			ItemsControl itemsControl = GetAncestorItemsControl(uiElement as FrameworkElement);
			Debug.Assert(itemsControl != null);
			return new ItemsControlUIAdapter(itemsControl);
		}

		public bool Supports(object uiElement)
		{
			return (GetAncestorItemsControl(uiElement as FrameworkElement) != null);
		}

		private ItemsControl GetAncestorItemsControl(FrameworkElement frameworkElement)
		{
			while ((frameworkElement != null) && (!(frameworkElement is ItemsControl)))
			{
				frameworkElement = frameworkElement.Parent as FrameworkElement;
			}

			return frameworkElement as ItemsControl;
		}
	}
}
