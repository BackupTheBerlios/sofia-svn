using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Practices.CompositeUI.SmartParts;
using Microsoft.Practices.ObjectBuilder;

namespace Microsoft.Practices.CompositeUI.Windows.BuilderStrategies
{
	/// <summary>
	/// Implements a <see cref="BuilderStrategy"/> for <see cref="FrameworkElement"/> smart parts.
	/// </summary>
	public class FrameworkElementSmartPartStrategy : BuilderStrategy
	{
		private WorkItem GetWorkItem(IReadableLocator locator)
		{
			return locator.Get<WorkItem>(new DependencyResolutionLocatorKey(typeof(WorkItem), null));
		}

		public override object BuildUp(IBuilderContext context, Type t, object existing, string id)
		{
			if (existing is FrameworkElement)
			{
				AddHierarchy(GetWorkItem(context.Locator), existing as FrameworkElement);
			}

			return base.BuildUp(context, t, existing, id);
		}

		public override object TearDown(IBuilderContext context, object item)
		{
			if (item is FrameworkElement)
			{
				RemoveHierarchy(GetWorkItem(context.Locator), item as FrameworkElement);
			}

			return base.TearDown(context, item);
		}

		private void AddHierarchy(WorkItem workItem, FrameworkElement frameworkElement)
		{
			ReplaceIfPlaceHolder(workItem, frameworkElement);

			foreach (object child in LogicalTreeHelper.GetChildren(frameworkElement))
			{
				if (!AddToWorkItem(workItem, child))
				{
					if (child is FrameworkElement)
					{
						AddHierarchy(workItem, child as FrameworkElement);
					}
				}
			}
		}

		private void RemoveHierarchy(WorkItem workItem, FrameworkElement frameworkElement)
		{
			if (frameworkElement != null)
			{
				RemoveNested(workItem, frameworkElement);
			}
		}

		private bool AddToWorkItem(WorkItem workItem, object item)
		{
			if (item != null)
			{
				if (ShouldAddToWorkItem(workItem, item))
				{
					FrameworkElement frameworkElement = item as FrameworkElement;

					if ((frameworkElement != null) && (!string.IsNullOrEmpty(frameworkElement.Name)))
					{
						workItem.Items.Add(frameworkElement, frameworkElement.Name);
					}
					else
					{
						workItem.Items.Add(frameworkElement);
					}

					return true;
				}
			}

			return false;
		}

		private bool ShouldAddToWorkItem(WorkItem workItem, object item)
		{
			return !workItem.Items.ContainsObject(item) && (IsSmartPart(item) || IsWorkspace(item) || IsPlaceholder(item));
		}

		private bool IsPlaceholder(object item)
		{
			return (item is ISmartPartPlaceholder);
		}

		private bool IsSmartPart(object item)
		{
			return (item.GetType().GetCustomAttributes(typeof(SmartPartAttribute), true).Length > 0);
		}

		private bool IsWorkspace(object item)
		{
			return (item is IWorkspace);
		}

		private void RemoveNested(WorkItem workItem, FrameworkElement frameworkElement)
		{
			foreach (object child in LogicalTreeHelper.GetChildren(frameworkElement))
			{
				workItem.Items.Remove(child);

				if (child is FrameworkElement)
				{
					RemoveNested(workItem, child as FrameworkElement);
				}
			}
		}

		private void ReplaceIfPlaceHolder(WorkItem workItem, FrameworkElement frameworkElement)
		{
			ISmartPartPlaceholder placeholder = frameworkElement as ISmartPartPlaceholder;

			if (placeholder != null)
			{
				FrameworkElement replacement = workItem.Items.Get<FrameworkElement>(placeholder.SmartPartName);

				if (replacement != null)
				{
					placeholder.SmartPart = replacement;
				}
			}
		}
	}
}
