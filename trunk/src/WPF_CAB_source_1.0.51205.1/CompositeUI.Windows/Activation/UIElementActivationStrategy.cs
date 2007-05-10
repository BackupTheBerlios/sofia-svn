using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using Microsoft.Practices.CompositeUI.SmartParts;
using Microsoft.Practices.ObjectBuilder;

namespace Microsoft.Practices.CompositeUI.Windows.Activation
{
	public class UIElementActivationStrategy : BuilderStrategy
	{
		public override object BuildUp(IBuilderContext context, Type typeToBuild, object existing, string idToBuild)
		{
			UIElement uiElement = existing as UIElement;

			if (uiElement != null)
			{
				WorkItem workItem = context.Locator.Get<WorkItem>(new DependencyResolutionLocatorKey(typeof(WorkItem), null));
				workItem.Services.Get<IUIElementActivationService>(true).UIElementAdded(uiElement);
			}

			return base.BuildUp(context, typeToBuild, existing, idToBuild);
		}

		public override object TearDown(IBuilderContext context, object item)
		{
			UIElement uiElement = item as UIElement;

			if (uiElement != null)
			{
				WorkItem workItem = context.Locator.Get<WorkItem>(new DependencyResolutionLocatorKey(typeof(WorkItem), null));
				workItem.Services.Get<IUIElementActivationService>(true).UIElementRemoved(uiElement );
			}

			return base.TearDown(context, item);
		}
	}
}
