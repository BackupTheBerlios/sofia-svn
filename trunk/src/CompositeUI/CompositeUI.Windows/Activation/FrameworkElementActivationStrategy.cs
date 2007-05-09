using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using Microsoft.Practices.CompositeUI.SmartParts;
using Microsoft.Practices.ObjectBuilder;

namespace Microsoft.Practices.CompositeUI.Windows.Activation
{
	/// <summary>
	/// Implements a <see cref="BuilderStrategy"/> for <see cref="FrameworkElement"/>s.
	/// </summary>
	public class FrameworkElementActivationStrategy : BuilderStrategy
	{
		public override object BuildUp(IBuilderContext context, Type typeToBuild, object existing, string idToBuild)
		{
			FrameworkElement frameworkElement = existing as FrameworkElement;

			if (frameworkElement != null)
			{
				WorkItem workItem = context.Locator.Get<WorkItem>(new DependencyResolutionLocatorKey(typeof(WorkItem), null));
				workItem.Services.Get<IFrameworkElementActivationService>(true).FrameworkElementAdded(frameworkElement);
			}

			return base.BuildUp(context, typeToBuild, existing, idToBuild);
		}

		public override object TearDown(IBuilderContext context, object item)
		{
			FrameworkElement frameworkElement = item as FrameworkElement;

			if (frameworkElement != null)
			{
				WorkItem workItem = context.Locator.Get<WorkItem>(new DependencyResolutionLocatorKey(typeof(WorkItem), null));
				workItem.Services.Get<IFrameworkElementActivationService>(true).FrameworkElementRemoved(frameworkElement);
			}

			return base.TearDown(context, item);
		}
	}
}
