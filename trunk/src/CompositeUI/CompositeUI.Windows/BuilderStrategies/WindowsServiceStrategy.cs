using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Practices.ObjectBuilder;
using Microsoft.Practices.CompositeUI.Windows.Activation;

namespace Microsoft.Practices.CompositeUI.Windows.BuilderStrategies
{
	/// <summary>
	/// Implements a <see cref="BuilderStrategy"/> to ensure work items have the correct services added to them.
	/// </summary>
	public class WindowsServiceStrategy : BuilderStrategy
	{
		public override object BuildUp(IBuilderContext context, Type typeToBuild, object existing, string idToBuild)
		{
			WorkItem workItem = existing as WorkItem;

			if (workItem != null && !workItem.Services.ContainsLocal(typeof(IFrameworkElementActivationService)))
			{
				workItem.Services.Add<IFrameworkElementActivationService>(new FrameworkElementActivationService());
			}

			return base.BuildUp(context, typeToBuild, existing, idToBuild);
		}
	}
}
