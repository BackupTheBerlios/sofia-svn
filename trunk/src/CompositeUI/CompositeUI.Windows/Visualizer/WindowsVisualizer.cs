using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Practices.ObjectBuilder;
using Microsoft.Practices.CompositeUI.Windows.Activation;
using Microsoft.Practices.CompositeUI.Windows.BuilderStrategies;

namespace Microsoft.Practices.CompositeUI.Windows.Visualizer
{
	public class WindowsVisualizer : CabVisualizer
	{
		protected override void AddBuilderStrategies(Builder builder)
		{
			builder.Strategies.AddNew<WindowsServiceStrategy>(BuilderStage.Initialization);
			builder.Strategies.AddNew<FrameworkElementActivationStrategy>(BuilderStage.Initialization);
			builder.Strategies.AddNew<FrameworkElementSmartPartStrategy>(BuilderStage.Initialization);
		}

		protected override void AddServices()
		{
			RootWorkItem.Services.AddNew<FrameworkElementActivationService, IFrameworkElementActivationService>();
		}

		protected override sealed void CreateVisualizationShell()
		{
			RootWorkItem.Items.AddNew<VisualizerWindow>("VisualizerWindow").Show();
		}
	}
}
