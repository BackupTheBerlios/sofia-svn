using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using Microsoft.Practices.CompositeUI.Commands;
using Microsoft.Practices.CompositeUI.UIElements;
using Microsoft.Practices.CompositeUI.Windows.Activation;
using Microsoft.Practices.CompositeUI.Windows.BuilderStrategies;
using Microsoft.Practices.CompositeUI.Windows.UIElements;
using Microsoft.Practices.CompositeUI.Windows.Visualizer;
using Microsoft.Practices.ObjectBuilder;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// Extends <see cref="CabShellApplication{TWorkItem,TShell}"/> to support shell-based applications that use Windows
	/// Presentation Foundation.
	/// </summary>
	/// <typeparam name="TWorkItem">
	/// The type of the root application work item.
	/// </typeparam>
	/// <typeparam name="TShell">
	/// The type for the shell to use.
	/// </typeparam>
	public abstract class WindowsApplication<TWorkItem, TShell> : CabShellApplication<TWorkItem, TShell>
		where TWorkItem : WorkItem, new()
	{
		/// <summary>
		/// Initializes an instance of the <see cref="WindowsApplication{TWorkItem,TShell}"/> class.
		/// </summary>
		protected WindowsApplication()
		{
			VisualizerType = typeof(WindowsVisualizer);
		}

		/// <summary>
		/// Adds Windows specific strategies to the builder.
		/// </summary>
		protected override void AddBuilderStrategies(Builder builder)
		{
			builder.Strategies.AddNew<WindowsServiceStrategy>(BuilderStage.Initialization);
			builder.Strategies.AddNew<FrameworkElementActivationStrategy>(BuilderStage.Initialization);
			builder.Strategies.AddNew<FrameworkElementSmartPartStrategy>(BuilderStage.Initialization);
		}

		/// <summary>
		/// See <see cref="CabApplication{TWorkItem}.AddServices"/>
		/// </summary>
		protected override void AddServices()
		{
			RootWorkItem.Services.AddNew<FrameworkElementActivationService, IFrameworkElementActivationService>();
		}

		/// <summary>
		/// See <see cref="CabShellApplication{TWorkItem,TShell}.AfterShellCreated"/>
		/// </summary>
		protected override void AfterShellCreated()
		{
			RegisterUIElementAdapterFactories();
			RegisterCommandAdapters();
		}

		private void RegisterCommandAdapters()
		{
			ICommandAdapterMapService mapService = RootWorkItem.Services.Get<ICommandAdapterMapService>();
			mapService.Register(typeof(FrameworkElement), typeof(FrameworkElementCommandAdapter));
		}

		private void RegisterUIElementAdapterFactories()
		{
			IUIElementAdapterFactoryCatalog catalog = RootWorkItem.Services.Get<IUIElementAdapterFactoryCatalog>();
			catalog.RegisterFactory(new ItemsControlUIAdapterFactory());
		}
	}
}
