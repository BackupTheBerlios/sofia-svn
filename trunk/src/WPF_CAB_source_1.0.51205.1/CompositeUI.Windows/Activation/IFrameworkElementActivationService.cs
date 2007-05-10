using System.Windows;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// An activation service for <see cref="FrameworkElement"/>s.
	/// </summary>
	public interface IFrameworkElementActivationService
	{
		void FrameworkElementAdded(FrameworkElement frameworkElement);

		void FrameworkElementRemoved(FrameworkElement frameworkElement);
	}
}