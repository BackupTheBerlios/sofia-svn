using System.Windows;

namespace Microsoft.Practices.CompositeUI.Windows
{
	/// <summary>
	/// Defines a service to deal with the <see cref="WorkItem"/> activation and deactivation based on its contained
	/// <see cref="UIElement"/> state.
	/// </summary>
	public interface IUIElementActivationService
	{
		/// <summary>
		/// Notifies that a <see cref="UIElement"/> has been added to the container.
		/// </summary>
		void UIElementAdded(UIElement uiElement);

		/// <summary>
		/// Notifies that a <see cref="UIElement"/> has been removed from the container.
		/// </summary>
		void UIElementRemoved(UIElement uiElement);
	}
}