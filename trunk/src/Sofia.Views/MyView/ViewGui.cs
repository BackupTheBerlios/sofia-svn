
using System;
using Sofia.Core;

namespace Sofia.Views.MyView
{
	
	public class ViewGui : BaseView
	{
		
		public ViewGui () : base ("gui.glade", "ViewGui")
		{
			
		}
		
		public override string Caption { get { return "Vue test"; } }
	}
	
}