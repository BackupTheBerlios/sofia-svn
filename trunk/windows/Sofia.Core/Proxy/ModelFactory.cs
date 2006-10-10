
using System;
using com.db4o;

namespace Sofia.Core
{
	
	public class ModelFactory
	{
		string modelName;
		
		public ModelFactory(string modelName)
		{
			this.modelName = modelName;
		}
		
		public ObjectSet SendRequest(string request)
   		{
 			AppDomain domain = AppDomain.CreateDomain("ModelDomain");
 			try
 			{
				Proxy proxy = (Proxy) domain.CreateInstanceAndUnwrap("Proxy", "Sofia.Core.Proxy");
				ModelRequest modelRequest = proxy.LoadModel(modelName + ".dll", request);
				return modelRequest.Result;
			}
			finally
			{
				AppDomain.Unload(domain);
			}
   		}
	}
	
}
