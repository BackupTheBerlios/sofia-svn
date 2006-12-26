
using System;
using System.Collections;
using System.Reflection;

namespace Sofia.Core
{
	
	public class Proxy: MarshalByRefObject
	{		
		public ModelRequest LoadModel(string assemblyFilename, string request)
		{
			Assembly Dynamic = Assembly.LoadFrom(assemblyFilename);
			
			Type TypeofIModel = typeof(IModel);
			foreach (Type Exported in Dynamic.GetExportedTypes())
				if (TypeofIModel.IsAssignableFrom(Exported))
				{
					ConstructorInfo Constructor = Exported.GetConstructor(Type.EmptyTypes);
					if (Constructor != null)
					{
						IModel instance = (IModel) Constructor.Invoke(null);
						ModelRequest modelRequest = new ModelRequest(instance, request);
						return modelRequest;
					}
				}
			// if no matches
			return null;

		}
	}

	[Serializable()]
	public class ModelRequest
	{	
		private IList _Result;

		public ModelRequest(IModel model, string request)
		{
			_Result = model.SendRequest(request);
		}
		
		public IList Result { get {	return _Result; } }
		
	}

}
