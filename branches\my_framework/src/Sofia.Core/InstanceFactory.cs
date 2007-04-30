using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

namespace Sofia.Reflection
{
    class InstanceFactory
    {
        /// <summary>
        /// Invoque le constructeur sans paramètres d'un type d'une assembly
        /// </summary>
        /// <param name="assemblyFile">Chemin complet de l'assembly</param>
        /// <param name="type">Type exporté par l'assembly</param>
        /// <returns>L'instance de l'objet</returns>
        public static object CreateInstanceFrom(string assemblyFile, Type type, object[] parameters, Type[] ctorTypes)
        {
            //Chargement de l'assembly
            Assembly assembly = Assembly.LoadFrom(assemblyFile);

            //Appel du constructeur
            Type[] types = assembly.GetExportedTypes();
            foreach (Type exported in types)
            {
                if (type.IsAssignableFrom(exported))
                {
                    ConstructorInfo Constructor = exported.GetConstructor(ctorTypes);
                    if (Constructor != null)
                        return Constructor.Invoke(parameters);
                }
            }
            return null;

        }
    }
}
