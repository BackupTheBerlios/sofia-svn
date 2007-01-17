using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Data;
using System.Data.Common;

namespace Sofia.Data.Common
{
    #region DDL attributes

    /// <summary>
    /// Marks the field as primary key
    /// </summary>
    [AttributeUsage(AttributeTargets.Field)]
    public class PrimaryKeyAttribute : Attribute
    {
        /// <summary>
        /// Initialize a new instance of the class
        /// </summary>
        public PrimaryKeyAttribute()
        {
        }
    }

    /// <summary>
    /// Marks the fiels as sized field and sets its size.
    /// </summary>
    [AttributeUsage(AttributeTargets.Field)]
    public class FieldSizeAttribute : Attribute
    {
        /// <summary>
        /// Initialize a new instance of the class.
        /// </summary>
        /// <param name="size">The field size.</param>
        public FieldSizeAttribute(int size)
        {
            _Size = size;
        }

        /// <summary>
        /// The field size.
        /// </summary>
        private int _Size;

        /// <summary>
        /// Gets the field size.
        /// </summary>
        public int Size
        {
            get
            {
                return _Size;
            }
        }

    }

    #endregion

    #region Metadata update attributes

    /*
    [AttributeUsage(AttributeTargets.Field | AttributeTargets.Class)]
    public class UpdatedAttribute : Attribute
    {
        public enum UpdateOperation
        {
            Renamed,
            Removed
        }

        public UpdatedAttribute(string[] oldNames, UpdateOperation operation)
        {
            _Operation = operation;
            _OldNames = oldNames;
        }

        /// <summary>
        /// Constructeur
        /// </summary>
        /// <param name="oldTypes">Hashtable composé du type et de sa taille</param>
        public UpdatedAttribute(DbTypeInfo[] oldTypes)
        {
            _OldTypes = oldTypes;
        }

        private DbTypeInfo[] _OldTypes;
        private string[] _OldNames;

        public DbTypeInfo[] OldTypes
        {
            get
            {
                return _OldTypes;
            }
        }

        public string[] OldNames
        {
            get
            {
                return _OldNames;
            }
        }

        private UpdateOperation _Operation;
        public UpdateOperation Operation
        {
            get { return _Operation; }
            set { _Operation = value; }
        }

    }
    */

    #endregion

    #region Field type classes

    /// <summary>
    /// Represents a database field.
    /// </summary>
    public class DbField
    {
        #region Private fields
        /// <summary>
        /// The field name.
        /// </summary>
        private string _Name;
        /// <summary>
        /// The field value.
        /// </summary>
        private object _Value;
        /// <summary>
        /// Indicates the field is used in WHERE clause.
        /// </summary>
        private bool _IsFiltered;
        /// <summary>
        /// Indicates the field is used in a ORDER BY clause. Holds the sort direction.
        /// </summary>
        private SqlSortDirection _SortDirection;
        /// <summary>
        /// Holds the underlying database type.
        /// </summary>
        private DbType _DbType;
        #endregion

        #region Public properties
        /// <summary>
        /// Gets or sets the field name.
        /// </summary>
        public string Name
        {
            get { return _Name; }
            set { _Name = value; }
        }
        /// <summary>
        /// Gets or sets the underlying database type.
        /// </summary>
        public DbType DbType
        {
            get { return _DbType; }
            set { _DbType = value; }
        }
        /// <summary>
        /// Gets or sets the field value.
        /// </summary>
        public object Value
        {
            get { return _Value; }
            set { _Value = value; }
        }
        /// <summary>
        /// Gets or sets the field's presence in the WHERE clause.
        /// </summary>
        public bool Filtered
        {
            get { return _IsFiltered; }
            set { _IsFiltered = value; }
        }
        /// <summary>
        /// Gets or sets the field's presence in the ORDER BY clause. Gets or sets its sort direction.
        /// </summary>
        public SqlSortDirection SortDirection
        {
            get { return _SortDirection; }
            set { _SortDirection = value; }
        }
        #endregion

        #region Méthodes publiques
        /// <summary>
        /// Initialize a new instance of the class.
        /// </summary>
        /// <param name="name">The field name</param>
        public DbField(string name)
        {
            _Name = name;
        }
        /// <summary>
        /// Default constructor
        /// </summary>
        public DbField() : this("") { }
        /// <summary>
        /// Sets null to field value.
        /// </summary>
        public void Clear()
        {
            _Value = null;
        }
        /// <summary>
        /// Indicates if th field value is undefined
        /// </summary>
        /// <returns>True if the value is not assigned, else false.</returns>
        public bool IsEmpty()
        {
            return _Value == null;
        }
        #endregion
    }

    /// <summary>
    /// Represents a int32 field.
    /// </summary>
    public class DbInt32Field : DbField
    {
        /// <summary>
        /// Holds the int32 value.
        /// </summary>
        private int _Value;
        /// <summary>
        /// Gets or sets the int32 field value.
        /// </summary>
        public new int Value
        {
            get { return _Value; }
            set
            {
                base.Value = value;
                _Value = value;
            }
        }
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.Int32.
        /// </summary>
        public DbInt32Field()
        {
            DbType = DbType.Int32;
        }
    }

    /// <summary>
    /// Represents a int64 field.
    /// </summary>
    public class DbInt64Field : DbField
    {
        /// <summary>
        /// Holds the int64 value.
        /// </summary>
        private long _Value;
        /// <summary>
        /// Gets or sets the int64 field value.
        /// </summary>
        public new long Value
        {
            get { return _Value; }
            set
            {
                base.Value = value;
                _Value = value;
            }
        }
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.Int64.
        /// </summary>
        public DbInt64Field()
        {
            DbType = DbType.Int64;
        }
    }

    /// <summary>
    /// Represents a DateTime field.
    /// </summary>
    public class DbDateTimeField : DbField
    {
        /// <summary>
        /// Holds the DateTime field value.
        /// </summary>
        private DateTime _Value;
        /// <summary>
        /// Gets or sets the DateTime field value.
        /// </summary>
        public new DateTime Value
        {
            get { return _Value; }
            set
            {
                base.Value = value;
                _Value = value;
            }
        }
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.DateTime.
        /// </summary>
        public DbDateTimeField()
        {
            DbType = DbType.DateTime;
        }
    }

    /// <summary>
    /// Represents a string field.
    /// </summary>
    public class DbStringField : DbField
    {
        /// <summary>
        /// Holds the field size.
        /// </summary>
        private int _Size;
        /// <summary>
        /// Gets or sets the field size.
        /// </summary>
        public int Size
        {
            get { return _Size; }
            set { _Size = value; }
        }
        /// <summary>
        /// Holds the string field value.
        /// </summary>
        private string _Value;
        /// <summary>
        /// Gets or sets the string field value. Crop value if value size is greater than the field size.
        /// </summary>
        public new string Value
        {
            get
            {
                return _Value;
            }
            set
            {
                // todo : lever l'exception si Value.Lengyh > Size
                // todo : retourner false si Value.Lengyh > Size
                _Value = value;
                if (Size != -1)
                    if (value.Length > Size)
                        _Value = value.Remove(Size);
                base.Value = _Value;
            }
        }
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.String.
        /// </summary>
        public DbStringField()
        {
            DbType = DbType.String;
        }
    }
    /// <summary>
    /// Represents a text blob field.
    /// </summary>
    public class DbTextField : DbStringField
    {
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.String.
        /// Sets the size to -1.
        /// </summary>
        public DbTextField()
        {
            DbType = DbType.String;
            Size = -1;
        }
    }
    #endregion

    /// <summary>
    /// Represents a database table.
    /// </summary>
    public abstract class EntityBase
    {
        #region Private fields
        /// <summary>
        /// Holds the database connexion object.
        /// </summary>
        private Server _Server;
        private DbDataReader _DbDataReader;
        private List<SqlJoin> _Joins;

        #endregion

        #region Propriétés

        /// <summary>
        /// Obtient le nom du type
        /// </summary>
        public string Name
        {
            get
            {
                return this.GetType().Name;
            }
        }

        /// <summary>
        /// Obtient l'objet de connexion
        /// </summary>
        public Server Server
        {
            get { return _Server; }
        }

        #endregion

        #region Contructeur

        /// <summary>
        /// Constructeur
        /// </summary>
        /// <param name="server">Objet de connexion à la base de données</param>
        public EntityBase(Server server)
        {
            _Server = server;
            _Joins = new List<SqlJoin>();

            foreach (FieldInfo fieldInfo in GetFields())
            {
                //Instanciation de la propriété                
                DbField fieldInstance = (DbField)System.Activator.CreateInstance(fieldInfo.FieldType);
                fieldInstance.Name = fieldInfo.Name;
                fieldInfo.SetValue(this, fieldInstance);

                //Initialisation de l'objet
                fieldInstance.Filtered = false;
                fieldInstance.Value = null;

                if (fieldInfo.FieldType == typeof(DbStringField))
                    (fieldInstance as DbStringField).Size = GetDbTypeSize(fieldInfo);
            }

            //Mise à jour dans la base de données
            EntityUpdater entityUpdater = new EntityUpdater(this);
            entityUpdater.Check();
        }

        #endregion

        #region Réinitialisation de propriétés

        /// <summary>
        /// Réinitialise le tri
        /// </summary>
        public void ResetSort()
        {
            GetFields().ForEach(ActionResetSort);
        }

        /// <summary>
        /// Réinitialise la valeur de tous les champs
        /// </summary>
        public void FlushFields()
        {
            foreach (FieldInfo fieldInfo in GetFields())
            {
                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;
                field.Value = null;
            }

        }

        /// <summary>
        /// Réinitialise tous la propriété Filtered de tous les champs
        /// </summary>
        public void ResetFilters()
        {
            foreach (FieldInfo fieldInfo in GetFields())
            {
                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;
                field.Filtered = false;
            }

        }

        /// <summary>
        /// Délégué sur une classe générique Action pour le type FieldInfo qui a pour effet d'enlever le tri sur un champ
        /// </summary>
        /// <param name="FieldInfo"></param>
        private void ActionResetSort(FieldInfo fieldInfo)
        {
            object fieldValue = fieldInfo.GetValue(this);
            DbField field = (DbField)fieldValue;
            field.SortDirection = SqlSortDirection.None;
        }

        #endregion

        #region Lecture des enregistrements

        /// <summary>
        /// Détermine si le nom d'une colonne est présente dans le reader
        /// </summary>
        /// <param name="field"></param>
        private bool ReaderHasRow(DbField field)
        {
            try
            {
                _DbDataReader.GetOrdinal(field.Name);
                return true;
            }
            catch (IndexOutOfRangeException)
            {
                return false;
            }
        }

        /// <summary>
        /// Lecture des enregistrements résultant de la dernière requête
        /// </summary>
        /// <returns></returns>
        public bool Read()
        {
            bool hasRecords;
            hasRecords = _DbDataReader.Read();

            if (hasRecords)
            {
                foreach (FieldInfo fieldInfo in GetFields())
                {
                    object fieldValue = fieldInfo.GetValue(this);
                    DbField field = (DbField)fieldValue;
                    if (ReaderHasRow(field))
                    {
                        field.Value = _DbDataReader[field.Name];
                    }
                }

                //TODO : DataHistory.add(this);
            }

            return hasRecords;
        }

        #endregion

        #region Methodes bas niveau pour exécution des requêtes

        /// <summary>
        /// ExecuteReader sur un objet FbQuery initialisé
        /// </summary>
        /// <param name="fbQuery">objet FbQuery initialisé</param>
        protected bool ExecuteReader(Query fbQuery)
        {
            if (_DbDataReader != null)
                _DbDataReader.Close();

            //Exécution de la requête
            _DbDataReader = fbQuery.ExecuteReader();


            if (_DbDataReader == null)
            {
                return false;
            }

            //Affectation des propriétés
            return Read();
        }

        /// <summary>
        /// ExecuteNonQuery sur un objet FbQuery initialisé
        /// </summary>
        /// <param name="fbQuery">objet FbQuery initialisé</param>
        protected bool ExecuteNonQuery(Query fbQuery)
        {
            //Exécution de la requête
            if (!fbQuery.ExecuteNonQuery())
            {
                return false;
            }
            else
                return true;
        }

        #endregion

        #region Recherche de champs

        /// <summary>
        /// Delegate de prédicat portant sur les propriétés de type Field
        /// </summary>
        /// <param name="property">Instance de la propriété Field</param>
        /// <returns>Vrai si la propriété Field.Filtered = true</returns>
        private bool IsFilteredField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return field.Filtered;
        }

        /// <summary>
        /// Delegate de prédicat portant sur les propriétés de type Field
        /// </summary>
        /// <param name="property">Instance de la propriété Field</param>
        /// <returns>Vrai si la propriété Field.IsPrimaryKey = true</returns>
        private bool IsPrimaryKeyField(FieldInfo fieldInfo)
        {
            foreach (object attribute in fieldInfo.GetCustomAttributes(true))
            {
                if (attribute is PrimaryKeyAttribute)
                {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Delegate de prédicat portant sur les propriétés de type Field
        /// </summary>
        /// <param name="property">Instance de la propriété Field</param>
        /// <returns>Vrai si la propriété Field.SortDirection != None</returns>
        private bool IsSortedField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return field.SortDirection != SqlSortDirection.None;
        }

        /// <summary>
        /// Delegate de prédicat portant sur les propriétés de type Field
        /// </summary>
        /// <param name="property">Instance de la propriété Field</param>
        /// <returns>Vrai si la propriété Field.Value != null</returns>
        private bool IsAffectedField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return field.Value != null;
        }

        /// <summary>
        /// Delegate de prédicat portant sur les propriétés de type Field
        /// </summary>
        /// <param name="property">Instance de la propriété Field</param>
        /// <returns>Vrai si la propriété Field.Value != null</returns>
        private bool IsAffectedNonPrimaryField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return (field.Value != null && !IsPrimaryKeyField(fieldInfo));
        }

        /// <summary>
        /// Obtient de la liste des propriétés publiques
        /// </summary>
        /// <returns>Une liste générique des propriétés publiques</returns>
        private List<FieldInfo> GetFields()
        {
            return new List<FieldInfo>(this.GetType().GetFields());
        }

        /// <summary>
        /// Retourne le champ qui a la propriété Name
        /// </summary>
        /// <param name="name">Nom du champ</param>
        /// <returns>Un champ ou nul si le champ n'est pas trouvé</returns>
        private FieldInfo GetField(string name)
        {
            List<FieldInfo> fields = new List<FieldInfo>(this.GetType().GetFields());
            return fields.Find(delegate(FieldInfo field) { return field.Name == name; });
        }

        #endregion

        #region Méthodes DML frontales

        /// <summary>
        /// Initialise les champs de l'entité avec les valeurs chargées depuis la base de données.
        /// La clause WHERE se base sur la valeur de la clé primaire définie et initialisée pour l'entité 
        /// </summary>
        public bool Fill()
        {
            return BuildQuery(GetDefaultSelect, IsPrimaryKeyField, false);
        }

        /// <summary>
        /// Initialise les champs de l'entité avec les valeurs chargées depuis la base de données.
        /// La clause WHERE se base sur la valeur des champs passés en paramètre
        /// </summary>
        /// <param name="filteredFields">Liste des champs initialisés avec une valeur</param>
        public bool Filter()
        {
            return BuildQuery(GetFilteredSelect, IsFilteredField, false);
        }

        /// <summary>
        /// 
        /// </summary>
        public bool Update()
        {
            //Déterminer si l'enregistrement est à ajouter ou à insérer en fonction de la valeur de la clé primaire
            if (Exists())
                return BuildQuery(GetDefaultUpdate, IsAffectedField, true);
            else
                return BuildQuery(GetDefaultInsert, IsAffectedField, true);
        }
        /// <summary>
        /// Insertion d'un enregistrement
        /// </summary>
        public bool Insert()
        {
            return BuildQuery(GetDefaultInsert, IsAffectedField, true);
        }
        /// <summary>
        /// Permet de déterminer si un enregistrement existe en se basant sur sa clé primaire
        /// </summary>
        /// <returns></returns>
        public bool Exists()
        {
            return BuildQuery(GetPrimarySelect, IsPrimaryKeyField, false);
        }

        /// <summary>
        /// Suppression de l'enregistrement dont la clé primaire est spécifiée
        /// </summary>
        public bool Delete()
        {
            return BuildQuery(GetDefaultDelete, IsPrimaryKeyField, true);
        }

        /// <summary>
        /// Création de la tabme
        /// </summary>
        public bool Create()
        {
            bool executionResult;

            executionResult = BuildQuery(GetDefaultCreate, null, true);
            executionResult = executionResult && BuildQuery(GetDefaultAddPrimaryConstraint, null, true);

            return executionResult;
        }


        #endregion

        #region Construction de liste de champs

        /// <summary>
        /// Delagate permettant de transformer une chaîne de caractères
        /// </summary>
        /// <param name="value">La chaine à transformer</param>
        /// <returns>La chaine tranformée</returns>
        private delegate string FieldTransformation(DbField field);

        /// <summary>
        /// Transforme un champ en un champ paramétré
        /// <example>FIELD => @FIELD</example>
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private string TransformToParametizedField(DbField field)
        {
            return "@" + field.Name;
        }

        /// <summary>
        /// Transforme un champ en un champ présent dans une clause ORDER BY
        /// <example>FIELD => FIELD Asc</example>
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private string TransformToOrderedField(DbField field)
        {
            return field.Name + " " + Enum.GetName(typeof(SqlSortDirection), field.SortDirection);
        }

        /// <summary>
        /// Transforme un champ en un champ paramétré
        /// <example>FIELD => FIELD = @FIELD</example>
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private string TransformToParametizedAffectationField(DbField field)
        {
            return field.Name + " = @" + field.Name;
        }

        /// <summary>
        /// Transforme un champ en un champ suivi de son type
        /// <example>FIELD => FIELD VARCHAR(32)</example>
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private string TransformToTypedField(DbField field)
        {
            //Détermination de la chaine représentant le type du champ
            FieldInfo fieldInfo = GetField(field.Name);

            if (fieldInfo != null)
            {
                string type = _Server.SgbdDDL.GetDDLType(field.DbType, GetDbTypeSize(fieldInfo), IsPrimaryKeyField(fieldInfo));
                return String.Format("{0} {1}", field.Name, type);
            }
            else return "";
        }


        /// <summary>
        /// Sélection d'une liste de propriétés pour construire une liste de champs SQL en fonction d'une transformation
        /// </summary>
        /// <param name="fieldFilter">Délégué qui définit les propriétés à rechercher</param>
        /// <param name="stringTransformation">Délégué qui spécifie une transformation à appliquer au nom de chaque propriété</param>
        /// <param name="separator">Séparateur</param>
        /// <returns>Une liste de nom de champs séparés par le séparateur</returns>
        private string GetFieldList(Predicate<FieldInfo> match, FieldTransformation transformation, string separator)
        {
            string fieldList = "";
            int fieldIndex = 0;
            List<FieldInfo> fields;

            //Filtre sur les propriétés
            if (match != null)
                fields = GetFields().FindAll(match);
            else
                fields = GetFields();


            //Parcours des propriétés filtrées
            foreach (FieldInfo fieldInfo in fields)
            {
                string fieldName;

                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;

                //Application d'une transformation sur le nom de la propriété
                if (transformation != null)
                    fieldName = transformation(field);
                else
                    fieldName = fieldInfo.Name;

                //Construction de la chaine SQL
                if (fieldIndex == 0)
                    fieldList = fieldName;
                else
                    fieldList = fieldList + separator + fieldName;

                fieldIndex++;
            }

            return fieldList;

        }

        private string GetFieldList(Predicate<FieldInfo> match, FieldTransformation transformation)
        {
            return GetFieldList(match, transformation, ",");
        }

        private string GetFieldList(Predicate<FieldInfo> match)
        {
            return GetFieldList(match, null, ",");
        }

        private string GetFieldList()
        {
            return GetFieldList(null, null, ",");
        }

        #endregion

        #region Contruction des clauses SQL

        private delegate string BuildSqlClauseDelegate();

        /// <summary>
        /// Génération de la requête SELECT en se basant sur la clé primaire pour la constitution de la clause WHERE
        /// </summary>
        /// <returns></returns>
        private string GetDefaultSelect()
        {
            //Liste des champs
            string fieldList = GetFieldList();

            //Liste des clé primaires
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, " AND ");

            //Liste de tri
            string sortedList = GetFieldList(IsSortedField, TransformToOrderedField);
            if (sortedList.Length != 0)
                sortedList = "ORDDER BY " + sortedList;

            //Construction de la chaine SQL
            return string.Format("SELECT {0} FROM {1} WHERE {2} {3}", fieldList, Name, whereClause, sortedList);

        }

        /// <summary>
        /// Génération d'une requête SELECT ne prenant en compte que la clé primaire. Sert pour vérifier l'existance d'un enregistrement.
        /// </summary>
        /// <returns></returns>
        private string GetPrimarySelect()
        {
            //Liste des champs
            string fieldList = GetFieldList(IsPrimaryKeyField);

            //Liste des clé primaires
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, " AND ");

            //Construction de la chaine SQL
            return string.Format("SELECT {0} FROM {1} WHERE {2}", fieldList, Name, whereClause);

        }

        /// <summary>
        /// Génération de la requête SELECT en se basant sur les champs filtrés pour la constitution de la clause WHERE
        /// </summary>
        /// <returns></returns>
        private string GetFilteredSelect()
        {
            //Liste des champs
            string fieldList = GetFieldList();

            //Liste des champs filtrés
            string whereClause = GetFieldList(IsFilteredField, TransformToParametizedAffectationField, " AND ");
            if (whereClause.Length > 0)
                whereClause = "WHERE " + whereClause;

            //Liste de tri
            string sortedList = GetFieldList(IsSortedField, TransformToOrderedField);
            if (sortedList.Length != 0)
                sortedList = "ORDER BY " + sortedList;

            //Construction de la chaine SQL
            return string.Format("SELECT {0} FROM {1} {2} {3}", fieldList, Name, whereClause, sortedList);

        }

        /// <summary>
        /// Génération de la requête INSERT
        /// <param name="ignorePrimaryKey">L'initialisation de la chaine se fait sans la clé primaire si ce paramètre est true</param>
        /// </summary>
        /// <returns></returns>
        private string GetDefaultInsert()
        {
            //Liste des champs
            string fieldList = GetFieldList(IsAffectedField);
            string parameterList = GetFieldList(IsAffectedField, TransformToParametizedField);

            //Construction de la chaine SQL
            return string.Format("INSERT INTO {0} ({1}) VALUES ({2})", Name, fieldList, parameterList);
        }

        /// <summary>
        /// Génération de la requête UPDATE        
        /// </summary>
        /// <returns></returns>
        private string GetDefaultUpdate()
        {
            //Liste des champs
            string fieldList = GetFieldList(IsAffectedNonPrimaryField, TransformToParametizedAffectationField);

            if (fieldList.Length == 0)
                return "";

            //Liste des clé primaires
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, " AND ");

            //Construction de la chaine SQL
            return string.Format("UPDATE {0} SET {1} WHERE {2}", Name, fieldList, whereClause);
        }

        /// <summary>
        /// Génération de la requête DELETE
        /// </summary>
        /// <returns></returns>
        private string GetDefaultDelete()
        {
            //Liste des parametres
            string parameterList = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, " AND ");

            //Construction de la chaine SQL
            return string.Format("DELETE FROM {0} WHERE {1}", Name, parameterList);
        }

        /// <summary>
        /// Génération de la requête CREATE TABLE
        /// </summary>
        /// <returns></returns>
        private string GetDefaultCreate()
        {
            //Liste des champs
            string fieldList = GetFieldList(null, TransformToTypedField);
            string pkFields = GetFieldList(IsPrimaryKeyField);

            //Construction de la chaine SQL
            return string.Format("CREATE TABLE {0} ({1})", Name, fieldList);
        }

        /// <summary>
        /// Génération de la requête ALTER TABLE
        /// </summary>
        /// <returns></returns>
        private string GetDefaultAddPrimaryConstraint()
        {
            //Liste des champs
            string fieldList = GetFieldList(IsPrimaryKeyField);

            //Construction de la chaine SQL
            return string.Format("ALTER TABLE {0} ADD CONSTRAINT PK_{0} PRIMARY KEY ({1});", Name, fieldList);
        }


        /// <summary>
        /// Construction de la chaîne de requête
        /// </summary>
        /// <param name="buildSqlClauseDelegate"></param>
        /// <param name="parametersFillingPredicate"></param>
        /// <param name="nonQuery"></param>
        /// <returns></returns>
        private bool BuildQuery(BuildSqlClauseDelegate buildSqlClauseDelegate,
            Predicate<FieldInfo> parametersFillingPredicate, bool nonQuery)
        {
            //Instanciation de la requête
            Query fbQuery = new Query(_Server);
            fbQuery.CommandText = buildSqlClauseDelegate();

            if (fbQuery.CommandText.Length == 0)
                return true;

            //Affectation des paramètres
            FillParameters(fbQuery, parametersFillingPredicate);


            //Execution de la requête
            _Server.OpenConnection();
            try
            {
                if (nonQuery)
                    return ExecuteNonQuery(fbQuery);
                else
                    return ExecuteReader(fbQuery);
            }
            finally
            {
                _Server.CloseConnexion();
            }
        }

        #endregion

        #region Initialisation des paramètres

        /// <summary>
        /// Initialisation des paramètres de la requête en fonction des champs
        /// </summary>
        /// <param name="fbQuery">La requête</param>
        /// <param name="match">Ne traite que les propriétés qui répondent à ce prédicat</param>
        private void FillParameters(Query fbQuery, Predicate<FieldInfo> match)
        {
            fbQuery.FlushParameters();
            List<FieldInfo> fields;

            if (match != null)
                fields = GetFields().FindAll(match);
            else
                return;

            foreach (FieldInfo fieldInfo in fields)
            {
                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;
                DbType dbType = field.DbType;
                fbQuery.AddParameter(field.Name, dbType, field.Value);
            }
        }

        /// <summary>
        /// Permet d'obtenir la taille du type de données associé au champ
        /// </summary>
        /// <param name="fieldInfo">Le champ</param>
        /// <returns>Un entier représentant la taille du type de donnée</returns>
        private int GetDbTypeSize(FieldInfo fieldInfo)
        {
            object[] attributes = fieldInfo.GetCustomAttributes(typeof(FieldSizeAttribute), true);
            if (attributes.Length != 0)
            {
                FieldSizeAttribute attribute = attributes[0] as FieldSizeAttribute;
                return attribute.Size;
            }
            else
            {
                return -1;
            }
        }

        #endregion

    }

    /// <summary>
    /// Classe représentant une jointure
    /// </summary>
    public class SqlJoin
    {
        #region Champs privés

        private EntityBase _Entity;
        private DbFieldMapping _FieldMapping;

        #endregion

        #region Propriétés

        /// <summary>
        /// Entité sur laquelle porte la jointure
        /// </summary>
        public EntityBase Entity
        {
            get { return _Entity; }
            set { _Entity = value; }
        }

        /// <summary>
        /// Mapping du champ
        /// </summary>
        public DbFieldMapping FieldMapping
        {
            get { return _FieldMapping; }
            set { _FieldMapping = value; }
        }

        #endregion

        #region Méthodes publiques

        /// <summary>
        /// Constructeur
        /// </summary>
        public SqlJoin(EntityBase entity, DbField leftField, DbField rightField)
        {
            _Entity = entity;
            _FieldMapping = new DbFieldMapping(leftField, rightField);
        }

        #endregion
    }

    /// <summary>
    /// Classe représentant une correspondance entre deux champs d'une entité
    /// </summary>
    public class DbFieldMapping
    {
        #region Champs privés

        private DbField _LeftField;
        private DbField _RightField;

        #endregion

        #region Propriétés

        public DbField LeftField
        {
            get { return _LeftField; }
            set { _LeftField = value; }
        }

        public DbField RightField
        {
            get { return _RightField; }
            set { _RightField = value; }
        }

        #endregion

        #region Méthodes publiques

        public DbFieldMapping(DbField leftField, DbField rightField)
        {
            _LeftField = leftField;
            _RightField = rightField;
        }

        #endregion
    }

    public enum SqlSortDirection
    {
        None,
        Asc,
        Desc
    }
}

