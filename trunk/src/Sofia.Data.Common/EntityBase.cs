using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Data;
using System.Data.Common;

namespace Sofia.Data.Common
{
    #region EntityBase class
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
        #endregion
        #region Public properties
        /// <summary>
        /// Gets the type name.
        /// </summary>
        public string Name
        {
            get
            {
                return this.GetType().Name;
            }
        }
        /// <summary>
        /// Gets the database connexion object.
        /// </summary>
        public Server Server
        {
            get
            {
                return _Server;
            }
        }
        #endregion
        #region Public methods
        #region Initialization
        /// <summary>
        /// Initialize a new instance of the class.
        /// </summary>
        /// <param name="server">Database connexion object</param>
        public EntityBase(Server server)
        {
            _Server = server;

            //Initialize fields instance based upon the public members in the derived class
            foreach (FieldInfo fieldInfo in GetFields())
            {
                //New field instance 
                DbField fieldInstance = (DbField)System.Activator.CreateInstance(fieldInfo.FieldType);
                fieldInstance.Name = fieldInfo.Name;
                fieldInfo.SetValue(this, fieldInstance);

                //Initialization of the field instance's properties
                fieldInstance.Filtered = false;
                fieldInstance.Value = null;
                if (fieldInfo.FieldType == typeof(DbStringField))
                    (fieldInstance as DbStringField).Size = GetDbTypeSize(fieldInfo);
            }
#if AUTOUPDATE            
            EntityUpdater entityUpdater = new EntityUpdater(this);
            entityUpdater.Check();
#endif
        }
        #endregion
        #region Field's properties reseting
        /// <summary>
        /// Resets the sort for each field
        /// </summary>
        public void ResetSort()
        {
            foreach (FieldInfo fieldInfo in GetFields())
            {
                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;
                field.SortDirection = SqlSortDirection.None;
            }
        }
        /// <summary>
        /// Resets each field value
        /// </summary>
        public void FlushFields()
        {
            foreach (FieldInfo fieldInfo in GetFields())
            {
                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;
                field.Clear();
            }
        }
        /// <summary>
        /// Resets the Filtered property of each field
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
        #endregion
        #region Reading
        /// <summary>
        /// Invoke the Read method of the internal DbDataReader and sets each field's value property
        /// with the content of the internal DbDataReader.
        /// </summary>
        /// <returns>True if any record can be read, else false.</returns>
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
        #region Frontend DML methods
        /// <summary>
        /// Indicates if a record already exists in the database table, based upon the primary key value.
        /// </summary>
        /// <returns>True if the record already exists, else false.</returns>
        public bool Exists()
        {
            return BuildQuery(GetPrimarySelect, IsPrimaryKeyField, false);
        }
        /// <summary>
        /// Fills the properties value with the result of a SELECT query.
        /// </summary>
        /// <returns>True if no errors, else false.</returns>
        public bool Fill()
        {
            return BuildQuery(GetDefaultSelect, IsPrimaryKeyField, false);
        }
        /// <summary>
        /// Fills the properties vallue with the result of a SELECT WHERE query. The WHERE clause is builded with the
        /// filtered fields.
        /// </summary>
        // <returns>True if no errors, else false.</returns>
        public bool Filter()
        {
            return BuildQuery(GetFilteredSelect, IsFilteredField, false);
        }
        /// <summary>
        /// Executes an INSERT query if the primary key value does not exists 
        /// in the database table, or an UPDATE query if the primary key value 
        /// already exists. The values updated in the database are picked up 
        /// from the fields assigned value property.
        /// </summary>
        /// <returns>True if no errors, else false.</returns>
        public bool Update()
        {
            if (Exists())
                return BuildQuery(GetDefaultUpdate, IsAffectedField, true);
            else
                return BuildQuery(GetDefaultInsert, IsAffectedField, true);
        }
        /// <summary>
        /// Executes an INSERT query. The values updated in the database are 
        /// picked up from the fields assigned value property.
        /// </summary>
        /// <returns>True if no errors, else false.</returns>
        public bool Insert()
        {
            return BuildQuery(GetDefaultInsert, IsAffectedField, true);
        }
        /// <summary>
        /// Deletes the database table record with the specified primary key value.
        /// </summary>
        /// <returns>True if no errors, else false.</returns>
        public bool Delete()
        {
            return BuildQuery(GetDefaultDelete, IsPrimaryKeyField, true);
        }
        /// <summary>
        /// Creates the database table with the name of the derived class name.
        /// </summary>
        /// <returns>True if no errors, else false.</returns>
        public bool Create()
        {
            bool result;
            try
            {
                result = BuildQuery(GetDefaultCreate, null, true);
                result = result && BuildQuery(GetDefaultAddPrimaryConstraint, null, true);

                return result;
            }
            catch (Exception)
            {
                return false;
            }
        }
        #endregion
        #endregion
        #region Private methods
        #region Fields property reflection methods
        /// <summary>
        /// Gets the size attributed to the specified field.
        /// </summary>
        /// <param name="fieldInfo">A field property info.</param>
        /// <returns>The integer representing the field's size.</returns>
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
        #region Reading
        /// <summary>
        /// Indicates a row exists in the internal DbDataReader, based on the field name.
        /// </summary>
        /// <param name="field">A field object.</param>
        /// <returns>True if the row exists, else false.</returns>
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
        #endregion
        #region Low-level querying methods
        /// <summary>
        /// Invokes the ExecuteReader method on the internal Query object. Initialize the internal
        /// DbDataReader object.
        /// </summary>
        /// <param name="query">A Query object.</param>
        /// <returns>True if no errors, else false.</returns>
        private bool ExecuteReader(Query query)
        {
            if (_DbDataReader != null)
                _DbDataReader.Close();

            //Query execution
            _DbDataReader = query.ExecuteReader();

            if (_DbDataReader == null)
                return false;

            //First DbDataReader Read in order to initialize field's value property
            return Read();
        }
        /// <summary>
        /// Invokes the ExecuteNonQuery method on the internal Query object.
        /// </summary>
        /// <param name="query">A Query object</param>
        /// <returns>True if no errors, else false.</returns>
        protected bool ExecuteNonQuery(Query query)
        {
            //Query execution
            return query.ExecuteNonQuery();
        }
        #endregion
        #region Field searching
        /// <summary>
        /// Predicate delegate based upon the field's Filtered property.
        /// </summary>
        /// <param name="fieldInfo">A Field property info.</param>
        /// <returns>True if the field is Filtered, else false.</returns>
        private bool IsFilteredField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return field.Filtered;
        }
        /// <summary>
        /// Predicate delegate based upon the field's PrimaryKey attribute.
        /// </summary>
        /// <param name="fieldInfo">A Field property info.</param>
        /// <returns>True if the field is marked as primary key, else false.</returns>
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
        /// Predicate delegate based upon the field's SortDirection property.
        /// </summary>
        /// <param name="fieldInfo">A Field property info.</param>
        /// <returns>True if the field is sorted, else false.</returns>
        private bool IsSortedField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return field.SortDirection != SqlSortDirection.None;
        }
        /// <summary>
        /// Predicate delegate based upon the field's value.
        /// </summary>
        /// <param name="fieldInfo">A Field property info.</param>
        /// <returns>True if the field value is assigned, else false.</returns>
        private bool IsAffectedField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return !field.IsEmpty();
        }
        /// <summary>
        /// Predicate delegate based upon the PrimaryKey attribute and the field's value
        /// </summary>
        /// <param name="fieldInfo">A Field property info.</param>
        /// <returns>True if the fied value is assigned and the field is a primary key, else false.</returns>
        private bool IsAffectedNonPrimaryField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return (field.Value != null && !IsPrimaryKeyField(fieldInfo));
        }
        /// <summary>
        /// Gets the derived class public member list.
        /// </summary>
        /// <returns>A generic list of FieldInfo objects.</returns>
        private List<FieldInfo> GetFields()
        {
            return new List<FieldInfo>(this.GetType().GetFields());
        }
        /// <summary>
        /// Gets a field with the specified name.
        /// </summary>
        /// <param name="name">Field name.</param>
        /// <returns>A field property info</returns>
        private FieldInfo GetField(string name)
        {
            List<FieldInfo> fields = new List<FieldInfo>(this.GetType().GetFields());
            return fields.Find(delegate(FieldInfo field) { return field.Name == name; });
        }
        #endregion
        #region String field list building
        /// <summary>
        /// Field name string transformation action delegate.
        /// </summary>
        /// <param name="field">A field object.</param>
        /// <returns>A string containing the field name transformed.</returns>
        private delegate string FieldTransformation(DbField field);
        /// <summary>
        /// Transforms the field to a parameterized field.
        /// <example>FIELD => @FIELD</example>
        /// </summary>
        /// <param name="field">A field object.</param>
        /// <returns>A string containing the field name transformed.</returns>
        private string TransformToParametizedField(DbField field)
        {
            return "@" + field.Name;
        }
        /// <summary>
        /// Transforms the field to an ordered field.
        /// <example>FIELD => FIELD Asc</example>
        /// </summary>
        /// <param name="field">A field object.</param>
        /// <returns>A string containing the field name transformed.</returns>
        private string TransformToOrderedField(DbField field)
        {
            return field.Name + " " + Enum.GetName(typeof(SqlSortDirection), field.SortDirection);
        }
        /// <summary>
        /// Transforms the field to a parameterized field with assignation.
        /// <example>FIELD => FIELD = @FIELD</example>
        /// </summary>
        /// <param name="field">A field object.</param>
        /// <returns>A string containing the field name transformed.</returns>
        private string TransformToParametizedAffectationField(DbField field)
        {
            return field.Name + " = @" + field.Name;
        }
        /// <summary>
        /// Transforms the field to a typed field.
        /// <example>FIELD => FIELD VARCHAR(32)</example>
        /// </summary>
        /// <param name="field">A field object.</param>
        /// <returns>A string containing the field name transformed.</returns>
        private string TransformToTypedField(DbField field)
        {
            FieldInfo fieldInfo = GetField(field.Name);

            if (fieldInfo != null)
            {
                string type = _Server.SgbdDDL.GetDDLType(field.DbType, GetDbTypeSize(fieldInfo), IsPrimaryKeyField(fieldInfo));
                return String.Format("{0} {1}", field.Name, type);
            }
            else return "";
        }
        /// <summary>
        /// Gets a string field list with the specified separator. The fields match the specified match 
        /// predicate delegate and their names are transformed by the specified 
        /// string transformation action delegate.
        /// </summary>
        /// <param name="match">A match predicate delegate.</param>
        /// <param name="transformation">A string transformation action delegate.</param>
        /// <param name="separator">The string that separates fields.</param>
        /// <returns>A string field list with the transformed fields name separated by the specified separator.</returns>
        private string GetFieldList(Predicate<FieldInfo> match, FieldTransformation transformation, string separator)
        {
            string fieldList = "";
            int fieldIndex = 0;
            List<FieldInfo> fields;

            //Public member match
            if (match != null)
                fields = GetFields().FindAll(match);
            else
                fields = GetFields();

            //Public filtered members iteration
            foreach (FieldInfo fieldInfo in fields)
            {
                string fieldName;

                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;

                //Transform the field name
                if (transformation != null)
                    fieldName = transformation(field);
                else
                    fieldName = fieldInfo.Name;

                //Build the SQL text
                if (fieldIndex == 0)
                    fieldList = fieldName;
                else
                    fieldList = fieldList + separator + fieldName;

                fieldIndex++;
            }

            return fieldList;

        }
        /// <summary>
        /// Gets a string field list. The fields match the specified match 
        /// predicate delegate and their names are transformed by the specified 
        /// string transformation action delegate.
        /// </summary>
        /// <param name="match">A match predicate delegate.</param>
        /// <param name="transformation">A string transformation action delegate.</param>
        /// <returns>A string field list with the transformed fields name separated by commas.</returns>
        private string GetFieldList(Predicate<FieldInfo> match, FieldTransformation transformation)
        {
            return GetFieldList(match, transformation, ",");
        }
        /// <summary>
        /// Gets a string field list. The fields match the specified match 
        /// predicate delegate.
        /// </summary>
        /// <param name="match">A match predicate delegate.</param>
        /// <returns>A string field list separated by commas.</returns>
        private string GetFieldList(Predicate<FieldInfo> match)
        {
            return GetFieldList(match, null, ",");
        }
        /// <summary>
        /// Gets the string field list.
        /// </summary>
        /// <returns>The string field list separated by commas.</returns>
        private string GetFieldList()
        {
            return GetFieldList(null, null, ",");
        }
        #endregion
        #region SQL command text building
        /// <summary>
        /// SQL clause builder delegate.
        /// </summary>
        /// <returns>A string SQL clause.</returns>
        private delegate string BuildSqlClauseDelegate();
        /// <summary>
        /// Builds a SELECT * clause with a WHERE clause that filters the primary key value.
        /// </summary>
        /// <returns>A SELECT SQL clause.</returns>
        private string GetDefaultSelect()
        {
            string fieldList = GetFieldList();
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, " AND ");
            string sortedList = GetFieldList(IsSortedField, TransformToOrderedField);

            if (sortedList.Length != 0)
                sortedList = "ORDDER BY " + sortedList;

            return string.Format("SELECT {0} FROM {1} WHERE {2} {3}", fieldList, Name, whereClause, sortedList);
        }
        /// <summary>
        /// Builds a SELECT primary key fields clause with a WHERE clause that filters the primary key value.
        /// Génération d'une requête SELECT ne prenant en compte que la clé primaire. Sert pour vérifier l'existance d'un enregistrement.
        /// </summary>
        /// <returns>A SELECT SQL clause.</returns>
        private string GetPrimarySelect()
        {
            string fieldList = GetFieldList(IsPrimaryKeyField);
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, " AND ");
            return string.Format("SELECT {0} FROM {1} WHERE {2}", fieldList, Name, whereClause);
        }
        /// <summary>
        /// Builds a SELECT * clause with a WHERE clause builded with the filtered fields.
        /// </summary>
        /// <returns>A SELECT SQL clause.</returns>
        private string GetFilteredSelect()
        {
            string fieldList = GetFieldList();

            string whereClause = GetFieldList(IsFilteredField, TransformToParametizedAffectationField, " AND ");
            if (whereClause.Length > 0)
                whereClause = "WHERE " + whereClause;

            string sortedList = GetFieldList(IsSortedField, TransformToOrderedField);
            if (sortedList.Length != 0)
                sortedList = "ORDER BY " + sortedList;

            return string.Format("SELECT {0} FROM {1} {2} {3}", fieldList, Name, whereClause, sortedList);
        }
        /// <summary>
        /// Builds an INSERT clause with the assigned fields.
        /// </summary>
        /// <returns>An INSERT SQL clause.</returns>
        private string GetDefaultInsert()
        {
            string fieldList = GetFieldList(IsAffectedField);
            string parameterList = GetFieldList(IsAffectedField, TransformToParametizedField);
            return string.Format("INSERT INTO {0} ({1}) VALUES ({2})", Name, fieldList, parameterList);
        }
        /// <summary>
        /// Builds an UPDATE clause with th assigned fields.
        /// </summary>
        /// <returns>An UPDATE SQL clause.</returns>
        private string GetDefaultUpdate()
        {
            string fieldList = GetFieldList(IsAffectedNonPrimaryField, TransformToParametizedAffectationField);
            if (fieldList.Length == 0)
                return "";

            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, " AND ");
            return string.Format("UPDATE {0} SET {1} WHERE {2}", Name, fieldList, whereClause);
        }
        /// <summary>
        /// Builds a DELETE clause with a WHERE clause builded with the primary key value.
        /// </summary>
        /// <returns>A DELETE SQL clause.</returns>
        private string GetDefaultDelete()
        {
            string parameterList = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, " AND ");
            return string.Format("DELETE FROM {0} WHERE {1}", Name, parameterList);
        }
        /// <summary>
        /// Builds a CREATE TABLE clause. Uses the field properties and
        /// attributes of the derived class.
        /// </summary>
        /// <returns>A CREATE TABLE SQL clause.</returns>
        private string GetDefaultCreate()
        {
            string fieldList = GetFieldList(null, TransformToTypedField);
            return string.Format("CREATE TABLE {0} ({1})", Name, fieldList);
        }
        /// <summary>
        /// Builds an SQL clause to add primary key constaint to a database table.
        /// </summary>
        /// <returns>An ALTER TABLE ADD CONTRAINT PRIMARY KEY clause.</returns>
        private string GetDefaultAddPrimaryConstraint()
        {
            string fieldList = GetFieldList(IsPrimaryKeyField);
            return string.Format("ALTER TABLE {0} ADD CONSTRAINT PK_{0} PRIMARY KEY ({1});", Name, fieldList);
        }
        /// <summary>
        /// Builds the whole SQL commant text.
        /// </summary>
        /// <param name="buildSqlClauseDelegate">A SQL clause builder delegate.</param>
        /// <param name="parametersFillingPredicate">A predicate delagate to indicate the way of initializing query parameters value.</param>
        /// <param name="nonQuery">Indicates if the query is a DDL query.</param>
        /// <returns>The query SQL command text.</returns>
        private bool BuildQuery(BuildSqlClauseDelegate buildSqlClauseDelegate,
            Predicate<FieldInfo> parametersFillingPredicate, bool nonQuery)
        {
            //Instanciate the Query object
            Query fbQuery = new Query(_Server);
            fbQuery.CommandText = buildSqlClauseDelegate();

            if (fbQuery.CommandText.Length == 0)
                return true;

            //Initialize parameters
            FillParameters(fbQuery, parametersFillingPredicate);

            //Execute the query
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
        #region Parameters initialization
        /// <summary>
        /// Initializes parameters value. Gets the value from the fields value property.
        /// </summary>
        /// <param name="query">A Query object.</param>
        /// <param name="match">Predicate delegate to filter parameters that must be initialized.</param>
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
        #endregion
        #endregion
    }
    #endregion
    #region Public enums
    public enum SqlSortDirection
    {
        None,
        Asc,
        Desc
    }
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
        #region Public methods
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
        #region Private fields
        /// <summary>
        /// Holds the int32 value.
        /// </summary>
        private int _Value;
        #endregion
        #region Public properties
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
        #endregion
        #region Public methods
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.Int32.
        /// </summary>
        public DbInt32Field()
        {
            DbType = DbType.Int32;
        }
        #endregion
    }
    /// <summary>
    /// Represents a int64 field.
    /// </summary>
    public class DbInt64Field : DbField
    {
        #region Private fields
        /// <summary>
        /// Holds the int64 value.
        /// </summary>
        private long _Value;
        #endregion
        #region Public properties
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
        #endregion
        #region Public methods
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.Int64.
        /// </summary>
        public DbInt64Field()
        {
            DbType = DbType.Int64;
        }
        #endregion
    }
    /// <summary>
    /// Represents a DateTime field.
    /// </summary>
    public class DbDateTimeField : DbField
    {
        #region Private fields
        /// <summary>
        /// Holds the DateTime field value.
        /// </summary>
        private DateTime _Value;
        #endregion
        #region Public properties
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
        #endregion
        #region Public methods
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.DateTime.
        /// </summary>
        public DbDateTimeField()
        {
            DbType = DbType.DateTime;
        }
        #endregion
    }
    /// <summary>
    /// Represents a string field.
    /// </summary>
    public class DbStringField : DbField
    {
        #region Private fields
        /// <summary>
        /// Holds the field size.
        /// </summary>
        private int _Size;
        /// <summary>
        /// Holds the string field value.
        /// </summary>
        private string _Value;
        #endregion
        #region Public properties
        /// <summary>
        /// Gets or sets the field size.
        /// </summary>
        public int Size
        {
            get { return _Size; }
            set { _Size = value; }
        }
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
        #endregion
        #region Public methods
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.String.
        /// </summary>
        public DbStringField()
        {
            DbType = DbType.String;
        }
        #endregion
    }
    /// <summary>
    /// Represents a text blob field.
    /// </summary>
    public class DbTextField : DbStringField
    {
        #region Public methods
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.String.
        /// Sets the size to -1.
        /// </summary>
        public DbTextField()
        {
            DbType = DbType.String;
            Size = -1;
        }
        #endregion
    }
    /// <summary>
    /// Represents a binary blob field.
    /// </summary>
    public class DbBinaryField : DbField
    {
        #region Private fields
        /// <summary>
        /// Holds the int32 value.
        /// </summary>
        private object _Value;
        #endregion
        #region Public properties
        /// <summary>
        /// Gets or sets the int32 field value.
        /// </summary>
        public new object Value
        {
            get { return _Value; }
            set
            {
                base.Value = value;
                _Value = value;
            }
        }
        #endregion
        #region Public methods
        /// <summary>
        /// Initialize a new instance of the class. Sets the underlying database type to DbType.Int32.
        /// </summary>
        public DbBinaryField()
        {
            DbType = DbType.Binary;
        }
        #endregion
    }

    #endregion
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
}

