using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Data;
using System.Data.Common;

namespace Sofia.Data.Common
{
    [AttributeUsage(AttributeTargets.Field)]
    public class PrimaryKeyAttribute : Attribute
    {
        public PrimaryKeyAttribute()
        {
        }
    }

    [AttributeUsage(AttributeTargets.Field)]
    public class FieldTypeAttribute : Attribute
    {
        public FieldTypeAttribute(DbType dbType)
        {
            _DbType = dbType;
        }

        private DbType _DbType;
        public DbType DbType
        {
            get
            {
                return _DbType;
            }
        }

    }


    /// <summary>
    /// Classe repr�sentant un champ li� � la base de donn�es
    /// </summary>
    public class DbField
    {
        #region Champs priv�s

        private string _Name;
        private object _Value;
        private bool _IsFiltered;
        private SqlSortDirection _SortDirection;

        #endregion

        #region Propri�t�s

        /// <summary>
        /// Nom du champ
        /// </summary>
        public string Name
        {
            get { return _Name; }
            set { _Name = value; }
        }

        /// <summary>
        /// Valeur du champ
        /// </summary>
        public object Value
        {
            get { return _Value; }
            set { _Value = value; }
        }

        /// <summary>
        /// D�termine si le champ entre dans une clause WHERE
        /// </summary>
        public bool Filtered
        {
            get { return _IsFiltered; }
            set { _IsFiltered = value; }
        }

        /// <summary>
        /// Indique si le champ entre dans le cadre d'un tri
        /// </summary>
        public SqlSortDirection SortDirection
        {
            get { return _SortDirection; }
            set { _SortDirection = value; }
        }

        #endregion

        #region M�thodes publiques

        /// <summary>
        /// Constructeur
        /// </summary>
        /// <param name="name">Nom du champ</param>
        /// <param name="isPrimaryKey">Indique si ce champ entre dans la composition de la cl� primaire</param>
        public DbField(string name)
        {
            _Name = name;
        }

        /// <summary>
        /// Constructeur par d�faut
        /// </summary>
        public DbField() : this("") { }

        /// <summary>
        /// Valeur enti�re de l'objet Value
        /// </summary>
        /// <remarks>Pas de v�rification de type</remarks>
        /// <returns>Un entier</returns>
        public int ToInt()
        {
            return (int)Value;
        }

        #endregion
    }

    /// <summary>
    /// Classe repr�sentant une table de la base de donn�es
    /// </summary>
    public abstract class EntityBase
    {
        #region Enumerations

        public enum FieldListMode
        {
            Select,
            Insert
        }

        #endregion

        #region Champs priv�s

        private Server _DbServer;
        private DbDataReader _DbDataReader;
        private List<SqlJoin> _Joins;

        #endregion

        #region Contructeur

        public EntityBase(Server dbServer)
        {
            _DbServer = dbServer;
            _Joins = new List<SqlJoin>();

            foreach (FieldInfo fieldInfo in GetFields())
            {
                //Instanciation de la propri�t�
                DbField fieldInstance = (DbField)System.Activator.CreateInstance(typeof(DbField));
                fieldInstance.Name = fieldInfo.Name;
                fieldInfo.SetValue(this, fieldInstance);

                //Initialisation de l'objet
                fieldInstance.Filtered = false;                
                fieldInstance.Value = null;
            }

        }

        #endregion

        #region R�initialisation de propri�t�s

        /// <summary>
        /// R�initialise le tri
        /// </summary>
        public void ResetSort()
        {
            GetFields().ForEach(ActionResetSort);
        }

        /// <summary>
        /// R�initialise la valeur de tous les champs
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
        /// R�initialise tous la propri�t� Filtered de tous les champs
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
        /// D�l�gu� sur une classe g�n�rique Action pour le type FieldInfo qui a pour effet d'enlever le tri sur un champ
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
        /// D�termine si le nom d'une colonne est pr�sente dans le reader
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
        /// Lecture des enregistrements r�sultant de la derni�re requ�te
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
            }

            return hasRecords;
        }        

        #endregion

        #region M�thodes prot�g�es

        /// <summary>
        /// Retourne le nom de la table en accord avec le nom de la classe
        /// </summary>
        protected string GetTableName()
        {
            return this.GetType().Name;
        }

        #endregion

        #region Methodes bas niveau pour ex�cution des requ�tes

        /// <summary>
        /// ExecuteReader sur un objet FbQuery initialis�
        /// </summary>
        /// <param name="fbQuery">objet FbQuery initialis�</param>
        protected bool ExecuteReader(Query fbQuery)
        {
            if (_DbDataReader != null)
                _DbDataReader.Close();

            //Ex�cution de la requ�te
            _DbDataReader = fbQuery.ExecuteReader();


            if (_DbDataReader == null)
            {
                return false;
            }

            //Affectation des propri�t�s
            return Read();
        }

        /// <summary>
        /// ExecuteNonQuery sur un objet FbQuery initialis�
        /// </summary>
        /// <param name="fbQuery">objet FbQuery initialis�</param>
        protected bool ExecuteNonQuery(Query fbQuery)
        {
            //Ex�cution de la requ�te
            if (!fbQuery.ExecuteNonQuery())
            {
                return false;
            }
            else
                return true;
        }

        #endregion

        #region Recherche de propri�t�s

        /// <summary>
        /// Delegate de pr�dicat portant sur les propri�t�s de type Field
        /// </summary>
        /// <param name="property">Instance de la propri�t� Field</param>
        /// <returns>Vrai si la propri�t� Field.Filtered = true</returns>
        private bool IsFilteredField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return field.Filtered;
        }

        /// <summary>
        /// Delegate de pr�dicat portant sur les propri�t�s de type Field
        /// </summary>
        /// <param name="property">Instance de la propri�t� Field</param>
        /// <returns>Vrai si la propri�t� Field.IsPrimaryKey = true</returns>
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
        /// Delegate de pr�dicat portant sur les propri�t�s de type Field
        /// </summary>
        /// <param name="property">Instance de la propri�t� Field</param>
        /// <returns>Vrai si la propri�t� Field.SortDirection != None</returns>
        private bool IsSortedField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return field.SortDirection != SqlSortDirection.None;
        }

        /// <summary>
        /// Delegate de pr�dicat portant sur les propri�t�s de type Field
        /// </summary>
        /// <param name="property">Instance de la propri�t� Field</param>
        /// <returns>Vrai si la propri�t� Field.Value != null</returns>
        private bool IsAffectedField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return field.Value != null;
        }

        /// <summary>
        /// Delegate de pr�dicat portant sur les propri�t�s de type Field
        /// </summary>
        /// <param name="property">Instance de la propri�t� Field</param>
        /// <returns>Vrai si la propri�t� Field.Value != null</returns>
        private bool IsAffectedNonPrimaryField(FieldInfo fieldInfo)
        {
            DbField field = (DbField)fieldInfo.GetValue(this);
            return (field.Value != null && !IsPrimaryKeyField(fieldInfo));
        }

        /// <summary>
        /// Obtient de la liste des propri�t�s publiques
        /// </summary>
        /// <returns>Une liste g�n�rique des propri�t�s publiques</returns>
        protected List<FieldInfo> GetFields()
        {
            return new List<FieldInfo>(this.GetType().GetFields());
        }


        #endregion

        #region M�thodes DML frontales

        /// <summary>
        /// Initialise les champs de l'entit� avec les valeurs charg�es depuis la base de donn�es.
        /// La clause WHERE se base sur la valeur de la cl� primaire d�finie et initialis�e pour l'entit� 
        /// </summary>
        public virtual bool Fill()
        {
            return BuildQuery(GetDefaultSelect, IsPrimaryKeyField, false);
        }

        /// <summary>
        /// Initialise les champs de l'entit� avec les valeurs charg�es depuis la base de donn�es.
        /// La clause WHERE se base sur la valeur des champs pass�s en param�tre
        /// </summary>
        /// <param name="filteredFields">Liste des champs initialis�s avec une valeur</param>
        public virtual bool Filter()
        {
            return BuildQuery(GetFilteredSelect, IsFilteredField, false);
        }

        /// <summary>
        /// 
        /// </summary>
        public virtual bool Update()
        {
            //D�terminer si l'enregistrement est � ajouter ou � ins�rer en fonction de la valeur de la cl� primaire
            if (Exists())
                return BuildQuery(GetDefaultUpdate, IsAffectedField, true);
            else
                return BuildQuery(GetDefaultInsert, IsAffectedField, true);
        }

        /// <summary>
        /// Permet de d�terminer si un enregistrement existe en se basant sur sa cl� primaire
        /// </summary>
        /// <returns></returns>
        public bool Exists()
        {
            return BuildQuery(GetPrimarySelect, IsPrimaryKeyField, false);
        }

        /// <summary>
        /// Suppression de l'enregistrement dont la cl� primaire est sp�cifi�e
        /// </summary>
        public virtual bool Delete()
        {
            return BuildQuery(GetDefaultDelete, IsPrimaryKeyField, true);
        }

        #endregion

        #region Construction de liste de champs

        /// <summary>
        /// Delagate permettant de transformer une cha�ne de caract�res
        /// </summary>
        /// <param name="value">La chaine � transformer</param>
        /// <returns>La chaine tranform�e</returns>
        private delegate string FieldTransformation(DbField field);


        /// <summary>
        /// Transforme un champ en un champ param�tr�
        /// <example>FIELD => @FIELD</example>
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private string TransformToParametizedField(DbField field)
        {
            return "@" + field.Name;
        }

        /// <summary>
        /// Transforme un champ en un champ pr�sent dans une clause ORDER BY
        /// <example>FIELD => FIELD Asc</example>
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private string TransformToOrderedField(DbField field)
        {
            return field.Name + " " + Enum.GetName(typeof(SqlSortDirection), field.SortDirection);
        }

        /// <summary>
        /// Transforme un champ en un champ param�tr�
        /// <example>FIELD => FIELD = @FIELD</example>
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private string TransformToParametizedAffectationField(DbField field)
        {
            return field.Name + " = @" + field.Name;
        }

        /// <summary>
        /// S�lection d'une liste de propri�t�s pour construire une liste de champs SQL en fonction d'une transformation
        /// </summary>
        /// <param name="fieldFilter">D�l�gu� qui d�finit les propri�t�s � rechercher</param>
        /// <param name="stringTransformation">D�l�gu� qui sp�cifie une transformation � appliquer au nom de chaque propri�t�</param>
        /// <param name="separator">S�parateur</param>
        /// <returns>Une liste de nom de champs s�par�s par le s�parateur</returns>
        private string GetFieldList(Predicate<FieldInfo> match, FieldTransformation transformation, string separator)
        {
            string fieldList = "";
            int fieldIndex = 0;
            List<FieldInfo> fields;

            //Filtre sur les propri�t�s
            if (match != null)
                fields = GetFields().FindAll(match);
            else
                fields = GetFields();


            //Parcours des propri�t�s filtr�es
            foreach (FieldInfo fieldInfo in fields)
            {
                string fieldName;

                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;

                //Application d'une transformation sur le nom de la propri�t�
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
        /// G�n�ration de la requ�te SELECT en se basant sur la cl� primaire pour la constitution de la clause WHERE
        /// </summary>
        /// <returns></returns>
        private string GetDefaultSelect()
        {
            //Liste des champs
            string fieldList = GetFieldList();

            //Liste des cl� primaires
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, "AND");

            //Liste de tri
            string sortedList = GetFieldList(IsSortedField, TransformToOrderedField);
            if (sortedList.Length != 0)
                sortedList = "ORDDER BY " + sortedList;

            //Construction de la chaine SQL
            return string.Format("SELECT {0} FROM {1} WHERE {2} {3}", fieldList, GetTableName(), whereClause, sortedList);

        }

        /// <summary>
        /// G�n�ration d'une requ�te SELECT ne prenant en compte que la cl� primaire. Sert pour v�rifier l'existance d'un enregistrement.
        /// </summary>
        /// <returns></returns>
        private string GetPrimarySelect()
        {
            //Liste des champs
            string fieldList = GetFieldList(IsPrimaryKeyField);

            //Liste des cl� primaires
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, "AND");

            //Construction de la chaine SQL
            return string.Format("SELECT {0} FROM {1} WHERE {2}", fieldList, GetTableName(), whereClause);

        }

        /// <summary>
        /// G�n�ration de la requ�te SELECT en se basant sur les champs filtr�s pour la constitution de la clause WHERE
        /// </summary>
        /// <returns></returns>
        private string GetFilteredSelect()
        {
            //Liste des champs
            string fieldList = GetFieldList();

            //Liste des champs filtr�s
            string whereClause = GetFieldList(IsFilteredField, TransformToParametizedAffectationField, "AND");
            if (whereClause.Length > 0)
                whereClause = "WHERE " + whereClause;

            //Liste de tri
            string sortedList = GetFieldList(IsSortedField, TransformToOrderedField);
            if (sortedList.Length != 0)
                sortedList = "ORDER BY " + sortedList;

            //Construction de la chaine SQL
            return string.Format("SELECT {0} FROM {1} {2} {3}", fieldList, GetTableName(), whereClause, sortedList);

        }

        /// <summary>
        /// G�n�ration de la requ�te INSERT
        /// <param name="ignorePrimaryKey">L'initialisation de la chaine se fait sans la cl� primaire si ce param�tre est true</param>
        /// </summary>
        /// <returns></returns>
        private string GetDefaultInsert()
        {
            //Liste des champs
            string fieldList = GetFieldList(IsAffectedField);
            string parameterList = GetFieldList(IsAffectedField, TransformToParametizedField);

            //Construction de la chaine SQL
            return string.Format("INSERT INTO {0} ({1}) VALUES ({2})", GetTableName(), fieldList, parameterList);
        }

        /// <summary>
        /// G�n�ration de la requ�te UPDATE        
        /// </summary>
        /// <returns></returns>
        private string GetDefaultUpdate()
        {
            //Liste des champs
            string fieldList = GetFieldList(IsAffectedNonPrimaryField, TransformToParametizedAffectationField);

            //Liste des cl� primaires
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, "AND");

            //Construction de la chaine SQL
            return string.Format("UPDATE {0} SET {1} WHERE {2}", GetTableName(), fieldList, whereClause);
        }

        /// <summary>
        /// G�n�ration de la requ�te DELETE
        /// </summary>
        /// <returns></returns>
        private string GetDefaultDelete()
        {
            //Liste des parametres
            string parameterList = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, "AND");

            //Construction de la chaine SQL
            return string.Format("DELETE FROM {0} WHERE {1}", GetTableName(), parameterList);
        }

        /// <summary>
        /// Construction de la cha�ne de requ�te
        /// </summary>
        /// <param name="buildSqlClauseDelegate"></param>
        /// <param name="parametersFillingPredicate"></param>
        /// <param name="nonQuery"></param>
        /// <returns></returns>
        private bool BuildQuery(BuildSqlClauseDelegate buildSqlClauseDelegate,
            Predicate<FieldInfo> parametersFillingPredicate, bool nonQuery)
        {
            //Instanciation de la requ�te
            Query fbQuery = new Query(_DbServer);
            fbQuery.CommandText = buildSqlClauseDelegate();

            //Affectation des param�tres
            FillParameters(fbQuery, parametersFillingPredicate);

            //Execution de la requ�te
            if (nonQuery)
                return ExecuteNonQuery(fbQuery);
            else
                return ExecuteReader(fbQuery);
        }

        #endregion

        #region Initialisation des param�tres

        /// <summary>
        /// Initialisation des param�tres de la requ�te en fonction des champs
        /// </summary>
        /// <param name="fbQuery">La requ�te</param>
        /// <param name="match">Ne traite que les propri�t�s qui r�pondent � ce pr�dicat</param>
        private void FillParameters(Query fbQuery, Predicate<FieldInfo> match)
        {
            fbQuery.FlushParameters();
            List<FieldInfo> fields;

            if (match != null)
                fields = GetFields().FindAll(match);
            else
                fields = GetFields();

            foreach (FieldInfo fieldInfo in fields)
            {
                object fieldValue = fieldInfo.GetValue(this);
                DbField field = (DbField)fieldValue;
                DbType dbType = GetDbType(fieldInfo);
                fbQuery.AddParameter(field.Name, dbType, field.Value);
            }
        }

        /// <summary>
        /// Permet d'obtenir le type de donn�es associ� au champ
        /// </summary>
        /// <param name="fieldInfo">Le champ</param>
        /// <returns>Un type de donn�e</returns>
        private DbType GetDbType(FieldInfo fieldInfo)
        {
            object[] attributes = fieldInfo.GetCustomAttributes(typeof(FieldTypeAttribute), true);
            if (attributes.Length != 0)
            {
                FieldTypeAttribute attribute = attributes[0] as FieldTypeAttribute;
                return attribute.DbType;
            }
            else
            {
                return DbType.String;
            }
        }

        #endregion

    }

    /// <summary>
    /// Classe repr�sentant une jointure
    /// </summary>
    public class SqlJoin
    {
        #region Champs priv�s

        private EntityBase _Entity;
        private DbFieldMapping _FieldMapping;

        #endregion

        #region Propri�t�s

        /// <summary>
        /// Entit� sur laquelle porte la jointure
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

        #region M�thodes publiques

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
    /// Classe repr�sentant une correspondance entre deux champs d'une entit�
    /// </summary>
    public class DbFieldMapping
    {
        #region Champs priv�s

        private DbField _LeftField;
        private DbField _RightField;

        #endregion

        #region Propri�t�s

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

        #region M�thodes publiques

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
