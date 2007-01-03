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
    /// Classe représentant un champ lié à la base de données
    /// </summary>
    public class DbField
    {
        #region Champs privés

        private string _Name;
        private object _Value;
        private bool _IsFiltered;
        private SqlSortDirection _SortDirection;

        #endregion

        #region Propriétés

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
        /// Détermine si le champ entre dans une clause WHERE
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

        #region Méthodes publiques

        /// <summary>
        /// Constructeur
        /// </summary>
        /// <param name="name">Nom du champ</param>
        /// <param name="isPrimaryKey">Indique si ce champ entre dans la composition de la clé primaire</param>
        public DbField(string name)
        {
            _Name = name;
        }

        /// <summary>
        /// Constructeur par défaut
        /// </summary>
        public DbField() : this("") { }

        /// <summary>
        /// Valeur entière de l'objet Value
        /// </summary>
        /// <remarks>Pas de vérification de type</remarks>
        /// <returns>Un entier</returns>
        public int ToInt()
        {
            return (int)Value;
        }

        #endregion
    }

    /// <summary>
    /// Classe représentant une table de la base de données
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

        #region Champs privés

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
                //Instanciation de la propriété
                DbField fieldInstance = (DbField)System.Activator.CreateInstance(typeof(DbField));
                fieldInstance.Name = fieldInfo.Name;
                fieldInfo.SetValue(this, fieldInstance);

                //Initialisation de l'objet
                fieldInstance.Filtered = false;                
                fieldInstance.Value = null;
            }

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
            }

            return hasRecords;
        }        

        #endregion

        #region Méthodes protégées

        /// <summary>
        /// Retourne le nom de la table en accord avec le nom de la classe
        /// </summary>
        protected string GetTableName()
        {
            return this.GetType().Name;
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

        #region Recherche de propriétés

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
        protected List<FieldInfo> GetFields()
        {
            return new List<FieldInfo>(this.GetType().GetFields());
        }


        #endregion

        #region Méthodes DML frontales

        /// <summary>
        /// Initialise les champs de l'entité avec les valeurs chargées depuis la base de données.
        /// La clause WHERE se base sur la valeur de la clé primaire définie et initialisée pour l'entité 
        /// </summary>
        public virtual bool Fill()
        {
            return BuildQuery(GetDefaultSelect, IsPrimaryKeyField, false);
        }

        /// <summary>
        /// Initialise les champs de l'entité avec les valeurs chargées depuis la base de données.
        /// La clause WHERE se base sur la valeur des champs passés en paramètre
        /// </summary>
        /// <param name="filteredFields">Liste des champs initialisés avec une valeur</param>
        public virtual bool Filter()
        {
            return BuildQuery(GetFilteredSelect, IsFilteredField, false);
        }

        /// <summary>
        /// 
        /// </summary>
        public virtual bool Update()
        {
            //Déterminer si l'enregistrement est à ajouter ou à insérer en fonction de la valeur de la clé primaire
            if (Exists())
                return BuildQuery(GetDefaultUpdate, IsAffectedField, true);
            else
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
        public virtual bool Delete()
        {
            return BuildQuery(GetDefaultDelete, IsPrimaryKeyField, true);
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
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, "AND");

            //Liste de tri
            string sortedList = GetFieldList(IsSortedField, TransformToOrderedField);
            if (sortedList.Length != 0)
                sortedList = "ORDDER BY " + sortedList;

            //Construction de la chaine SQL
            return string.Format("SELECT {0} FROM {1} WHERE {2} {3}", fieldList, GetTableName(), whereClause, sortedList);

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
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, "AND");

            //Construction de la chaine SQL
            return string.Format("SELECT {0} FROM {1} WHERE {2}", fieldList, GetTableName(), whereClause);

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
            return string.Format("INSERT INTO {0} ({1}) VALUES ({2})", GetTableName(), fieldList, parameterList);
        }

        /// <summary>
        /// Génération de la requête UPDATE        
        /// </summary>
        /// <returns></returns>
        private string GetDefaultUpdate()
        {
            //Liste des champs
            string fieldList = GetFieldList(IsAffectedNonPrimaryField, TransformToParametizedAffectationField);

            //Liste des clé primaires
            string whereClause = GetFieldList(IsPrimaryKeyField, TransformToParametizedAffectationField, "AND");

            //Construction de la chaine SQL
            return string.Format("UPDATE {0} SET {1} WHERE {2}", GetTableName(), fieldList, whereClause);
        }

        /// <summary>
        /// Génération de la requête DELETE
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
            Query fbQuery = new Query(_DbServer);
            fbQuery.CommandText = buildSqlClauseDelegate();

            //Affectation des paramètres
            FillParameters(fbQuery, parametersFillingPredicate);

            //Execution de la requête
            if (nonQuery)
                return ExecuteNonQuery(fbQuery);
            else
                return ExecuteReader(fbQuery);
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
        /// Permet d'obtenir le type de données associé au champ
        /// </summary>
        /// <param name="fieldInfo">Le champ</param>
        /// <returns>Un type de donnée</returns>
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
