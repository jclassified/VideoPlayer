using System;
using System.Collections.Generic;
using UnityEngine;

namespace BunnyCDN
{
    [Serializable] 
    public class StorageObject
    {
        /// <summary>
        /// The unique GUID of the file
        /// </summary>
        public string Guid;
        /// <summary>
        /// The ID of the BunnyCDN user that holds the file
        /// </summary>
        public string UserId;

        /// <summary>
        /// The date when the file was created
        /// </summary>
        //public DateTime DateCreated;

        /// <summary>
        /// The date when the file was last modified
        /// </summary>
        //public DateTime LastChanged;

        /// <summary>
        /// The name of the storage zone to which the file is linked
        /// </summary>
        public string StorageZoneName;

        /// <summary>
        /// The path to the object
        /// </summary>
        public string Path;

        /// <summary>
        /// The name of the object
        /// </summary>
        public string ObjectName;

        /// <summary>
        /// The total of the object in bytes
        /// </summary>
        public long Length;

        /// <summary>
        /// True if the object is a directory
        /// </summary>
        public bool IsDirectory;

        /// <summary>
        /// The ID of the storage server that the file resides on
        /// </summary>
        public int ServerId;

        /// <summary>
        /// The ID of the storage zone that the object is linked to
        /// </summary>
        public long StorageZoneId;
        /// <summary>
        /// Gets the full path to the file
        /// </summary>
        public string FullPath => Path + ObjectName;
    }
}
