using System;

namespace BunnyCDN.Model
{
    /// <summary>
    /// An exception thrown by BunnyCDNStorage
    /// </summary>
    internal class BunnyCDNStorageException : Exception
    {
        /// <summary>
        /// Initializes a new instance of the BunnyCDNStorageException class
        /// </summary>
        /// <param name="message"></param>
        internal BunnyCDNStorageException(string message) : base(message) { }
    }
    
    /// <summary>
    /// An exception thrown by BunnyCDNStorage caused by authentication failure
    /// </summary>
    internal class BunnyCDNStorageAuthenticationException : BunnyCDNStorageException
    {
        /// <summary>
        /// Initialize a new instance of the BunnyCDNStorageAuthenticationException class
        /// </summary>
        /// <param name="path">The path that is not found</param>
        internal BunnyCDNStorageAuthenticationException(string storageZoneName, string accessKey) : base($"Authentication failed for storage zone '{storageZoneName}' with access key '{accessKey}'.")
        {

        }
    }
    /// <summary>
    /// An exception thrown by BunnyCDNStorage
    /// </summary>
    internal class BunnyCDNStorageFileNotFoundException : BunnyCDNStorageException
    {
        /// <summary>
        /// Initialize a new instance of the BunnyCDNStorageFileNotFoundException class
        /// </summary>
        /// <param name="path">The path that is not found</param>
        internal BunnyCDNStorageFileNotFoundException(string path) : base($"Could not find part of the object path: {path}")
        {

        }
    }
}
