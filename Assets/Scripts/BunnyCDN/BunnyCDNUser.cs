using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Threading.Tasks;
using UnityEngine;
using Debug = UnityEngine.Debug;

namespace BunnyCDN.Controller
{
    internal class BunnyCDNUser : BunnyCDNStorage
    {
        internal BunnyCDNUser() : base() { }

        #region Download

        /// <summary>
        /// Download the object to a local file
        /// </summary>
        /// <param name="path">path</param>
        /// <returns></returns>
        internal async Task DownloadObjectAsync(string remotePath, string localFilePath)
        {
            string targetPath = $"/{_storageZoneName}/{remotePath}";
            var normalizedPath = NormalizePath(targetPath);
            try
            {
                using (var stream = await DownloadObjectAsStreamAsync(normalizedPath))
                {

                    using (var fileStream = new FileStream(localFilePath, FileMode.OpenOrCreate,
                        FileAccess.ReadWrite, FileShare.ReadWrite))
                    {
                        await stream.CopyToAsync(fileStream);
                    }
                    
                    /*using (var bufferedStream = new BufferedStream(stream))
                    {
                        using (var fileStream = new FileStream(localFilePath, FileMode.OpenOrCreate,
                            FileAccess.ReadWrite, FileShare.ReadWrite))
                        {
                            bufferedStream.CopyTo(fileStream);
                        }
                    }*/
                }
            }
            catch (WebException ex)
            {
                throw MapResponseToException((HttpStatusCode) (int) ex.Status, remotePath);
            }
        }

        /// <summary>
        /// Return a stream with the contents of the object
        /// </summary>
        /// <param name="path">path</param>
        /// <returns></returns>
        internal async Task<Stream> DownloadObjectAsStreamAsync(string path)
        {
            try
            {
                string normalizedPath = NormalizePath(path, false);
                return await _http.GetStreamAsync(normalizedPath);
            }
            catch (WebException ex)
            {
                throw MapResponseToException((HttpStatusCode) (int) ex.Status, path);
            }
        }

        /// <summary>
        /// Download object to byte[]
        /// </summary>
        /// <param name="path">Path inside storage</param>
        /// <returns>Data in byte[]</returns>
        /// <exception cref="BunnyCDNStorageException"></exception>
        internal async Task<byte[]> DownloadObjectAsByteAsync(string path)
        {
            try
            {
                string targetPath = $"/{_storageZoneName}/{path}";
                string normalizedPath = NormalizePath(targetPath, false);
                byte[] data = await _http.GetByteArrayAsync(normalizedPath);
                return data;
            }
            catch (WebException ex)
            {
                throw MapResponseToException((HttpStatusCode) (int) ex.Status, path);
            }
        }

        #endregion
    }
}