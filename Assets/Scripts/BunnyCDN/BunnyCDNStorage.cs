using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Threading.Tasks;
using BunnyCDN.Model;
using SimpleJSON;
using UnityEngine;

namespace BunnyCDN.Controller
{
    internal class BunnyCDNStorage
    {
        /// <summary>
        /// The API access key used for authentication
        /// </summary>
        protected const string _apiKey = "e7608330-ca6e-45a7-a3fae667179d-5f7c-4319";

        /// <summary>
        /// The name of the storage zone we are working on
        /// </summary>
        protected const string _storageZoneName = "spherum-storage";

        internal const string path_StudioVideos = "studio_videos";

        internal enum VideoType
        {
            Studio,
        }

        /// <summary>
        /// The HTTP Client used for the API communication
        /// </summary>
        protected HttpClient _http = null;

        #region Lifecycle

        /// <summary>
        /// Initializes a new instance of the BunnyCDNStorage class 
        /// </summary>
        internal BunnyCDNStorage()
        {    
            if (Application.internetReachability != NetworkReachability.ReachableViaCarrierDataNetwork &&
                Application.internetReachability != NetworkReachability.ReachableViaLocalAreaNetwork)
            {
                return;
            }
            // Initialize the HTTP Client
            _http = new HttpClient();
            _http.Timeout = new TimeSpan(0, 0, 120);
            _http.DefaultRequestHeaders.Add("AccessKey", _apiKey);
            _http.BaseAddress = new Uri("https://storage.bunnycdn.com/");
        }

        /// <summary>
        /// Dispose Bunny workflow
        /// </summary>
        internal void Dispose()
        {
            if (_http != null)
            {
                _http.Dispose();
            }
        }

        #endregion
        
        #region Listing

        /// <summary>
        /// Get the list of all videos //TODO: Write partial load
        /// </summary>
        internal async Task<List<StorageObject>> GetVideosListAsync()
        {
            string targetPath = $"/{_storageZoneName}/{path_StudioVideos}/";
            string path = NormalizePath(targetPath, true);
            HttpResponseMessage response = await _http.GetAsync(path);
            if (response.IsSuccessStatusCode)
            {
                string responseJson = await response.Content.ReadAsStringAsync();
                List<StorageObject> objects = new List<StorageObject>();
                JSONNode data = JSON.Parse(responseJson);
                for (int i = 0; i < data.Count; i++)
                {
                    objects.Add(JsonUtility.FromJson<StorageObject>(data[i].ToString()));
                }
                return objects;
            }
            else
            {
                throw MapResponseToException(response.StatusCode, path);
            }
        }
        /// <summary>
        /// Get the list of storage objects on the given path
        /// </summary>
        internal async Task<List<StorageObject>> GetStorageObjectsAsync(string path)
        {
            string targetPath = $"/{_storageZoneName}/{path}";
            string normalizePath = NormalizePath(targetPath, true);
            HttpResponseMessage response = await _http.GetAsync(normalizePath);

            if (response.IsSuccessStatusCode)
            {
                string responseJson = await response.Content.ReadAsStringAsync();
                List<StorageObject> objects = new List<StorageObject>();
                JSONNode data = JSON.Parse(responseJson);
                for (int i = 0; i < data.Count; i++)
                {
                    objects.Add(JsonUtility.FromJson<StorageObject>(data[i].ToString()));
                }
                return objects;
            }
            else
            {
                throw MapResponseToException(response.StatusCode, normalizePath);
            }
        }

        #endregion

        #region Utils

        /// <summary>
        /// Map the API response to the correct BunnyCDNStorageExecption
        /// </summary>
        /// <param name="statusCode">The StatusCode returned by the API</param>
        /// <param name="path">The called path</param>
        protected BunnyCDNStorageException MapResponseToException(HttpStatusCode statusCode, string path)
        {
            if (statusCode == HttpStatusCode.NotFound)
            {
                return new BunnyCDNStorageFileNotFoundException(path);
            }

            if (statusCode == HttpStatusCode.Unauthorized)
            {
                return new BunnyCDNStorageAuthenticationException(_storageZoneName, _apiKey);
            }

            return new BunnyCDNStorageException("An unknown error has occured during the request.");
        }

        /// <summary>
        /// Normalize a path string
        /// </summary>
        /// <returns></returns>
        protected string NormalizePath(string path, bool? isDirectory = null)
        {
            if (!path.StartsWith($"/{_storageZoneName}/") && !path.StartsWith($"{_storageZoneName}/"))
            {
                throw new BunnyCDNStorageException(
                    $"Path validation failed. File path must begin with /{_storageZoneName}/.");
            }

            path = path.Replace("\\", "/");
            
            if (isDirectory != null)
            {
                if (isDirectory.Value)
                {
                    if (!path.EndsWith("/"))
                    {
                        path = path + "/";
                    }
                }
                else
                {
                    if (path.EndsWith("/") && path != "/")
                    {
                        throw new BunnyCDNStorageException("The requested path is invalid.");
                    }
                }
            }
            
            // Remove double slashes
            while (path.Contains("//"))
            {
                path.Replace("//", "/");
            }

            // Remove the starting slash
            if (path.StartsWith("/"))
            {
                path = path.Remove(0, 1);
            }

            return path;
        }

        #endregion
    }
}