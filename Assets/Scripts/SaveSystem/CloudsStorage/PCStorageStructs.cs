using System;

namespace PCStorage.Model
{
    public class PCVideoData
    {
        public string Path_Audio;
        public string Path_Preview;
        public PCInfo Info;
        public string Path_Frames;
        
        public PCVideoData()
        {
            
        }

        public PCVideoData(string path)
        {
            
        }
    }

    [Serializable]
    public struct PCInfo
    {
        public int version;
        public string title;
        public string description;
        
        /// <summary>
        /// DateTime format
        /// </summary>
        public string length;
        public ulong framesCount;
        public int bitRate;
        
        public ulong size;
        
        public string owner;
        /// <summary>
        /// DateTime format
        /// </summary>
        public string creationDate;
    }
}