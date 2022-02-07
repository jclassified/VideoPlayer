using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using PCStorage.Model;
using UnityEngine;

internal static class Saver
{
    //----------General fields
    private static string path_General = Application.persistentDataPath;
    internal static string Path_General => path_General;

    //----------Data fields

    #region Media

    private static string dirName_Media = "Media";
    internal static string Path_Media
    {
        get
        {
            string path = string.Format("{0}", path_General);
            if (!Directory.Exists(path)) Directory.CreateDirectory(path);
            path = string.Format("{0}/{1}", path_General, dirName_Media);
            return path;
        }
    }

    internal static string CreatePath_Media(string subfolder)
    {
        string path = string.Format("{0}/{1}", Path_Media, subfolder);
        return path;
    }
    internal static string CreatePath_Calibration(string subfolder)
    {
        string path = string.Format("{0}/{1}", Path_Media, subfolder);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        path = string.Format("{0}/{1}/{2}", Path_Media, subfolder, fName_Calibration);
        return path;
    }
    internal static string CreatePath_ColorCalibration(string subfolder)
    {
        string path = string.Format("{0}/{1}", Path_Media, subfolder);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        path = string.Format("{0}/{1}/{2}", Path_Media, subfolder, fName_ColorCalibration);
        return path;
    }
    internal static string CreatePath_Bounding(string subfolder)
    {
        string path = string.Format("{0}/{1}", Path_Media, subfolder);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        path = string.Format("{0}/{1}/{2}", Path_Media, subfolder, fName_Bounding);
        return path;
    }

    #endregion
    
    #region Stream Controller

    private static string dirName_Control = "Controller";
    internal static string Path_Controller
    {
        get
        {
            string path = string.Format("{0}/{1}", path_General, dirName_Control);
            if (!Directory.Exists(path)) Directory.CreateDirectory(path);
            return path;
        }
    }

    private static string fName_Calibration = "calibration.txt";
    internal static string Path_StreamCalibration = string.Format("{0}/{1}", Path_Controller, fName_Calibration);

    private static string fName_ColorCalibration = "color_calibration.json";
    internal static string Path_StreamColorCalibration = string.Format("{0}/{1}", Path_Controller, fName_ColorCalibration);

    private static string fName_Bounding = "bounding.txt";
    internal static string Path_StreamBounding = string.Format("{0}/{1}", Path_Controller, fName_Bounding);

    #endregion
    
    #region Clouds

    internal const string fName_PCInfo = "info.txt";
    internal const string fName_PCPreview = "preview.jpg";
    internal const string dirName_Frames = "Frames";
    internal const string dirName_Audio = "Audio";

    //----------Clouds .PCD
    private static string dirName_Clouds = "Clouds";
    internal static string Path_Clouds = string.Format("{0}/{1}", path_General, dirName_Clouds);
    internal static string Path_CloudsFrames = string.Format("{0}/{1}/{2}", path_General, dirName_Clouds, dirName_Frames);

    internal static string CreatePath_Clouds(string subfolder)
    {
        string path = string.Format("{0}/{1}", Path_Clouds, subfolder);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        return path;
    }
    internal static string CreatePath_CloudsFrames(string subfolder)
    {
        subfolder = subfolder.Replace('\\', '/');
        string path;
        if (!subfolder.Contains(path_General))
        {
            path = string.Format("{0}/{1}/{2}", Path_Clouds, subfolder, dirName_Frames);
        }
        else
        {
           path = string.Format("{0}/{1}", subfolder, dirName_Frames);
        }

        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        return path;
    }

    //----------Clouds .bin
    private static string dirName_CloudsBinary = "CloudsBinary";
    private static string path_CloudsBinaryLoaded = string.Format("{0}/{1}", path_General, dirName_CloudsBinary);

    /// <summary>
    /// Returns full path to all videos directories
    /// </summary>
    /// <param name="type">General local dir location</param>
    /// <returns>Full path to directory</returns>
    internal static string CreatePath_CloudsBinary()
    {
        if (!Directory.Exists(path_CloudsBinaryLoaded))
        {
            Directory.CreateDirectory(path_CloudsBinaryLoaded);
        }

        return path_CloudsBinaryLoaded;
    }

    /// <summary>
    /// Returns full path to root video's directory
    /// </summary>
    /// <param name="type">General local dir location</param>
    /// <param name="subfolder">Video dir identifier in format "date dir"/"video's name dir"</param>
    /// <returns>Full path to directory</returns>
    internal static string CreatePath_CloudsBinary(string subfolder)
    {
        int index = subfolder.IndexOf(dirName_CloudsBinary);
        if (index != -1)
        {
            subfolder = subfolder.Substring(index + dirName_CloudsBinary.Length + 1);
        }

        string path = string.Format("{0}/{1}", path_CloudsBinaryLoaded, subfolder);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        return path;
    }
    /// <summary>
    /// Returns full path to frames dir(!)
    /// </summary>
    /// <param name="type">General local dir location</param>
    /// <param name="subfolder">Video dir identifier in format "date dir"/"video's name dir"</param>
    /// <returns>Full path to directory</returns>
    internal static string CreatePath_CloudsBinaryFrames(string subfolder)
    {
        string path = CreatePath_CloudsBinary(subfolder);
        path = string.Format("{0}/{1}", path, dirName_Frames);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        return path;
    }
    /// <summary>
    /// Returns full path to audio dir(!)
    /// </summary>
    /// <param name="type">General local dir location</param>
    /// <param name="subfolder">Video dir identifier in format "date dir"/"video's name dir"</param>
    /// <returns>Full path to directory</returns>
    internal static string CreatePath_CloudsBinaryAudio(string subfolder)
    {
        string path = CreatePath_CloudsBinary(subfolder);
        path = string.Format("{0}/{1}", path, dirName_Audio);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        return path;
    }
    /// <summary>
    /// Returns full path to info file(!)
    /// </summary>
    /// <param name="type">General local dir location</param>
    /// <param name="subfolder">Video dir identifier in format "date dir"/"video's name dir"</param>
    /// <returns>Full path to file</returns>
    internal static string CreatePath_CloudsBinaryInfo(string subfolder)
    {
        string path = CreatePath_CloudsBinary(subfolder);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        path = string.Format("{0}/{1}", path, fName_PCInfo);
        return path;
    }
    /// <summary>
    /// Returns full path to preview image(!)
    /// </summary>
    /// <param name="type">General local dir location</param>
    /// <param name="subfolder">Video dir identifier in format "date dir"/"video's name dir"</param>
    /// <returns>Full path to file</returns>
    internal static string CreatePath_CloudsBinaryPreview(string subfolder)
    {
        string path = CreatePath_CloudsBinary(subfolder);
        if (!Directory.Exists(path)) Directory.CreateDirectory(path);
        path = string.Format("{0}/{1}", path, fName_PCPreview);
        return path;
    }

    #endregion
    
    //---------------------------//

    #region Media folder

    internal static string Media_CreateNew()
    {
        string currentDate = DateTime.Today.ToPath();
        bool dayExists = Media_IsDayExist(currentDate);

        DirectoryInfo dateDirectory;
        if (!dayExists)
        {
            DirectoryInfo directory = new DirectoryInfo(Path_Media);
            dateDirectory = directory.CreateSubdirectory(currentDate);
        }
        else
        {
            dateDirectory = new DirectoryInfo(String.Format("{0}/{1}", Path_Media, currentDate));
        }

        int recordsInDay = Media_GetDayCirclesCount(currentDate);
        DirectoryInfo recordDirectory = dateDirectory.CreateSubdirectory((recordsInDay + 1).ToString());
        return recordDirectory.FullName;
    }

    // ../Media/..?..
    internal static string[] Media_GetDays()
    {
        DirectoryInfo directory = new DirectoryInfo(Path_Media);
        if (!directory.Exists) directory.Parent.CreateSubdirectory(dirName_Media);
        DirectoryInfo[] childDirs = directory.GetDirectories();
        
        string[] dirs = new string[childDirs.Length];
        for (int i = 0; i < childDirs.Length; i++)
        {
            dirs[i] = childDirs[i].Name;
        }

        return dirs;
    }

    internal static bool Media_IsDayExist(string date = null)
    {
        if (date == null)
        {
            date = DateTime.Today.ToPath();
        }
        
        string[] recordedDays = Media_GetDays();
        for (int i = 0; i < recordedDays.Length; i++)
        {
            if (recordedDays[i].Contains(date))
            {
                return true;
            }
        }

        return false;
    }

    // ../Media/<Date>/..?..
    internal static string[] Media_GetDayCircles(string datePath = null)
    {
        if (datePath == null) datePath = DateTime.Today.ToPath();
        string path = datePath;
        if (!datePath.Contains(dirName_Media)) path = String.Format("{0}/{1}", Path_Media, datePath);
        
        DirectoryInfo directory = new DirectoryInfo(path);
        DirectoryInfo[] childDirs = directory.GetDirectories();

        string[] dirs = new string[childDirs.Length];
        for (int i = 0; i < childDirs.Length; i++)
        {
            dirs[i] = childDirs[i].Name;
        }

        return dirs;
    }
    internal static string[] Media_GetDayCircles(DateTime date)
    {
        string path = String.Format("{0}/{1}", Path_Media, date.ToPath());
        
        DirectoryInfo directory = new DirectoryInfo(path);
        DirectoryInfo[] childDirs = directory.GetDirectories();

        string[] dirs = new string[childDirs.Length];
        for (int i = 0; i < childDirs.Length; i++)
        {
            dirs[i] = childDirs[i].Name;
        }

        return dirs;
    }

    internal static int Media_GetDayCirclesCount(string datePath)
    {
        string path = datePath;
        if (!datePath.Contains(dirName_Media)) path = String.Format("{0}/{1}", Path_Media, datePath);
        
        DirectoryInfo directory = new DirectoryInfo(path);
        DirectoryInfo[] childDirs = directory.GetDirectories();

        return childDirs.Length;
    }
    internal static int Media_GetDayCirclesCount(DateTime date)
    {
        string path = String.Format("{0}/{1}", Path_Media, date.ToPath());
        
        DirectoryInfo directory = new DirectoryInfo(path);
        DirectoryInfo[] childDirs = directory.GetDirectories();

        return childDirs.Length;
    }
    
    // ../Media/<Date>/<RecordIndex>/..?..
    internal static string[] Media_GetFiles(string subFolder)
    {
        if (!subFolder.Contains(dirName_Media)) subFolder = string.Format("{0}/{1}", Path_Media, subFolder);
        DirectoryInfo directory = new DirectoryInfo(subFolder);
        string[] videoExtensions = VideoCompressions.GetAllExtensions();
        FileInfo[] files = directory.GetFiles();
        List<string> availableFiles = new List<string>();
        for (int i = 0; i < files.Length; i++)
        {
            string fileName = files[i].Name;
            for (int j = 0; j < videoExtensions.Length; j++)
            {
                if (fileName.Contains(videoExtensions[j]))
                    availableFiles.Add(fileName);
            }
        }

        return availableFiles.ToArray();
    }

    internal static void CopyAudio(string sourcePath, string targetPath)
    {
        string[] sourceAudio = GetFiles(sourcePath, AudioFormats.mp3.ToString());
        if (sourceAudio == null || sourceAudio.Length < 1) return;

        sourcePath = string.Format("{0}/{1}", sourcePath, sourceAudio[0]);
        targetPath = string.Format("{0}/{1}", targetPath, sourceAudio[0]);
        File.Copy(sourcePath, targetPath, true);
    }
    
    //---------------------------//
    

    #endregion

    #region Clouds folder

    internal static string Clouds_GetFullPath(string shortPath)
    {
        return String.Format("{0}/{1}", Path_Clouds, shortPath);
    }
    internal static string Clouds_CreateNew(DateTime date, string processTitle = null)
    {
        string sDate = date.ToPath();
        bool dayExists = Clouds_IsDayExist(sDate);

        DirectoryInfo dateDirectory;
        if (!dayExists)
        {
            DirectoryInfo directory = new DirectoryInfo(Path_Clouds);
            dateDirectory = directory.CreateSubdirectory(sDate);
        }
        else
        {
            dateDirectory = new DirectoryInfo(String.Format("{0}/{1}", Path_Clouds, sDate));
        }

        DirectoryInfo processDir;
        if (processTitle == null)
        {
            int processedInDate = Clouds_GetDaysCount(sDate);
            processDir = dateDirectory.CreateSubdirectory((processedInDate + 1).ToString());
        }
        else
        {
            processDir = dateDirectory.CreateSubdirectory(processTitle);
        }

        return processDir.FullName;
    }
    internal static bool Clouds_IsDayExist(string date = null)
    {
        if (date == null)
        {
            date = DateTime.Today.ToPath();
        }
        
        string[] processedDays = Clouds_GetDays();
        for (int i = 0; i < processedDays.Length; i++)
        {
            if (processedDays[i].Contains(date))
            {
                return true;
            }
        }

        return false;
    }
    internal static string[] Clouds_GetDays()
    {
        DirectoryInfo directory = new DirectoryInfo(Path_Clouds);
        if (!directory.Exists) directory.Parent.CreateSubdirectory(dirName_Clouds);
        DirectoryInfo[] childDirs = directory.GetDirectories();
        
        string[] dirs = new string[childDirs.Length];
        for (int i = 0; i < childDirs.Length; i++)
        {
            dirs[i] = childDirs[i].Name;
        }

        return dirs;
    }
    internal static int Clouds_GetDaysCount(string datePath)
    {
        string path = datePath;
        if (!datePath.Contains(dirName_Clouds)) path = String.Format("{0}/{1}", Path_Clouds, datePath);
        
        DirectoryInfo directory = new DirectoryInfo(path);
        DirectoryInfo[] childDirs = directory.GetDirectories();

        return childDirs.Length;
    }
    internal static string[] Clouds_GetDayCircles(string datePath = null)
    {
        if (datePath == null) datePath = DateTime.Today.ToPath();
        string path = datePath;
        if (!datePath.Contains(dirName_Clouds)) path = String.Format("{0}/{1}", Path_Clouds, datePath);
        
        DirectoryInfo directory = new DirectoryInfo(path);
        DirectoryInfo[] childDirs = directory.GetDirectories();

        string[] dirs = new string[childDirs.Length];
        for (int i = 0; i < childDirs.Length; i++)
        {
            dirs[i] = childDirs[i].Name;
        }

        return dirs;
    }
    internal static string[] Clouds_GetDayCircles(DateTime date)
    {
        string path = String.Format("{0}/{1}", Path_Clouds, date.ToPath());
        
        DirectoryInfo directory = new DirectoryInfo(path);
        DirectoryInfo[] childDirs = directory.GetDirectories();

        string[] dirs = new string[childDirs.Length];
        for (int i = 0; i < childDirs.Length; i++)
        {
            dirs[i] = childDirs[i].Name;
        }

        return dirs;
    }
    internal static string[] Clouds_GetFiles(string path)
    {
        return GetFiles(CreatePath_Clouds(path), "pcd");
    }

    #endregion

    #region CloudsBinary folder
    internal static string[] Binary_GetDays()
    {
        string path = path_CloudsBinaryLoaded;
        DirectoryInfo directory = new DirectoryInfo(path);
        if (!directory.Exists) directory.Parent.CreateSubdirectory(dirName_CloudsBinary);
        DirectoryInfo[] childDirs = directory.GetDirectories();
        
        string[] dirs = new string[childDirs.Length];
        for (int i = 0; i < childDirs.Length; i++)
        {
            dirs[i] = childDirs[i].Name;
        }

        return dirs;
    }
    internal static string[] Binary_GetDayCircles(string datePath = null)
    {
        if (datePath == null) datePath = DateTime.Today.ToPath();
        string path = datePath;
        path = CreatePath_CloudsBinary(datePath);
        
        DirectoryInfo directory = new DirectoryInfo(path);
        DirectoryInfo[] childDirs = directory.GetDirectories();

        string[] dirs = new string[childDirs.Length];
        for (int i = 0; i < childDirs.Length; i++)
        {
            dirs[i] = childDirs[i].Name;
        }

        return dirs;
    }
    internal static string[] Binary_GetDayCircles(DateTime date)
    {
        return Binary_GetDayCircles(date.ToPath());
    }
    internal static string[] Binary_GetFrames(string path)
    {
        return GetFiles(CreatePath_CloudsBinaryFrames(path), "bin");
    }
    internal static string[] Binary_GetAudios(string path)
    {
        return GetFiles(CreatePath_CloudsBinaryFrames(path), "bin");
    }
    internal static int Binary_GetFramesCount(string path)
    {
        return GetFilesCount(CreatePath_CloudsBinaryFrames(path), "bin");
    }

    internal static PCInfo Binary_GetInfo(string path)
    {
        bool exists = File.Exists(CreatePath_CloudsBinaryInfo(path));
        if (!exists)
        {
            Binary_SetInfo(path);
        }

        string rawData = File.ReadAllText(CreatePath_CloudsBinaryInfo(path));
        PCInfo info = JsonUtility.FromJson<PCInfo>(rawData);
        return info;
    }
    internal static bool Binary_InfoExists(string path)
    {
        bool exists = File.Exists(CreatePath_CloudsBinaryInfo(path));
        return exists;
    }
    internal static void Binary_SetInfo(string path)
    {
        PCInfo newInfo = new PCInfo();

        string[] parsedPath = path.Split('/');
        newInfo.version = 0;
        newInfo.title = parsedPath[parsedPath.Length - 1];
        newInfo.description = "";
        newInfo.framesCount = (ulong)Binary_GetFrames(path).Length / 2;
        newInfo.bitRate = 30;

        newInfo.size = 0;
        newInfo.owner = "";
        newInfo.creationDate = DateTime.Now.ToString();
        
        Binary_SetInfo(path, newInfo);
    }

    internal static void Binary_SetInfo(string path, PCInfo info)
    {
        #region Recalc length

        uint frameTime = (uint)Mathf.RoundToInt(1000 / info.bitRate);
        ulong lMs = (ulong)(info.framesCount * frameTime);
        int lH = (int)(lMs / (1000 * 60 * 60));
        lMs -= (ulong)(lH * 1000 * 60 * 60);
        
        int lM = (int)(lMs / (1000 * 60));
        lMs -= (ulong)(lM * 1000 * 60);
        
        int lS = (int)(lMs / 1000);
        lMs -= (ulong)(lS * 1000);
        info.length = new DateTime(1, 1, 1, lH, lM, lS, (int) lMs).ToString();

        #endregion

        string setInfo = JsonUtility.ToJson(info);
        File.WriteAllText(CreatePath_CloudsBinaryInfo(path), setInfo);
    }

    internal static void Binary_CollectInfo(string path, ref PCInfo info)
    {
        info.framesCount = (ulong)Binary_GetFrames(path).Length / 2;
    }

    #endregion
    
    //---------------------------//

    #region Local save system
    //----------Calibration----------//
    //TODO EGOR : save system calibration and bounding

    //---------- Color calibration ----------//
    internal static void ColorCalibration_Save(string data)
    {
        File.WriteAllText(Path_StreamColorCalibration, data);
    }

    internal static string ColorCalibration_Get()
    {
        if (!File.Exists(Path_StreamColorCalibration)) return null;
        string text = File.ReadAllText(Path_StreamColorCalibration);
        return text;
    }

    #endregion
    
    //---------------------------//

    #region Utils

    internal static string[] GetFiles(string path, string ext = null)
    {
        bool dirExists = Directory.Exists(path);
        if (!dirExists) return null;
        DirectoryInfo directoryInfo = new DirectoryInfo(path);
        string extension = ext != null ? String.Format("*.{0}", ext) : "*.*";
        var ordered = directoryInfo.GetFiles(extension).Select(f => new FileInfo(f.ToString())).OrderBy(f => f.Name);
        string[] files = new string[ordered.Count()];
        int i = 0;
        foreach (var fileInfo in ordered)
        {
            files[i] = fileInfo.Name;
            i++;
        }
        return files;
    }

    /// <summary>
    /// Returns given files count with given extension. 
    /// </summary>
    /// <param name="path">Files path</param>
    /// <param name="ext">Extension of files, can be null</param>
    /// <returns></returns>
    internal static int GetFilesCount(string path, string ext = null)
    {
        bool dirExists = Directory.Exists(path);
        if (!dirExists) return 0;
        DirectoryInfo directoryInfo = new DirectoryInfo(path);
        string extension = ext != null ? String.Format("*.{0}", ext) : null;
        var ordered = directoryInfo.GetFiles(extension);
        return ordered.Length;
    }

    //----------Extensions----------
    internal static string ToPath(this DateTime date)
    {
        string path = String.Format("{0:0000}.{1:00}.{2:00}", date.Year, date.Month, date.Day);
        return path;
    }
    #endregion
}

//----------Supported file extensions----------
internal struct VideoCompressions
{
    internal static string MKV = ".mkv";

    internal static string[] GetAllExtensions()
    {
        FieldInfo[] fields = typeof(VideoCompressions).GetFields();
        string[] outData = new string[fields.Length];
        for (int i = 0; i < outData.Length; i++)
        {
            outData[i] = fields[i].GetValue(null) as string;
        }

        return outData;
    }
}
internal static class AudioCompressions
{
    internal static string[] GetAllExtensions()
    {
        Array values = Enum.GetValues(typeof(AudioFormats));
        string[] outData = new string[values.Length];
        foreach (var value in values)
        {
            outData[(int) value] = ((AudioFormats) value).ToString();
        }

        return outData;
    }
}
internal enum AudioFormats
{
    wav,
    zip,
    mp3
}