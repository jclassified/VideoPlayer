using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;
using BunnyCDN;
using BunnyCDN.Controller;
using PCStorage.Model;
using PointCloud.Player;
using UnityEngine;
using UnityEngine.Networking;
using Debug = UnityEngine.Debug;

public static class VideoManager
{
    #region ATTENTION!!! First load Oculus App cruch(!) HARDCODE ZONE. BE CAREFUL, YOU CAN LOSE YOUR EYES AND MIND HERE
    
    public static async Task LoadAllVideos(IProgress<float> progress)
    {
        BunnyCDNUser bunnyUser = new BunnyCDNUser();
        progress.Report(0f);

        const string path_Egor = "2021.11.26/Egor_Greeting";
        const string path_NikHead = "2021.11.26/Nikita_Head";
        const string path_OlegHead = "2021.11.26/Oleg_Head";
        const string path_Vitalya = "2021.11.26/Vitalya_PlayMusic";

        string[] egor = Saver.Binary_GetFrames($"{BunnyCDNStorage.path_StudioVideos}/{path_Egor}");
        string[] nick = Saver.Binary_GetFrames($"{BunnyCDNStorage.path_StudioVideos}/{path_NikHead}");
        string[] oleg = Saver.Binary_GetFrames($"{BunnyCDNStorage.path_StudioVideos}/{path_OlegHead}");
        string[] vitalya = Saver.Binary_GetFrames($"{BunnyCDNStorage.path_StudioVideos}/{path_Vitalya}");
        
        bool EgorExist = egor != null && egor.Length > 0;
        bool NickExist = nick != null && nick.Length > 0;
        bool OlegExist = oleg != null && oleg.Length > 0;
        bool VitalyaExist = vitalya != null && vitalya.Length > 0;
        
        
        #region Collect data
        //----------Egor cyber-punk

        List<StorageObject> videos_Egor = (!EgorExist) ? await bunnyUser.GetStorageObjectsAsync($"{BunnyCDNStorage.path_StudioVideos}/{path_Egor}/Frames") : new List<StorageObject>();
        /*List<StorageObject> videos_NikHead = (!NickExist) ? await bunnyUser.GetStorageObjectsAsync($"studio_videos/{path_NikHead}/Frames") : new List<StorageObject>();
        List<StorageObject> videos_OlegHead = (!OlegExist) ? await bunnyUser.GetStorageObjectsAsync($"studio_videos/{path_OlegHead}/Frames") : new List<StorageObject>();
        List<StorageObject> videos_Vita = (!VitalyaExist) ? await bunnyUser.GetStorageObjectsAsync($"studio_videos/{path_Vitalya}/Frames") : new List<StorageObject>();
        */
        float totalCount = videos_Egor.Count /*+ videos_NikHead.Count + videos_OlegHead.Count + videos_Vita.Count*/;
        
        float currentLoaded = 0f;
        #endregion
        
        /*//Load Info
        await bunnyUser.DownloadObjectAsync(BunnyCDNStorage.VideoType.Studio,
            "2021.11.16/Egor_CyberPunk/info.txt",
            $"{Saver.CreatePath_CloudsBinary(Saver.GeneralPath.Loaded, "2021.11.16/Egor_CyberPunk")}/info.txt");
                
        //Load Audio
        for (int i = 0; i < audios_Egor.Count; i++)
        { 
            await bunnyUser.DownloadObjectAsync(BunnyCDNStorage.VideoType.Studio,
                $"2021.11.16/Egor_CyberPunk/Audio/{audios_Egor[i].ObjectName}",
                $"{Saver.CreatePath_CloudsBinaryAudio(Saver.GeneralPath.Loaded, "2021.11.16/Egor_CyberPunk")}/{audios_Egor[i].ObjectName}");
        }*/
        
        //Load Egor
        for (int i = 0; i < videos_Egor.Count; i++)
        { 
            await bunnyUser.DownloadObjectAsync($"{BunnyCDNStorage.path_StudioVideos}/{path_Egor}/Frames/{videos_Egor[i].ObjectName}",
                Saver.CreatePath_CloudsBinaryFrames($"{BunnyCDNStorage.path_StudioVideos}/{path_Egor}/{videos_Egor[i].ObjectName}"));
            currentLoaded += 1f;
            float progressValue = currentLoaded / totalCount;
            progress.Report(progressValue);
        }
        /*//Load NikHead
        for (int i = 0; i < videos_NikHead.Count; i++)
        { 
            await bunnyUser.DownloadObjectAsync(BunnyCDNStorage.VideoType.Studio,
                $"{path_NikHead}/Frames/{videos_NikHead[i].ObjectName}",
                $"{Saver.CreatePath_CloudsBinaryFrames(Saver.GeneralPath.Loaded, path_NikHead)}/{videos_NikHead[i].ObjectName}");
            
            currentLoaded += 1f;
            float progressValue = currentLoaded / totalCount;
            progress.Report(progressValue);
        }
        //Load OlegHead
        for (int i = 0; i < videos_OlegHead.Count; i++)
        { 
            await bunnyUser.DownloadObjectAsync(BunnyCDNStorage.VideoType.Studio,
                $"{path_OlegHead}/Frames/{videos_OlegHead[i].ObjectName}",
                $"{Saver.CreatePath_CloudsBinaryFrames(Saver.GeneralPath.Loaded, path_OlegHead)}/{videos_OlegHead[i].ObjectName}");
            
            currentLoaded += 1f;
            float progressValue = currentLoaded / totalCount;
            progress.Report(progressValue);
        }
        //Load Vitalya
        for (int i = 0; i < videos_Vita.Count; i++)
        { 
            await bunnyUser.DownloadObjectAsync(BunnyCDNStorage.VideoType.Studio,
                $"{path_Vitalya}/Frames/{videos_Vita[i].ObjectName}",
                $"{Saver.CreatePath_CloudsBinaryFrames(Saver.GeneralPath.Loaded, path_Vitalya)}/{videos_Vita[i].ObjectName}");
            
            currentLoaded += 1f;
            float progressValue = currentLoaded / totalCount;
            progress.Report(progressValue);
        }*/
        bunnyUser.Dispose();
        progress.Report(1f);
    }
    #endregion
    
    #region Built in videos

    private const string bi_fnCDNPreloadVideos = "system/preloadvideos.txt";

    #endregion

    #region CDN Videos

    private static List<string> cdn_Videos;
    internal static List<string> CDN_Videos => cdn_Videos;
    internal static async Task CDN_GetList(Action<bool> updateCompleted)
    {
        BunnyCDNUser bunny = new BunnyCDNUser();
        var videos = await bunny.GetVideosListAsync();
        bunny.Dispose();

        cdn_Videos = new List<string>();
        for (int i = 0; i < videos.Count; i++)
        {
            cdn_Videos.Add(videos[i].ObjectName);
        }
        updateCompleted.Invoke(true);
    }
    internal static async Task CDN_LoadVideo(string shortPath, IProgress<DoubleInt> progress)
    {
        BunnyCDNUser bunny = new BunnyCDNUser();
        //------Collect files
        List<StorageObject> videosList =
            await bunny.GetStorageObjectsAsync($"{BunnyCDNStorage.path_StudioVideos}/{shortPath}/Frames");
        List<StorageObject> audiosList =
            await bunny.GetStorageObjectsAsync($"{BunnyCDNStorage.path_StudioVideos}/{shortPath}/Audio");
        int totalCount = videosList.Count + audiosList.Count + 2;
        int currentLoaded = 0;
        string locPathFrame = Saver.CreatePath_CloudsBinaryFrames(shortPath);
        string locPathAudio = Saver.CreatePath_CloudsBinaryAudio(shortPath); 
        
        //-----Load info file
        await bunny.DownloadObjectAsync($"{BunnyCDNStorage.path_StudioVideos}/{shortPath}/{Saver.fName_PCInfo}",
            Saver.CreatePath_CloudsBinaryInfo($"{shortPath}"));
        currentLoaded++;
        progress.Report(new DoubleInt(currentLoaded, totalCount));
        
        await bunny.DownloadObjectAsync($"{BunnyCDNStorage.path_StudioVideos}/{shortPath}/{Saver.fName_PCPreview}",
            Saver.CreatePath_CloudsBinaryPreview($"{shortPath}"));
        currentLoaded++;
        progress.Report(new DoubleInt(currentLoaded, totalCount));

        //-----Load frames
        Parallel.For(0, videosList.Count,  async i =>
        {
            await bunny.DownloadObjectAsync(
                $"{BunnyCDNStorage.path_StudioVideos}/{shortPath}/{Saver.dirName_Frames}/{videosList[i].ObjectName}",
                $"{locPathFrame}/{videosList[i].ObjectName}");
            currentLoaded ++;
            progress.Report(new DoubleInt(currentLoaded, totalCount));
        });

        //-----Load audios
        for (int i = 0; i < audiosList.Count; i++)
        {
            await bunny.DownloadObjectAsync(
                $"{BunnyCDNStorage.path_StudioVideos}/{shortPath}/{Saver.dirName_Audio}/{audiosList[i].ObjectName}",
                $"{locPathAudio}/{audiosList[i].ObjectName}");
            currentLoaded ++;
            progress.Report(new DoubleInt(currentLoaded, totalCount));
        }

        bunny.Dispose();
        progress.Report(new DoubleInt(totalCount, totalCount));
    }
    internal static async Task CDN_UpdateInfo(string shortPath)
    {
        BunnyCDNUser bunny = new BunnyCDNUser();
        await bunny.DownloadObjectAsync($"{BunnyCDNStorage.path_StudioVideos}/{shortPath}/{Saver.fName_PCInfo}",
            Saver.CreatePath_CloudsBinaryInfo($"{shortPath}"));
        bunny.Dispose();
    }
    
    internal static async Task CDN_LoadPreview(string shortPath)
    {
        BunnyCDNUser bunny = new BunnyCDNUser();
        await bunny.DownloadObjectAsync($"{BunnyCDNStorage.path_StudioVideos}/{shortPath}/{Saver.fName_PCPreview}",
            Saver.CreatePath_CloudsBinaryPreview($"{shortPath}"));
        bunny.Dispose();
    }

    #endregion

    #region Video playeres controls

    private static CloudPlayer _currentPlayer;
    private static string _currentVideo;
    
    internal static void VPC_RegisterPlayer(CloudPlayer player)
    {
        _currentPlayer = player;
        VPC_PrepareVideoPlayer();
    }

    internal static void VPC_RegisterVideo(string shortPath)
    {
        _currentVideo = shortPath;
        VPC_PrepareVideoPlayer();
    }

    public static void VPC_ClearPlayer()
    {
        if (_currentPlayer != null)
        {
            _currentPlayer.Dispose();
            _currentPlayer = null;
        }
        _currentVideo = null;
    }

    private static void VPC_PrepareVideoPlayer()
    {
        if (_currentPlayer != null && _currentVideo != null)
        {
            _currentPlayer.Stop();
            _currentPlayer.Init(_currentVideo);
        }
    }

    #endregion
    
    #region Utils

    internal static async Task<PCInfo> GetVideoInfo(string shortPath)
    {
        bool exists = Saver.Binary_InfoExists(shortPath);
        if (!exists)
        {
            await CDN_UpdateInfo(shortPath);
        }

        string docPath = Saver.CreatePath_CloudsBinaryInfo(shortPath);
        string data = File.ReadAllText(docPath);
        PCInfo info = JsonUtility.FromJson<PCInfo>(data);
        return info;
    }

    internal static IEnumerator GetVideoPreview(string shortPath, Action<Sprite> result)
    {
        string fullPath = Saver.CreatePath_CloudsBinaryPreview(shortPath);
        bool exists = File.Exists(fullPath);
        if (!exists)
        {
            Task load = CDN_LoadPreview(shortPath);
            while (!load.IsCompleted)
            {
                yield return null;
            }
        }

        using (UnityWebRequest request = UnityWebRequestTexture.GetTexture(String.Format("file:///{0}", fullPath)))
        {
            yield return request.SendWebRequest();
            Texture texture = DownloadHandlerTexture.GetContent(request);
            texture.name = shortPath;
            result.Invoke(texture.ToSprite());
        }
    }

    internal static async Task<bool> CheckLoaded(string shortPath)
    {
        //Info file exists
        bool exists = Saver.Binary_InfoExists(shortPath);
        if (!exists) return false;

        //Correct frames count
        PCInfo curInfo = Saver.Binary_GetInfo(shortPath);
        uint framesCount = (uint)Saver.Binary_GetFramesCount(shortPath);
        if (framesCount != curInfo.framesCount) return false;

        //Update info file
        await CDN_UpdateInfo(shortPath);
        PCInfo newInfo = Saver.Binary_GetInfo(shortPath);
        if (newInfo.version != curInfo.version) return false;
        return true;
    }

    internal static IEnumerator GetAudio(string shortPath, Action<AudioClip> result)
    {
        string pathAudio = Saver.CreatePath_CloudsBinaryAudio(shortPath);
        string webPathAudio = String.Format("file:///{0}", pathAudio);
        string[] audioFiles = Saver.GetFiles(pathAudio);

        if (audioFiles == null || audioFiles.Length < 1)
        {
            result.Invoke(null);
            yield break;
        }
        
        string webPathFile = String.Format("{0}/{1}", webPathAudio, audioFiles[0]);
        using (UnityWebRequest request = UnityWebRequestMultimedia.GetAudioClip(webPathFile, AudioType.UNKNOWN))
        {
            yield return request.SendWebRequest();
            AudioClip clip = DownloadHandlerAudioClip.GetContent(request);
            clip.name = audioFiles[0];
            result.Invoke(clip);
        }
    }

    #endregion
}
