using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading.Tasks;
using BunnyCDN.Controller;
using PCStorage.Model;
using PointCloud.Player;
using UnityEngine;
using UnityEngine.UI;

public class VideoDesk : MonoBehaviour
{
    [SerializeField] private Toggle tg_Loaded;
    [SerializeField] private Text t_Name;
    [SerializeField] private Button b_OpenLoad;
    [SerializeField] private Image i_Preview;

    [SerializeField] private GameObject go_Load;
    [SerializeField] private Image i_Progress;
    [SerializeField] private Text t_Progress;
    private const string format_Progress = "{0}/{1}";
    
    [SerializeField] private CloudPlayer player;

    private PCInfo info;
    private bool loaded;
    private bool loading;

    private string path;

    internal void Init(string path)
    {
        this.path = path;
        LoadInfo();
    }

    private void LoadInfo()
    {
        go_Load.SetActive(false);

        VideoManager.GetVideoInfo(path).ContinueWith(res =>
        {
            info = res.Result;
            t_Name.text = info.title;
            CheckLoadedState();
        }, TaskScheduler.FromCurrentSynchronizationContext());
        
        StartCoroutine(VideoManager.GetVideoPreview(path, sprite =>
        {
            i_Preview.sprite = sprite;
        }));
        

        b_OpenLoad.onClick.AddListener(() =>
        {
            if (loaded)
            {
                PlayVideo();
            }
            else
            {
                Load();
            }
        });
    }

    private void CheckLoadedState()
    {
        VideoManager.CheckLoaded(path).ContinueWith(res =>
        {
            loaded = res.Result;
            tg_Loaded.isOn = loaded;
        }, TaskScheduler.FromCurrentSynchronizationContext());
    }

    private void Load()
    {
        go_Load.SetActive(true);
        VideoManager.CDN_LoadVideo(path, new Progress<DoubleInt>(progress =>
        {
            t_Progress.text = String.Format(format_Progress, progress.value1, progress.value2);
            i_Progress.fillAmount = (float) progress.value1 / (float) progress.value2;
        })).ContinueWith(res =>
        {
            if (go_Load != null) go_Load.SetActive(false);
            CheckLoadedState();
        }, TaskScheduler.FromCurrentSynchronizationContext());
    }

    private void PlayVideo()
    {
        VideoManager.VPC_RegisterVideo(path);
    }

    internal void Dispose()
    {
        Destroy(gameObject);
    }
}
