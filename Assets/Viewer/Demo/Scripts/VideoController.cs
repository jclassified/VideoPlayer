using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using PointCloud.Player;
using UnityEditor;
using UnityEngine;
using UnityEngine.UI;

public class VideoController : UIPanel
{
    [Space, Header("VideoController fields")]
    [SerializeField] private Button b_UpdateList;

    //Videos listing
    [Space] 
    [SerializeField] private RectTransform rt_VideosList;
    [SerializeField] private VideoDesk defaultDesk;
    [SerializeField] private float deskWidth = 420f;
    private VideoDesk[] currentDesks;
    
    //Video player
    [Space] [SerializeField] private CloudPlayer player;
    protected override void Start()
    {
        base.Start();
        UpdateVideosList();
    }

    protected override void SetListeners()
    {
        b_UpdateList.onClick.AddListener(UpdateVideosList);
    }

    protected override void RemoveListeners()
    {
        b_UpdateList.onClick.RemoveListener(UpdateVideosList);
    }

    private void UpdateVideosList()
    {
        VideoManager.CDN_GetList(success => UpdateVideosCompleted(success));
    }

    private void UpdateVideosCompleted(bool success)
    {
        if (currentDesks != null)
            for (int i = 0; i < currentDesks.Length; i++)
            {
                currentDesks[i].Dispose();
            }
        List<string> videos = VideoManager.CDN_Videos;
        currentDesks = new VideoDesk[videos.Count];
        for (int i = 0; i < videos.Count; i++)
        {
            if (videos[i].Length < 13) continue;
            rt_VideosList.sizeDelta += new Vector2(deskWidth, 0);
            
            //TODO <Video player>: load video desk from Addressables
            GameObject newDesk = Instantiate(defaultDesk.gameObject, rt_VideosList);
            currentDesks[i] = newDesk.GetComponent<VideoDesk>();
            currentDesks[i].Init(videos[i]);
        }
    }
}
#if UNITY_EDITOR
[CustomEditor(typeof(VideoController)), CanEditMultipleObjects]
public class VideoControllerEditor : Editor
{
    public override void OnInspectorGUI()
    {
        DrawDefaultInspector();
        VideoController script = (VideoController)target;
        if (GUILayout.Button("Open videos folder"))
        { 
            EditorUtility.OpenWithDefaultApp(Saver.CreatePath_CloudsBinary());
        }
    }
}
#endif
