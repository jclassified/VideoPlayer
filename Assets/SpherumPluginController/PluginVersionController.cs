using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class PluginVersionController : EditorWindow
{
    [MenuItem("Spherum/Plugins/Video player/Open latest release")]
    public static void CheckRelease()
    {
        Application.OpenURL("https://github.com/New-Reality-LLC/PCVideoPlayer/releases/latest");
    }
    [MenuItem("Spherum/Plugins/Video player/Load")]
    public static void UpdateVideoPlayer()
    {
        Application.OpenURL("https://github.com/New-Reality-LLC/PCVideoPlayer/releases/latest/download/PointCloudPlayer_v1.0.4.unitypackage");
    }
}