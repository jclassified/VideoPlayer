using System;
using UnityEngine;

public abstract class UIPanel : MonoBehaviour, IDisposable
{
    [Header("UIPanel fields")]
    [SerializeField] private RectTransform rt_InSafeArea;

    protected bool inited;

    internal bool Inited => inited;
    
    protected virtual void Start()
    {
        if (rt_InSafeArea == null)
        {
            rt_InSafeArea = GetComponent<RectTransform>();
        }
        
        ApplySafeArea();
        SetListeners();
        inited = true;
    }
    protected abstract void SetListeners();
    protected abstract void RemoveListeners();

    private void ApplySafeArea()
    {
        Rect safeRect = Screen.safeArea;

        Vector2 anchorMin = safeRect.position;
        Vector2 anchorMax = safeRect.position + safeRect.size;
        
        Rect pixelRect = GetComponentInParent<Canvas>().pixelRect;
        anchorMin.x /= pixelRect.width;
        anchorMin.y /= pixelRect.height;

        anchorMax.x /= pixelRect.width;
        anchorMax.y /= pixelRect.height;

        rt_InSafeArea.anchorMin = anchorMin;
        rt_InSafeArea.anchorMax = anchorMax;
    }

    public void Dispose()
    {
        RemoveListeners();
        Destroy(gameObject);
    }

    private void OnDestroy()
    {
        Dispose();
    }

    private void OnApplicationQuit()
    {
        Dispose();
    }
}
