using System;
using PointCloud.Player;
using UnityEngine;
using UnityEngine.UI;

public class PlayerUI : UIPanel
{
    [Space, Header("Player fields")]
    [SerializeField] private CloudPlayer _player;

    [SerializeField] private Toggle tg_PlayState;
    private Image i_PlayState;
    [SerializeField] private Sprite sp_Play;
    [SerializeField] private Sprite sp_Pause;

    [SerializeField] private Slider sl_Timeline;

    private int totalTime;
    [SerializeField] private Text t_Time;

    private void Start()
    {
        base.Start();
        UpdateVideoData(null, EventArgs.Empty);
    }

    private void SetPlayState(object sender, bool isPlaying)
    {
        if (isPlaying)
        {
            i_PlayState.sprite = sp_Pause;
        }
        else
        {
            i_PlayState.sprite = sp_Play;
        }
        tg_PlayState.SetIsOnWithoutNotify(isPlaying);
    }
    private void SetProgress(object sender, float progress)
    {
        sl_Timeline.SetValueWithoutNotify(progress);
        //t_Time.text = 
    }

    private void UpdateVideoData(object sender, EventArgs eventArgs)
    {
        totalTime = (int)(_player.framesCount / _player.fps);
    }

    protected override void SetListeners()
    {
        i_PlayState = tg_PlayState.GetComponentInChildren<Image>();
        tg_PlayState.onValueChanged.AddListener(isOn =>
        {
            if (isOn)
            {
                _player.Play();
            }
            else
            {
                _player.Pause();
            }
            SetPlayState(_player, isOn);
        });
        sl_Timeline.onValueChanged.AddListener(value =>
        {
            _player.SetFrame(value);
        });
        _player.onProgress += SetProgress;
        _player.onPlay += SetPlayState;
        _player.OnVideoSet += UpdateVideoData;
    }

    protected override void RemoveListeners()
    {
        tg_PlayState.onValueChanged.RemoveAllListeners();
        _player.onProgress -= SetProgress;
        _player.onPlay -= SetPlayState;
    }
}
