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
    
    [SerializeField] private Toggle tg_Loop;
    [SerializeField] private Slider sl_Timeline;
    
    private void Start()
    {
        base.Start();
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
        sl_Timeline.value = progress;
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
                i_PlayState.sprite = sp_Play;
            }
            SetPlayState(_player, isOn);
        });
        tg_Loop.onValueChanged.AddListener(isOn =>
        {
            if (isOn)
            {
                _player.loopType = CloudPlayer.LoopType.Loop;
            }
            else
            {
                _player.loopType = CloudPlayer.LoopType.Once;
            }
        });
        _player.onProgress += SetProgress;
        _player.onPlay += SetPlayState;
    }

    protected override void RemoveListeners()
    {
        tg_PlayState.onValueChanged.RemoveAllListeners();
        tg_Loop.onValueChanged.RemoveAllListeners();
        _player.onProgress -= SetProgress;
        _player.onPlay -= SetPlayState;
    }
}
