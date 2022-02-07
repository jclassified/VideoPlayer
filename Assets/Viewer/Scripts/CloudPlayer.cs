using System;
using PCStorage.Model;
using UnityEngine;

namespace PointCloud.Player
{
    //[ExecuteAlways]
    public class CloudPlayer : MonoBehaviour
    {
        //-----Fields

        #region Data types

        internal enum InitType
        {
            Empty,
            FirstFrame,
            Play
        }

        internal enum LoopType
        {
            Once,
            Loop,
            PingPong
        }

        #endregion

        #region Main fields

        [Header("Run system settings")] [SerializeField]
        private InitType _initType;

        [SerializeField] internal LoopType loopType = default;

        internal PCInfo videoInfo { get; private set; }
        [SerializeField] private string path_Video;
        internal int fps { get; private set; }
        internal long framesCount { get; private set; }

        [SerializeField] private AudioSource audio;
        [SerializeField] private ViewerPC viewer;

        internal EventHandler OnVideoSet;

        #endregion

        #region Video state fields

        //Play state
        private bool _isPlaying;
        public bool playing
        {
            get { return _isPlaying; }
            private set
            {
                _isPlaying = value;
                if (onPlay != null)
                {
                    onPlay.Invoke(this, _isPlaying);
                }
            }
        }

        internal EventHandler<bool> onPlay;

        //Video playing progress in percents
        private float _currentProgress;
        internal float currentProgress
        {
            get => _currentProgress;
            private set
            {
                _currentProgress = value;
                if (onProgress != null)
                {
                    onProgress.Invoke(this, _currentProgress);
                }
            }
        }

        internal EventHandler<float> onProgress;

        //In time
        private float startTime = 0f;
        private float elapsedTime = 0f;

        //Video playing progress in frames
        private float _currentFrame = 0;
        private float currentFrame
        {
            get { return _currentFrame; }
            set
            {
                ChangeFrame(value);
                elapsedTime = _currentFrame / fps;
            }
        }

        #endregion 
        
        //-----Methods
        #region Main Controls

        public void Init(string localPath = null)
        {
            path_Video = localPath ?? path_Video;
            AnalyzeVideo();
            viewer.Processed_Path = path_Video;

            viewer.Init(path_Video);

            audio.loop = false;
            audio.playOnAwake = false;
            audio.Stop();

            _currentFrame = -1f;

            StartCoroutine(VideoManager.GetAudio(path_Video, clip =>
            {
                if (clip != null)
                {
                    audio.clip = clip;
                }

                switch (_initType)
                {
                    case InitType.Empty:
                        break;
                    case InitType.FirstFrame:
                        FirstFrame();
                        break;
                    case InitType.Play:
                        Play();
                        break;
                    default:
                        break;
                }
            }));
        }

        public void Dispose()
        {
            Stop();
            viewer.Dispose();
            audio.clip = null;
        }

        #endregion

        #region Play controls

        public void Play()
        {
            LoadFrame(-1, currentFrame);
            playing = true;
            startTime = Time.realtimeSinceStartup - elapsedTime;

            audio.Play();
        }

        public void Pause()
        {
            playing = false;
            elapsedTime = Time.realtimeSinceStartup - startTime;
            audio.Pause();
        }

        public void Stop()
        {
            playing = false;
            elapsedTime = 0f;

            FirstFrame();
            audio.Stop();
            currentProgress = 0;
        }

        public void LastFrame()
        {
            if (framesCount >= 1)
            {
                currentFrame = framesCount - 1;
                return;
            }
        }

        public void FirstFrame()
        {
            currentFrame = 0;
        }

        public void NextFrame()
        {
            currentFrame++;
        }

        public void PreviousFrame()
        {
            currentFrame++;
        }

        public void SetFrame(long index)
        {
            currentFrame = index;
        }

        public void SetFrame(float progress)
        {
            currentFrame = (long)(framesCount * progress);
        }

        #endregion

        #region Change frame types

        private void ChangeFrameOnce(float value)
        {
            if (value < 0)
                value = 0;
            if (value < framesCount)
            {
                var last = _currentFrame;
                _currentFrame = value;
                LoadFrame(last, value);
            }
            else
                Stop();
        }

        private void ChangeFrameLoop(float value)
        {
            if (value >= framesCount)
                value %= framesCount;
            else if (value < 0)
                value += framesCount;
            var last = _currentFrame;
            _currentFrame = value;
            LoadFrame(last, value);
        }

        private void ChangeFramePingPong(float value)
        {
            value = Mathf.PingPong(value, framesCount);
            var last = _currentFrame;
            _currentFrame = value;
            LoadFrame(last, value);
        }

        private bool ChangeFrame(float value)
        {
            if (_currentFrame != value && framesCount > 0)
            {
                switch (loopType)
                {
                    case LoopType.Once:
                        ChangeFrameOnce(value);
                        break;
                    case LoopType.Loop:
                        ChangeFrameLoop(value);
                        break;
                    case LoopType.PingPong:
                        ChangeFramePingPong(value);
                        break;
                }

                return true;
            }

            return false;
        }

        #endregion
        
        #region System

        private void Start()
        {
            VideoManager.VPC_RegisterPlayer(this);
        }

        private void AnalyzeVideo()
        {
            videoInfo = Saver.Binary_GetInfo(path_Video);
            framesCount = (long)videoInfo.framesCount;
            fps = videoInfo.bitRate;

            if (OnVideoSet != null)
            {
                OnVideoSet.Invoke(this, EventArgs.Empty);
            }
        }

        private void LoadFrame(double lastFrame, double newFrame)
        {
            long lastF = Convert.ToInt64(lastFrame);
            long newF = Convert.ToInt64(newFrame);

            if (newF == 0 && _isPlaying)
            {
                audio.Stop();
                audio.Play();
            }

            if (lastF != newF)
            {
                viewer.SetFrame(newF);
                currentProgress = newF / (float)framesCount;
            }
        }

        private void Update()
        {
            if (playing)
            {
                currentFrame = (Time.realtimeSinceStartup - startTime) * fps;
            }
        }

        private void OnApplicationQuit()
        {
            Dispose();
        }

        private void OnDestroy()
        {
            Dispose();
        }

        #endregion
    }
}
