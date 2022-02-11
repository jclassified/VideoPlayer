using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using UnityEngine;
using UnityEngine.VFX;
using Debug = UnityEngine.Debug;

namespace PointCloud.Player
{
    [ExecuteAlways]
    internal class ViewerPC : MonoBehaviour
    {
        [SerializeField] private VisualEffect vfx;

        [SerializeField] private ComputeShader computeShader;
        private VFXTexture vfxTexture;

        /// <summary>
        /// Don't overwrite field! It's only for InspectorGUI
        /// </summary>
        [HideInInspector] public string Processed_Path;
        [SerializeField] private string FullPath;

        [SerializeField] private string[] files;
        private class PCFrame
        {
            internal long Index { get; }
            internal byte[] frame { get; }
            internal PCFrame(long frameIndex, byte[] frameAr)
            {
                Index = frameIndex;
                frame = frameAr;
            }
        }

        private Dictionary<long, PCFrame> loadedDictionary;

        [SerializeField, Range(0f, 100f)] private float pointSize = .001f;
        [SerializeField, Range(0, 5)] private int maxBackUpSize;
        [SerializeField] private int maxPointCount = 250000;
        private int loadBias;
        private long currentFrame;
        
        private Thread loadThread;
        private Thread cleanThread;
        private Coroutine graphicRoutine;

        internal void Init(string path)
        {
            Dispose();
            vfxTexture = new VFXTexture(computeShader, maxPointCount);
            Processed_Path = path;
            FullPath = Saver.CreatePath_CloudsBinary(Processed_Path);
            files = Saver.Binary_GetFrames(Processed_Path);
            loadedDictionary = new Dictionary<long, PCFrame>();
            
            vfx.Play();
        }

        internal void Dispose()
        {
            vfxTexture = null;
            files = null;
            
            if (loadedDictionary != null)
            {
                loadedDictionary.Clear();
                loadedDictionary = null;
            }
            if (loadThread != null)
            {
                loadThread.Abort();
                loadThread = null;
            }

            if (cleanThread != null)
            {
                cleanThread.Abort();
                cleanThread = null;
            }

            if (graphicRoutine != null)
            {
                StopCoroutine(graphicRoutine);
                graphicRoutine = null;
            }
        }

        #region Playback methods

        internal void SetFrame(long frameId)
        {
            currentFrame = frameId;
            loadBias = 0;

            if (loadedDictionary == null)
            {
                loadedDictionary = new Dictionary<long, PCFrame>();
            }
            if (loadThread == null)
            {
                loadThread = new Thread(LocalLoadFrame);
                loadThread.Start();
            }

            if (cleanThread == null)
            {
                cleanThread = new Thread(ClearBackUp);
                cleanThread.Start();
            }

            if (graphicRoutine != null)
            {
                StopCoroutine(graphicRoutine);
            }

            graphicRoutine = StartCoroutine(SetFrame());
        }

        private IEnumerator SetFrame()
        {
            while (loadedDictionary != null && !loadedDictionary.ContainsKey(currentFrame))
            {
                yield return null;
            }
            PCFrame frame;
            lock (loadedDictionary)
            {
                loadedDictionary.TryGetValue(currentFrame, out frame);
            }
            vfxTexture.PointCloudToTexture(frame.frame);
            Texture tex_Cloud = vfxTexture.tex_Cloud;
            Texture tex_Color = vfxTexture.tex_Color;

            frame = null;

            vfx.Reinit();
            if (vfx.HasFloat("Size")) vfx.SetFloat("Size", pointSize);
            if (vfx.HasUInt("PointCount")) vfx.SetUInt("PointCount", maxPointCount);
            if (vfx.HasTexture("Position Map")) vfx.SetTexture("Position Map", tex_Cloud);
            if (vfx.HasTexture("Color Map")) vfx.SetTexture("Color Map", tex_Color);
        }
        private async void LocalLoadFrame()
        {
            while (true)
            {
                while (loadBias > maxBackUpSize)
                {
                    await Task.Delay(5);
                }
                long loadIndex = currentFrame + loadBias;
                
                if (loadIndex >= files.Length)
                {
                    loadIndex -= files.Length;
                }
                if (loadedDictionary.ContainsKey(loadIndex))
                {
                    loadBias++;
                    await Task.Delay(5);
                    continue;
                }
                string framePath = string.Format("{0}/{1}/{2}", FullPath, Saver.dirName_Frames, files[loadIndex]);

                Task[] tasks = new Task[1];
                byte[] frameAr = null;
                Func<object, byte[]> loadFunc = LocalLoadBin;

                tasks[0] = Task<byte[]>.Factory.StartNew(loadFunc, framePath).ContinueWith(res =>
                {
                    frameAr = res.Result;
                });
                
                try
                {
                    Task.WaitAll(tasks);
                }
                catch (AggregateException e)
                {
                    Debug.LogError($"Can't load data: {e.InnerExceptions}");
                    throw;
                }
                PCFrame newFrame = new PCFrame(loadIndex, frameAr);

                lock (loadedDictionary)
                {
                    loadedDictionary.Add(loadIndex, newFrame);
                }

                loadBias++;
            }
        }
        private byte[] LocalLoadBin(object inData)
        {
            string path = (string) inData;
            byte[] returnData;
            using (BinaryReader br = new BinaryReader(new FileStream(path, FileMode.Open, FileAccess.Read), Encoding.Default, false))
            {
                returnData = br.ReadBytes((int) br.BaseStream.Length);
            }

            return returnData;
        }

        private async void ClearBackUp()
        {
            while (true)
            {
                await Task.Delay(5);
                if (loadedDictionary == null || loadedDictionary.Count <= maxBackUpSize) continue;

                foreach (var loaded in loadedDictionary)
                {
                    if (loaded.Key < currentFrame)
                    {
                        lock (loadedDictionary)
                        {
                            loadedDictionary.Remove(loaded.Key);
                        }
                        break;
                    }
                }
            }
        }

        #endregion
    }
}
