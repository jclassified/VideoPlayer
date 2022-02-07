using System;
using System.Runtime.InteropServices;
using Unity.Collections;
using UnityEngine;

namespace PointCloud.Player
{
    internal class VFXTexture
    {
        internal Texture tex_Cloud { get; }
        internal Texture tex_Color { get; }
        
        private int width;
        private int _maxPointCount;
        
        private int kernel;
        private ComputeShader VFXShader;
        ComputeBuffer colorBuffer;
        ComputeBuffer frameBuffer;

        internal VFXTexture(ComputeShader cShader, int maxPointCount = 250000)
        {
            VFXShader = cShader;
            kernel = VFXShader.FindKernel("TransferFrame");

            _maxPointCount = maxPointCount;
            
            width = Mathf.CeilToInt(Mathf.Sqrt(_maxPointCount));
            VFXShader.SetInt("Width", width);
            
            tex_Cloud = new RenderTexture(width, width, 0, RenderTextureFormat.ARGBHalf)
                {enableRandomWrite = true, filterMode = FilterMode.Point};
            tex_Color = new RenderTexture(width, width, 0, RenderTextureFormat.ARGB32)
                {enableRandomWrite = true, filterMode = FilterMode.Point};
            
            frameBuffer = new ComputeBuffer((int) _maxPointCount * 3, sizeof(int), ComputeBufferType.Raw,
                ComputeBufferMode.SubUpdates);
        }

        internal void PointCloudToTexture(byte[] frame)
        {
            try
            {
                //-----Set frame array
                NativeArray<int> frameArray = frameBuffer.BeginWrite<int>(0, (int) _maxPointCount * 3); 
                int[] frameAr = new int[_maxPointCount * 3];
                int realSize = frame.Length / sizeof(int);
                IntPtr framePtr = Marshal.AllocHGlobal(frame.Length);
                Marshal.Copy(frame, 0, framePtr, frame.Length);
                Marshal.Copy(framePtr, frameAr, 0, realSize);
                Marshal.FreeHGlobal(framePtr);

                frameArray.CopyFrom(frameAr);
                frameBuffer.EndWrite<int>(_maxPointCount * 3);
                //-----------

                //-----Set ComputeBuffer fields
                VFXShader.SetTexture(kernel, "PositionMap", tex_Cloud);
                VFXShader.SetTexture(kernel, "ColorMap", tex_Color);

                VFXShader.SetInt("VertexCount", _maxPointCount);
                VFXShader.SetInt("PointLimit", realSize);

                VFXShader.SetBuffer(kernel, "FrameBuffer", frameBuffer);
                //-----------
                
                //------Process texture in ComputeBuffer
                VFXShader.Dispatch(kernel, width / 4, width / 4, 1);
            }
            catch (Exception e)
            {
                Debug.LogError(e);
            }
        } 
    }
}
