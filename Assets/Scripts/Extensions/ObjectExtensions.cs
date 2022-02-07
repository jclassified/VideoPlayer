using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Runtime.Serialization.Formatters.Binary;
using UnityEngine;

internal static class ObjectExtensions
{
    internal static bool ToMatrix(this string inData, out Matrix4x4 matrix, char parser = ';')
    {
        string[] data = inData.Split(parser);
        matrix = new Matrix4x4();

        for (int i = 0; i < data.Length; i++)
        {
            bool success = float.TryParse(data[i], out float result);
            if (!success) return false;
            int row = i / 4;
            int colomn = i % 4;
            matrix[row, colomn] = result;
        }

        return true;
    }

    internal static string ToString(this Matrix4x4 inData, char parser = ';')
    {
        string data = string.Empty;
        
        data += parser + inData.m00.ToString();
        data += parser + inData.m01.ToString();
        data += parser + inData.m02.ToString();
        data += parser + inData.m03.ToString();
        data += parser + inData.m10.ToString();
        data += parser + inData.m11.ToString();
        data += parser + inData.m12.ToString();
        data += parser + inData.m13.ToString();
        data += parser + inData.m20.ToString();
        data += parser + inData.m21.ToString();
        data += parser + inData.m22.ToString();
        data += parser + inData.m23.ToString();
        data += parser + inData.m30.ToString();
        data += parser + inData.m31.ToString();
        data += parser + inData.m32.ToString();
        data += parser + inData.m33.ToString();

        return data;
    }
    internal static byte[] ToByteArray<T>(T obj)
    {
        if (obj == null)
            return null;
        BinaryFormatter bf = new BinaryFormatter();
        using (MemoryStream ms = new MemoryStream())
        {
            bf.Serialize(ms, obj);
            return ms.ToArray();
        }
    }

    internal static object FromByteArray(byte[] data)
    {
        if(data == null)
            return default;
        BinaryFormatter bf = new BinaryFormatter();
        using (MemoryStream ms = new MemoryStream(data))
        {
            object obj = bf.Deserialize(ms);
            return obj;
        }
    }
    internal static T FromByteArray<T>(byte[] data)
    {
        if(data == null)
            return default;

        data = data.Skip(1).ToArray();
        BinaryFormatter bf = new BinaryFormatter();
        using (MemoryStream ms = new MemoryStream(data))
        {
            object obj = bf.Deserialize(ms);
            return (T)obj;
        }
    }

    internal static byte[] AddForwardByte(byte[] data, byte[] addData)
    {
        byte[] result = new byte[data.Length + addData.Length];
        addData.CopyTo(result, 0);
        data.CopyTo(result, addData.Length);
        return result;
    }
    internal static byte[] AddForwardByte(byte[] data, byte addData)
    {
        byte[] result = new byte[data.Length + 1];
        data.CopyTo(result, 1);
        result[0] = addData;
        return result;
    }

    internal static byte[] IntPtrToArray(this IntPtr input, uint size)
    {
        byte[] output = new byte[size];
        Marshal.Copy(input, output, 0, (int)size);
        return output;
    }
    internal static IntPtr ArrayToIntPtr(this byte[] input)
    {
        IntPtr ptr = Marshal.AllocHGlobal(input.Length);
        Marshal.Copy(input, 0, ptr, input.Length);
        return ptr;
    }

    internal static Sprite ToSprite(this Texture tex)
    {
        Sprite sprite = Sprite.Create((Texture2D)tex, new Rect(0f, 0f, tex.width, tex.height), new Vector2(.5f, .5f), 72f);
        sprite.name = tex.name;
        return sprite;
    }
    internal static Sprite ToSprite(this Texture2D tex)
    {
        Sprite sprite = Sprite.Create(tex, new Rect(0f, 0f, tex.width, tex.height), new Vector2(.5f, .5f), 72f);
        sprite.name = tex.name;
        return sprite;
    }
}

internal struct DoubleInt
{
    internal int value1;
    internal int value2;

    internal DoubleInt(int val1, int val2)
    {
        value1 = val1;
        value2 = val2;
    }
}
