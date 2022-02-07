using UnityEngine;

public class RandomMaterial : MonoBehaviour
{
    [SerializeField] private Material[] _materials;
    [SerializeField] private float _duration = 1;
    
    private MeshRenderer[] _meshRenderers;
    private float _runningTime;
    
    private void Start()
    {
        _meshRenderers = GetComponentsInChildren<MeshRenderer>();
    }
    
    private void Update()
    {
        _runningTime += Time.deltaTime;
        
        if (_runningTime >= _duration)
        {
            for (var i = 0; i < _meshRenderers.Length; i++)
            {
                _meshRenderers[i].material = _materials[Random.Range(0, _materials.Length)];
            
            }
            
            _runningTime = 0;
        }
    }
}