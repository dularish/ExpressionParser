using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VariablesManagementDemoApp
{
    public class Graph<T> where T : IEquatable<T>
    {
        Dictionary<T, List<T>> _adjacencyList = new Dictionary<T, List<T>>();

        public void AddEdge(T source, T destination)
        {
            if(!_adjacencyList.ContainsKey(source) || !_adjacencyList[source].Contains(destination))
            {
                if (!_adjacencyList.ContainsKey(source))
                {
                    _adjacencyList.Add(source, new List<T>() { destination });
                }
                else
                {
                    _adjacencyList[source].Add(destination);
                }
            }
        }

        public void DeleteEdge(T source, T destination)
        {
            if (_adjacencyList.ContainsKey(source))
            {
                _adjacencyList[source].Remove(destination);
            }
        }

        public Stack<T> TopologicalSort(T varToStart)
        {
            Dictionary<T, bool> visitedDict = _adjacencyList.ToDictionary(s => s.Key, s => false);
            Stack<T> resultStack = new Stack<T>();
            Stack<T> dfsRes = getDFS(visitedDict, varToStart, resultStack);

            return dfsRes;
        }

        private Stack<T> getDFS(Dictionary<T, bool> visitedDict, T dfsNode, Stack<T> resultStack)
        {
            visitedDict[dfsNode] = true;//Preventing looping again the childItems//Think of a case regarding this apart from cycles
            if (_adjacencyList.ContainsKey(dfsNode))
            {
                foreach (var childItem in _adjacencyList[dfsNode])
                {
                    if (!visitedDict.ContainsKey(childItem))
                    {
                        visitedDict.Add(childItem, false);
                    }

                    if (!visitedDict[childItem])
                    {
                        getDFS(visitedDict, childItem, resultStack);
                    }
                }
            }

            resultStack.Push(dfsNode);

            return resultStack;
        }

        public void DeleteAllEdgesTo(T destNode)
        {
            foreach (var _listItem in _adjacencyList.Keys)
            {
                if (!_listItem.Equals(destNode))
                {
                    _adjacencyList[_listItem].Remove(destNode);
                }
            }
        }
    }
}
