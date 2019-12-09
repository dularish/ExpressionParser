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

        public Stack<T> TopologicalSort(T varToStart, out bool isCyclicGraph)
        {
            Dictionary<T, GraphNodeVisitStatus> visitedDict = _adjacencyList.ToDictionary(s => s.Key, s => GraphNodeVisitStatus.NotVisisted);
            Stack<T> resultStack = new Stack<T>();
            Stack<T> dfsRes = getDFS(visitedDict, varToStart, resultStack, out bool isCyclicGraph_Inner);
            isCyclicGraph = isCyclicGraph_Inner;
            return dfsRes;
        }

        private Stack<T> getDFS(Dictionary<T, GraphNodeVisitStatus> visitedDict, T dfsNode, Stack<T> resultStack, out bool isCyclic)
        {
            visitedDict[dfsNode] = GraphNodeVisitStatus.InProcess;
            if (_adjacencyList.ContainsKey(dfsNode))
            {
                foreach (var childItem in _adjacencyList[dfsNode])
                {
                    if (!visitedDict.ContainsKey(childItem))
                    {
                        visitedDict.Add(childItem, GraphNodeVisitStatus.NotVisisted);
                    }

                    if (visitedDict[childItem] == GraphNodeVisitStatus.NotVisisted)
                    {
                        getDFS(visitedDict, childItem, resultStack, out bool isCyclic_Inner);
                    }

                    if(visitedDict[childItem] == GraphNodeVisitStatus.InProcess)
                    {
                        isCyclic = true;
                        return resultStack;
                    }
                }
            }

            visitedDict[dfsNode] = GraphNodeVisitStatus.Visited;
            resultStack.Push(dfsNode);

            isCyclic = false;
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

        public enum GraphNodeVisitStatus
        {
            NotVisisted = 0,
            InProcess = 1,
            Visited = 2
        }
    }
}
