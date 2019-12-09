using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace VariablesManagementDemoApp
{
    public class SimpleVar : INotifyPropertyChanged, IEquatable<SimpleVar>
    {
        private static bool _isDependencyTreeUpdationOnProgress = false;
        private static Graph<SimpleVar> _dependencyTreeGraph = new Graph<SimpleVar>();
        private static bool _isDependencyTreeMode = true;

        private string _strValue = string.Empty;
        private string _evaluatedValue = string.Empty;
        private DateTime _lastUpdatedDateTime = DateTime.Now;
        private string _name;
        private bool _isErrorHighlighted;
        private string _evaluationFailureMessage = "";

        private List<SimpleVar> _dependencyVariables = new List<SimpleVar>();

        public bool DependsOn(SimpleVar variableToCheckDependency)
        {
            return (variableToCheckDependency != null) && _dependencyVariables.Contains(variableToCheckDependency);
        }

        public string StrValue
        {
            get
            {
                return _strValue;
            }

            set
            {
                _strValue = value;
                onPropertyChanged();

                updateEvaluation(value, true);
            }
        }

        private void updateEvaluation(string value, bool isDependenciesToBeUpdated)
        {
            bool isEvaluationSuccess = evaluate(value, out string evaluatedResult, out string evaluationFailureMessage, out List<SimpleVar> dependencyVariables);
            if (isEvaluationSuccess)
            {
                _cachedVars[Name] = evaluatedResult;
            }
            else
            {
                _cachedVars.Remove(Name);
            }
            if (isDependenciesToBeUpdated)
            {
                clearDependencyVariables();
                addDependencyVariables(dependencyVariables);

                _dependencyTreeGraph.DeleteAllEdgesTo(this);
                foreach (var depVar in dependencyVariables)
                {
                    _dependencyTreeGraph.AddEdge(depVar, this);
                }
            }

            if (_isDependencyTreeMode)
            {
                if (!_isDependencyTreeUpdationOnProgress)
                {
                    Stack<SimpleVar> evaluationStack = _dependencyTreeGraph.TopologicalSort(this, out bool isCyclic);
                    if (isCyclic)
                    {
                        isEvaluationSuccess = false;
                        evaluationFailureMessage = "Cyclic referencing found by one of the dependencies";
                        _dependencyTreeGraph.DeleteAllEdgesTo(this);
                        _cachedVars.Remove(Name);
                    }
                    else
                    {
                        _isDependencyTreeUpdationOnProgress = true;
                        while (evaluationStack.Count > 0)
                        {
                            var topVar = evaluationStack.Pop();
                            topVar.updateEvaluation(topVar.StrValue, false);
                        }
                        _isDependencyTreeUpdationOnProgress = false;
                    }
                }
            }

            if (isEvaluationSuccess)
            {
                EvaluatedValue = evaluatedResult;
                EvaluationFailureMessage = string.Empty;
                IsErrorHighlighted = false;
            }
            else
            {
                EvaluatedValue = string.Empty;
                EvaluationFailureMessage = evaluationFailureMessage;
                IsErrorHighlighted = true;
            }
            LastUpdatedDateTime = DateTime.Now;

            if (!_isDependencyTreeMode)
            {
                ValueUpdated?.Invoke(this, EventArgs.Empty);
            }
        }

        private void addDependencyVariables(List<SimpleVar> dependencyVariables)
        {
            _dependencyVariables.AddRange(dependencyVariables);
            foreach (var variable in _dependencyVariables)
            {
                variable.NameChanged -= Variable_NameChanged;
                variable.NameChanged += Variable_NameChanged;

                variable.ValueUpdated -= Variable_ValueUpdated;
                variable.ValueUpdated += Variable_ValueUpdated;

                variable.ValueDestroyed -= Variable_ValueDestroyed;
                variable.ValueDestroyed += Variable_ValueDestroyed;
            }
        }

        private void Variable_ValueDestroyed(object sender, EventArgs e)
        {
            updateEvaluation(_strValue, true);
        }

        private void Variable_ValueUpdated(object sender, EventArgs e)
        {
            updateEvaluation(_strValue, false);
        }

        private void Variable_NameChanged(object sender, EventArgs e)
        {
            updateEvaluation(_strValue, true);
        }

        private void clearDependencyVariables()
        {
            foreach (var variable in _dependencyVariables)
            {
                variable.NameChanged -= Variable_NameChanged;

                variable.ValueUpdated -= Variable_ValueUpdated;

                variable.ValueDestroyed -= Variable_ValueDestroyed;
            }

            _dependencyVariables.Clear();
        }

        private ObservableCollection<SimpleVar> _centralizedVariablesCollection;
        private static Dictionary<string, string> _cachedVars = new Dictionary<string, string>();

        private bool evaluate(string value, out string evaluatedResult, out string evaluationFailureMessage, out List<SimpleVar> dependencyVariables)
        {
            Dictionary<string, string> dictForEvaluation = _centralizedVariablesCollection.ToDictionary(s => s.Name, s => s.StrValue);
            //var parsedOutput = MathematicalExpressionParser.parseAndEvaluateExpression(value, dictForEvaluation, this.Name);

            //Utilizing cachedVars collection
            foreach (var cachePair in _cachedVars)
            {
                dictForEvaluation[cachePair.Key] = cachePair.Value;
            }
            
            var parsedOutput = FParsecExpressionEvaluator.parseAndEvaluateExpressionExpressively(value, dictForEvaluation, this.Name);

            List<string> refVariables = parsedOutput.Item3.ToList();

            List<SimpleVar> dependencyVariablesLocal = new List<SimpleVar>();

            foreach (var variableName in refVariables.Distinct())
            {
                SimpleVar variableMatched = _centralizedVariablesCollection.Where(s => s.Name == variableName).FirstOrDefault();

                if (variableMatched == null)
                {
                    throw new Exception("Unexpected error! Variable " + variableName + " not found");
                }
                else
                {
                    dependencyVariablesLocal.Add(variableMatched);
                }
            }

            dependencyVariables = dependencyVariablesLocal;

            if (parsedOutput.Item1.IsEvaluationSuccess)
            {
                var evaluatedResultItem = (parsedOutput.Item1 as ExpParserConfigurables.ExpressionEvaluationResult.EvaluationSuccess).Item;

                string evaluatedResultString = string.Empty;

                if (evaluatedResultItem.IsDouble)
                {
                    evaluatedResultString = (evaluatedResultItem as ExpParserConfigurables.AllowedEvaluationResultTypes.Double).Item.ToString();
                }
                else if (evaluatedResultItem.IsString)
                {
                    evaluatedResultString = (evaluatedResultItem as ExpParserConfigurables.AllowedEvaluationResultTypes.String).Item;
                }
                else
                {
                    //This shouldn't be thrown at any cases as per current implementation
                    throw new NotImplementedException();
                }

                evaluatedResult = evaluatedResultString;
                evaluationFailureMessage = string.Empty;

                return true;
            }
            else
            {
                string evaluationErrorMessage = (parsedOutput.Item1 as ExpParserConfigurables.ExpressionEvaluationResult.EvaluationFailure).Item.ToString();

                evaluatedResult = string.Empty;
                evaluationFailureMessage = evaluationErrorMessage;

                return false;
            }
        }

        public void Destroy()
        {
            ValueDestroyed?.Invoke(this, EventArgs.Empty);
        }

        private void onPropertyChanged([CallerMemberName]string propertyName = "")
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        public bool Equals(SimpleVar other)
        {
            return this == other;
        }

        public string EvaluatedValue
        {
            get => _evaluatedValue; set
            {
                if(_evaluatedValue != value)
                {
                    _evaluatedValue = value;
                    onPropertyChanged();
                }
            }
        }

        public DateTime LastUpdatedDateTime
        {
            get => _lastUpdatedDateTime; private set
            {
                _lastUpdatedDateTime = value;
                onPropertyChanged();
            }
        }
        public string Name
        {
            get => _name; set
            {
                if(_name != value)
                {
                    _name = value;
                    onPropertyChanged();
                    NameChanged?.Invoke(this, EventArgs.Empty);
                }
            }
        }

        public bool IsErrorHighlighted
        {
            get { return _isErrorHighlighted; }
            set
            {
                if(_isErrorHighlighted != value)
                {
                    _isErrorHighlighted = value;
                    onPropertyChanged();
                }
                
            }
        }

        public string EvaluationFailureMessage
        {
            get { return _evaluationFailureMessage; }
            set
            {
                if(_evaluationFailureMessage != value)
                {
                    _evaluationFailureMessage = value;
                    onPropertyChanged();
                }
                
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;

        public event EventHandler<EventArgs> ValueUpdated;
        public event EventHandler<EventArgs> ValueDestroyed;
        public event EventHandler<EventArgs> NameChanged;

        public SimpleVar(string name, string value, System.Collections.ObjectModel.ObservableCollection<SimpleVar> variables)
        {
            _centralizedVariablesCollection = variables;
            Name = name;
            StrValue = value;
        }
    }
}
