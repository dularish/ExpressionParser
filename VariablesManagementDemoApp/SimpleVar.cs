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
    public class SimpleVar : INotifyPropertyChanged
    {
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

            if (isDependenciesToBeUpdated)
            {
                clearDependencyVariables();
                addDependencyVariables(dependencyVariables);
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
            ValueUpdated?.Invoke(this, EventArgs.Empty);
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

        private bool evaluate(string value, out string evaluatedResult, out string evaluationFailureMessage, out List<SimpleVar> dependencyVariables)
        {
            Dictionary<string, string> dictForEvaluation = _centralizedVariablesCollection.ToDictionary(s => s.Name, s => s.StrValue);
            //var parsedOutput = MathematicalExpressionParser.parseAndEvaluateExpression(value, dictForEvaluation, this.Name);
            
            var parsedOutput = RefractoredImpl.parseAndEvaluateExpressionExpressively(value, dictForEvaluation, this.Name);

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
                string evaluatedResultString = (parsedOutput.Item1 as MathematicalExpressionParser.ExpressionEvaluationResult.EvaluationSuccess).Item.ToString();

                evaluatedResult = evaluatedResultString;
                evaluationFailureMessage = string.Empty;

                return true;
            }
            else
            {
                string evaluationErrorMessage = (parsedOutput.Item1 as MathematicalExpressionParser.ExpressionEvaluationResult.EvaluationFailure).Item.ToString();

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
