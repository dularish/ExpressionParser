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

        public string StrValue
        {
            get
            {
                return _strValue;
            }

            set
            {
                if(_strValue != value)
                {
                    _strValue = value;
                    onPropertyChanged();
                    EvaluatedValue = evaluate(value);
                    LastUpdatedDateTime = DateTime.Now;
                    ValueUpdated?.Invoke(this, EventArgs.Empty);
                }
            }
        }

        private ObservableCollection<SimpleVar> _centralizedVariablesCollection;

        private string evaluate(string value)
        {
            Dictionary<string, string> dictForEvaluation = _centralizedVariablesCollection.Where(s => s != this).ToDictionary(s => s.Name, s => s.StrValue);
            var parsedOutput = MathematicalExpressionParser.parseAndEvaluateExpression(value, dictForEvaluation);

            if (parsedOutput.Item1.IsEvaluationSuccess)
            {
                return (parsedOutput.Item1 as MathematicalExpressionParser.ExpressionEvaluationResult.EvaluationSuccess).Item.ToString();
            }
            else
            {
                return value;
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
