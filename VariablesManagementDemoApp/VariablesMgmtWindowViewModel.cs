using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VariablesManagementDemoApp
{
    public class VariablesMgmtWindowViewModel
    {
        private ObservableCollection<SimpleVar> variables = new ObservableCollection<SimpleVar>();

        public ObservableCollection<SimpleVar> Variables { get => variables; set => variables = value; }

        public VariablesMgmtWindowViewModel()
        {
            Variables.CollectionChanged += Variables_CollectionChanged;
            SampleDataForTopoSort();
        }

        private void SampleDataForTopoSort()
        {
            Variables.Add(new SimpleVar("a", "2", variables));
            Variables.Add(new SimpleVar("b", "4", variables));
            Variables.Add(new SimpleVar("c", "4", variables));
            Variables.Add(new SimpleVar("d", "a + b", variables));
            Variables.Add(new SimpleVar("e", "b + c", variables));
            Variables.Add(new SimpleVar("f", "c * 2", variables));
            Variables.Add(new SimpleVar("g", "e + 2", variables));
            Variables.Add(new SimpleVar("h", "f + 3", variables));
            Variables.Add(new SimpleVar("i", "d + g + h", variables));
        }

        private void Variables_CollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            if(e.OldItems != null)
            {
                foreach (var item in e.OldItems)
                {
                    (item as SimpleVar).Destroy();
                }
            }
        }

        public void AddVariable(string name, string value)
        {
            if(name.Contains(" "))
            {
                throw new Exception("Variable name cannot contain spaces");
            }
            else if (string.IsNullOrEmpty(name))
            {
                throw new Exception("Cannot add an empty variable");
            }
            else if(Variables.Any(s => s.Name == name))
            {
                throw new Exception("Already a variable exists with the name");
            }
            else
            {
                Variables.Add(new SimpleVar(name, value, variables));
            }
        }

        public void DeleteVariable(string name)
        {
            SimpleVar varToDelete = Variables.Where(s => s.Name == name).FirstOrDefault();
            if(varToDelete != null)
            {
                Variables.Remove(varToDelete);
            }
        }
    }
}
