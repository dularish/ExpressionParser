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
            //SampleDataForTopoSort();
            SampleDataForBearingRollerReproduction();
        }

        private void SampleDataForBearingRollerReproduction()
        {
            Variables.Add(new SimpleVar("beam_damp", "0.01/(Math.PI*3320)", variables));
            Variables.Add(new SimpleVar("nRollers", "16", variables));
            Variables.Add(new SimpleVar("nSlices", "5", variables));
            Variables.Add(new SimpleVar("Bearing 1 nSlices", "nSlices", variables));
            Variables.Add(new SimpleVar("r_roller", "38/2.0*1e-3", variables));
            Variables.Add(new SimpleVar("r_ir", "215/2.0*1e-3", variables));
            Variables.Add(new SimpleVar("ir_width", "62.05*1e-3", variables));
            Variables.Add(new SimpleVar("r_ir_rim", "220.55/2.0*1e-3", variables));
            Variables.Add(new SimpleVar("ir_rim_length", "0.036*1e-3", variables));
            Variables.Add(new SimpleVar("clearance", "460*1e-6", variables));
            Variables.Add(new SimpleVar("r_or", "291/2.0*1e-3+clearance/2", variables));
            Variables.Add(new SimpleVar("or_width", "70*1e-3", variables));
            Variables.Add(new SimpleVar("l_roller", "62*1e-3", variables));
            Variables.Add(new SimpleVar("dM", "(r_ir+r_roller)*2", variables));
            Variables.Add(new SimpleVar("pocket_clearance", "150*1e-6", variables));
            Variables.Add(new SimpleVar("pocket_side_clearance", "1e-3", variables));
            Variables.Add(new SimpleVar("r_cg_pocket", "r_roller+pocket_clearance", variables));
            Variables.Add(new SimpleVar("cg_length", "62.5*1e-3", variables));
            Variables.Add(new SimpleVar("_R1", "r_roller", variables));
            Variables.Add(new SimpleVar("_R2", "r_roller", variables));
            Variables.Add(new SimpleVar("_density", "7850.0", variables));
            Variables.Add(new SimpleVar("L", "l_roller", variables));
            Variables.Add(new SimpleVar("A", "(_R1*_R1  + _R1*_R2 + _R2*_R2) / 3.0", variables));
            Variables.Add(new SimpleVar("M", "Math.PI*_density*L*A", variables));
            Variables.Add(new SimpleVar("LG", "L*(_R1*_R1+2.0*_R1*_R2+3.0*_R2*_R2 ) / (12.0 * A )", variables));
            Variables.Add(new SimpleVar("re_Jzz", "Math.PI*_density*L*(Math.Pow(_R1,4.0)+Math.Pow(_R1,3.0)*_R2+Math.Pow(_R1,2.0)*Math.Pow(_R2,2.0)+_R1*Math.Pow(_R2,3.0)+Math.Pow(_R2,4.0))/10.0", variables));
            Variables.Add(new SimpleVar("re_Jxx", "Math.PI*_density*Math.Pow(L,3.0)*(Math.Pow(_R1,2.0)+3.0*_R1*_R2+6.0*Math.Pow(_R2,2.0))/30.0+re_Jzz/2.0-M*LG*LG", variables));
            Variables.Add(new SimpleVar("re_Jyy", "re_Jxx", variables));
            Variables.Add(new SimpleVar("re_mass", "M", variables));
            Variables.Add(new SimpleVar("nk_or_YStr", "1e7/nSlices", variables));
            Variables.Add(new SimpleVar("nk_ir_YStr", "-1e7/nSlices", variables));
            Variables.Add(new SimpleVar("nk_cage_YStr", "-1e7/nSlices", variables));
            Variables.Add(new SimpleVar("nk_cage_rim_YStr", "-1e7/nSlices", variables));
            Variables.Add(new SimpleVar("BearingCage_RefDia", "dM*1000", variables));
            Variables.Add(new SimpleVar("BearingRow_RefDia", "dM*1000", variables));
            for (int i = 1; i < 17; i++)
            {
                Variables.Add(new SimpleVar("Roller " + i.ToString() + "_mass", "re_mass", variables));
                Variables.Add(new SimpleVar("Roller " + i.ToString() + "_jxx", "re_Jxx", variables));
                Variables.Add(new SimpleVar("Roller " + i.ToString() + "_jyy", "re_Jyy", variables));
                Variables.Add(new SimpleVar("Roller " + i.ToString() + "_jzz", "re_Jzz", variables));
            }
            Variables.Add(new SimpleVar("samExportJob_DampingInBeams", "beam_damp", variables));

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
