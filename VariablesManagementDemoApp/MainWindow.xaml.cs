using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace VariablesManagementDemoApp
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public VariablesMgmtWindowViewModel viewModel = new VariablesMgmtWindowViewModel();
        public MainWindow()
        {
            InitializeComponent();
            this.DataContext = viewModel;
        }

        private void _addVariable_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                viewModel.AddVariable(_variableNameTextBox.Text, _variableValueTextBox.Text);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error occurred! Try again", MessageBoxButton.OK, MessageBoxImage.Error);
            }

            _variableNameTextBox.Clear();
            _variableValueTextBox.Clear();
        }

        private void _testTopoSort_Click(object sender, RoutedEventArgs e)
        {
            Graph<char> variablesGraph = new Graph<char>();
            variablesGraph.AddEdge('A', 'D');
            variablesGraph.AddEdge('B', 'D');
            variablesGraph.AddEdge('B', 'E');
            variablesGraph.AddEdge('C', 'E');
            variablesGraph.AddEdge('C', 'F');
            variablesGraph.AddEdge('D', 'I');
            variablesGraph.AddEdge('E', 'G');
            variablesGraph.AddEdge('F', 'H');
            variablesGraph.AddEdge('D', 'I');
            variablesGraph.AddEdge('G', 'I');
            variablesGraph.AddEdge('H', 'I');

            StringBuilder sbRes = new StringBuilder();
            Stack<char> topoSortRes = variablesGraph.TopologicalSort('B');

            while(topoSortRes.Count > 0)
            {
                sbRes.Append(topoSortRes.Pop().ToString() + " - > ");
            }

            MessageBox.Show(sbRes.ToString());
        }
    }
}
