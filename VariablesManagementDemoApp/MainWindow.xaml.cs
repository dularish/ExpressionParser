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
    }
}
