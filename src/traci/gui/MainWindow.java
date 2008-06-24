package traci.gui;

import javax.swing.JFrame;

public class MainWindow extends JFrame
{
    private static final long serialVersionUID = 2288188481023569617L;
    
    public MainWindow(final DynamicJPanelDrawArea drawArea)
    {
        setSize(drawArea.width(), drawArea.height());
        setLocation(200, 200);
        
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        getContentPane().add(drawArea);
    }
}
