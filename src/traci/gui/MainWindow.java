package traci.gui;

import javax.swing.JFrame;

@SuppressWarnings("serial")
public class MainWindow extends JFrame
{
    public MainWindow(final DynamicJPanelDrawArea drawArea)
    {
        setSize(drawArea.width(), drawArea.height());
        setLocation(200, 200);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        getContentPane().add(drawArea);
    }
}
